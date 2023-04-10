require(zoo)
require(sp)
require(sf)
require(gstat)
require(forecast)
require(xts)
require(tidyverse)
require(forecast)
require(classInt)
require(RColorBrewer)
require(rgdal)
require(rgeos)

source("./dataFetch.R")
source("./loadingWeatherStations.R")

data_from_db <- fetch_data_from_db()
stations1 <- loadStation1()

###### function to interpolate for rainfall levels NA values ######
#--------------------------------------------------------------------------------#
impute_interpolation <- function(x) {
  na.locf(na.approx(x, na.rm = FALSE), na.rm = FALSE)
}

###### prepare rainfall data ######
#---------------------------------#
data_prep_arima <- function() { # function returns forecasted data
  data_from_db$timestamp <- as.POSIXct(paste(data_from_db$date, data_from_db$time), format = "%Y-%m-%d %H:%M:%S")
  data_from_db <- data_from_db %>% relocate(timestamp, .after = time)
  
  data_long <- data_from_db %>% 
    gather(key = "station_id", value = "rainfall_level", -date, -time, -timestamp)
  
  data_long <- data_long %>% 
    mutate(rainfall_level = sapply(data_long$rainfall_level, function(x) ifelse(is.null(x), NA, as.numeric(x)))) # replace NULL by NA
  
  data_imputed <- data_long %>% mutate(across(where(is.numeric), impute_interpolation))
  data_imputed <- merge(data_imputed, stations1, by = "station_id")
  station_ids <- unique(data_imputed$station_id)
  ###### List of lists to store xts objects for each station ######
  #------------------------------------------------------#
  station_xts_list <- list()
  for (station_id in station_ids) { # each list in station_xts_list is the all the rainfall data for that weather station
    station_data <- data_imputed[data_imputed$station_id == station_id, ]
    station_xts <- xts(station_data$rainfall_level, order.by = as.POSIXct(station_data$timestamp))
    station_xts_list[[station_id]] <- station_xts
  }
  ###### List of lists to store the ARIMA forecasts for each station ######
  #-----------------------------------------------------------------------#
  forecast_horizon <- 96 # e.g., 1 for 30 minutes ahead
  
  station_forecasts <- list()
  
  current_time <- Sys.time() # get current time
  current_min <- format(current_time, "%M")
  current_hour <- format(current_time, "%H")
  
  if (as.numeric(current_min) >= 30) {                       #Eg. 2135Hrs --> predict for 10pm
    index <- as.numeric(current_hour) * 2 + 2
  } else {
    index <- as.numeric(current_hour) * 2 + 1         #Eg. 2125Hrs --> predict for 930pm
  }
  
  for (station_id in station_ids) { # each list in station_forecasts is the forecasted rainfall data for that weather station
    station_xts <- station_xts_list[[as.character(station_id)]]
    arima_model <- auto.arima(station_xts)
    forecast_result <- forecast(arima_model, h = forecast_horizon)
    
    if (index > 1) { # get the predicted rainfall 30mins before time of interest Eg. 18*2 + 1 - 1 = 6pm
      pred_at_req_time_2 <- forecast_result$fitted[index-1]
    } else {
      pred_at_req_time_2 <- forecast_result$fitted[index]
    }
    
    pred_at_req_time <- forecast_result$fitted[index]     # rainfall at time of interest (30mins in the future). Eg. 18*2 + 1 = 630pm
    mm_per_min <- (pred_at_req_time - pred_at_req_time_2) / 30        # intervals are in 30mins
    station_forecasts[[as.character(station_id)]]$pred <- pred_at_req_time # Get the predicted rainfall for 30mins from current time
    station_forecasts[[as.character(station_id)]]$mmPerMin <- mm_per_min
  }
  return(station_forecasts)
}



