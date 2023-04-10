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

source("./dataPrep.R")

station_forecasts <- data_prep_arima()

get_forecast_data <- function() {
  forecast_data <- do.call(rbind, lapply(station_forecasts, function(x) data.frame(t(unlist(x)))))
  forecast_data <- rownames_to_column(forecast_data, var = "station_id")
  colnames(forecast_data)[2] <- 'forecasted_rainfall'
  
  forecast_data$mmPerMin[forecast_data$mmPerMin < 0] <- 0
  
  forecast_data$category <- ifelse(forecast_data$mmPerMin == 0, "No Rain", 
                                   ifelse(forecast_data$mmPerMin > 0 & forecast_data$mmPerMin < 0.04, "Light Rain", 
                                          ifelse(forecast_data$mmPerMin >= 0.04 & forecast_data$mmPerMin < 0.125, "Moderate Rain", 
                                                 ifelse(forecast_data$mmPerMin >= 0.125 & forecast_data$mmPerMin < 0.83, "Heavy Rain", "Violent Rain"))))
  return(forecast_data)
}
