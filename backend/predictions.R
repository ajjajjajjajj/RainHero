install.packages("sp")
install.packages("gstat")
install.packages("forecast")
install.packages("xts")
install.packages("zoo")

library(zoo)
library(sp)
library(gstat)
library(forecast)
library(xts)
library(tidyverse)
library(forecast)
source("getRainfall.R")

stations <- read.csv("~/Documents/dsa3101/dsa3101-2220-10-rain/backend/weather_stations.csv")
stations1 <- as.data.frame(stations) %>% 
  select(device_id, location.latitude, location.longitude) %>%
  rename(station_id = device_id)
stations2 <- as.data.frame(stations) %>% 
  select(device_id, location.latitude, location.longitude) %>%
  rename(station_id = device_id)
test <- get_rainfall("2023-03-01", "2023-03-02")
test$timestamp <- as.POSIXct(paste(test$date, test$time), format = "%Y-%m-%d %H:%M:%S")
data_long <- test %>% 
  gather(key = "station_id", value = "rainfall_level", -date, -time, -timestamp) 

data_long$rainfall_level <- sapply(data_long$rainfall_level, function(x) ifelse(is.null(x), NA, as.numeric(x)))

impute_interpolation <- function(x) {
  na.locf(na.approx(x, na.rm = FALSE), na.rm = FALSE)
}

data_imputed <- data_long %>% mutate(across(where(is.numeric), impute_interpolation))
data_imputed <- merge(data_imputed, stations1, by = "station_id")
station_ids <- unique(data_imputed$station_id)

# Create a list to store xts objects for each station
station_xts_list <- list()

for (station_id in station_ids) {
  station_data <- data_imputed[data_imputed$station_id == station_id, ]
  station_xts <- xts(station_data$rainfall_level, order.by = as.POSIXct(station_data$timestamp))
  station_xts_list[[station_id]] <- station_xts
}
# Forecast horizon (e.g., 1 for 30 minutes ahead)
forecast_horizon <- 1

# Create a list to store the ARIMA forecasts for each station
station_forecasts <- list()

for (station_id in station_ids) {
  station_xts <- station_xts_list[[as.character(station_id)]]
  arima_model <- auto.arima(station_xts)
  forecast_result <- forecast(arima_model, h = forecast_horizon)
  station_forecasts[[as.character(station_id)]] <- forecast_result$mean[1]
}

forecast_data <- data.frame(
  station_id = station_ids,
  forecasted_rainfall = as.numeric(station_forecasts)
)
stations2$station_id <- as.character(stations2$station_id)
forecast_data$station_id <- as.character(forecast_data$station_id)
forecast_data <- merge(forecast_data, stations2, by = "station_id", all.x=TRUE)
coordinates(forecast_data) <- ~location.longitude + location.latitude

# Create a variogram model
vgram <- variogram(forecasted_rainfall ~ 1, forecast_data)

# Fit a variogram model
vgram_model <- fit.variogram(vgram, model = vgm("Sph"))

# Perform Kriging interpolation for the user's location
user_longitude <- 103.75
user_latitude <- 1.32

user_location <- data.frame(longitude = user_longitude, latitude = user_latitude)
coordinates(user_location) <- ~longitude + latitude

user_kriging_result <- krige(forecasted_rainfall ~ 1, forecast_data, user_location, model = vgram_model)

predicted_rainfall <- user_kriging_result@data$var1.pred
predicted_rainfall
