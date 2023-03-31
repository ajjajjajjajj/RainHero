install.packages("sp")
install.packages("gstat")
install.packages("forecast")
install.packages("xts")
install.packages("zoo")
install.packages("rgdal")
install.packages("rgeos")

library(zoo)
library(sp)
library(sf)
library(gstat)
library(forecast)
library(xts)
library(tidyverse)
library(forecast)
library(classInt)
library(RColorBrewer)
library(rgdal)
library(rgeos)

###### get weather stations code, long, lat ######
#------------------------------------------------#
stations <- read.csv("./dsa3101-2220-10-rain/backend/weather_stations.csv")

# copy 1 of station data
stations1 <- as.data.frame(stations) %>% 
  select(device_id, location.latitude, location.longitude) %>%
  rename(station_id = device_id)

# copy 2 of station data
stations2 <- as.data.frame(stations) %>% 
  select(device_id, location.latitude, location.longitude) %>%
  rename(station_id = device_id)

###### load get_rainfall function from other R file ######
#--------------------------------------------------------#
source("./dsa3101-2220-10-rain/backend/getRainfall.R")
test <- get_rainfall('2023-03-01', '2023-03-02') # pull 2 days of data

###### add a timestamp to the rainfall data ######
#------------------------------------------------#
test$timestamp <- as.POSIXct(paste(test$date, test$time), format = "%Y-%m-%d %H:%M:%S")
test <- test %>% relocate(timestamp, .after = time)

###### prepare rainfall data ######
#---------------------------------#
data_long <- test %>% 
  gather(key = "station_id", value = "rainfall_level", -date, -time, -timestamp)

data_long <- data_long %>% 
  mutate(rainfall_level = sapply(data_long$rainfall_level, function(x) ifelse(is.null(x), NA, as.numeric(x)))) # replace NULL by NA

###### function to interpolate for rainfall levels NA values ######
#--------------------------------------------------------------------------------#
impute_interpolation <- function(x) {
  na.locf(na.approx(x, na.rm = FALSE), na.rm = FALSE)
}

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
forecast_horizon <- 1 # e.g., 1 for 30 minutes ahead

station_forecasts <- list()

for (station_id in station_ids) { # each list in station_forecasts is the forecasted rainfall data for that weather station
  station_xts <- station_xts_list[[as.character(station_id)]]
  arima_model <- auto.arima(station_xts)
  forecast_result <- forecast(arima_model, h = forecast_horizon)
  station_forecasts[[as.character(station_id)]] <- forecast_result$mean[1]
}

###### Extract the forecasted data for each weather station ######
#----------------------------------------------------------------#
forecast_data <- data.frame(
  station_id = station_ids,
  forecasted_rainfall = as.numeric(station_forecasts)
)

###### create spatial data from forecast_data ######
#--------------------------------------------------#
stations2$station_id <- as.character(stations2$station_id)
forecast_data$station_id <- as.character(forecast_data$station_id)
forecast_data <- merge(forecast_data, stations2, by = "station_id", all.x=TRUE)
coordinates(forecast_data) <- ~location.longitude + location.latitude

###### Fit a linear regression model using longitude and latitude as covariates ######
#------------------------------------------------------------------------------------#
lm_model <- lm(forecasted_rainfall ~ location.longitude + location.latitude, data = forecast_data)

###### Calculate the residuals of the linear regression model ######
#------------------------------------------------------------------#
forecast_data$residuals <- residuals(lm_model)

###### Create and fit a variogram model ######
#--------------------------------------------#
vgram_residuals <- variogram(residuals ~ 1, forecast_data)

vgram_model_residuals <- fit.variogram(vgram_residuals, model = vgm("Sph"))

###### Perform Kriging interpolation for the user's location ######
#-----------------------------------------------------------------#
# user_loc <- readline(prompt = "What is your location?: ") # use user_loc to get long and lat data from user input/GPS 
user_longitude <- 103.75
user_latitude <- 1.32

user_location <- data.frame(longitude = user_longitude, latitude = user_latitude)
coordinates(user_location) <- ~longitude + latitude

user_kriging_result <- krige(forecasted_rainfall ~ 1, forecast_data, user_location, model = vgram_model_residuals)

predicted_rainfall <- user_kriging_result@data$var1.pred
predicted_rainfall

###### Perform Kriging interpolation for the grid points ######
#-----------------------------------------------------------------#
max_v_point <- 1.470783
min_v_point <- 1.158762
max_h_point <- 104.0885
min_h_point <- 103.6057

grid_h_points <- seq(min_h_point, max_h_point, length.out=50)
grid_v_points <- seq(min_v_point, max_v_point, length.out=50)
grid <- expand.grid(longitude = grid_h_points, latitude = grid_v_points)

# Rename the columns in the grid data frame to match those in the forecast_data data frame
colnames(grid)[1:2] <- c("location.longitude", "location.latitude")

###### Convert the grid into a spatial data frame ######
#------------------------------------------------------#

coordinates(grid) <- ~location.longitude + location.latitude

###### Perform Kriging interpolation for each grid point ######
#-------------------------------------------------------------#
grid_kriging_result <- krige(residuals ~ 1, forecast_data, grid, model = vgram_model_residuals)


###### Add the kriging predictions of the residuals to the linear regression predictions ######
#--------------------------------------------------------------------------------------------#
grid$regression_prediction <- predict(lm_model, newdata = grid)
grid$predicted_rainfall_residuals <- grid_kriging_result@data$var1.pred
grid$predicted_rainfall <- grid$regression_prediction + grid$predicted_rainfall_residuals

grid <- as.data.frame(grid)

###### plot prediction value as heatmap over singapore ######
#-----------------------------------------------------------#
sg_poly <- readRDS("./dsa3101-2220-10-rain/backend/sg_poly_sf.rds")
stations <- st_as_sf(stations , coords=c("location.longitude", "location.latitude"))
st_crs(stations) <- 4326

grid_sf <- st_as_sf(grid, coords = c("Var2", "Var1"), crs = 4326)
forecast_data <- as.data.frame(forecast_data)
ggplot(sg_poly) +
  geom_sf() +
  geom_density_2d_filled(data = forecast_data, 
                         mapping = aes(x = location.longitude, y = location.latitude),
                         #contour_var = 'density',
                         alpha = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  theme_classic()

###### comparing predicted vs actual rainfall data ######
#-------------------------------------------------------#
# from above, data was from 2023-03-01 to 2023-03-02 so predicted is from 2023-03-03 to 2023-03-04
pred_vals <- forecast_result$fitted
actual <- get_rainfall("2023-03-03", "2023-03-04")
actual <- actual %>%
  select(date, time, S94) %>%
  mutate(predicted_vals = pred_vals) %>%
  mutate(ID = seq(1, dim(actual)[1], 1))

plot(x = c(actual$ID, actual$ID), y = c(actual$S94, actual$predicted_vals), col = c('blue', 'red'), pch = 20)

