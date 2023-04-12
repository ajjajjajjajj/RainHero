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

source("./loadingWeatherStations.R")
source("./forecastData.R")

stations2 <- loadStation2()
forecast_data <- get_forecast_data()

LR_variogram <- function() { # returns vgram_model_residuals for kriging function
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
  #return(vgram_model_residuals)
  
  
  
  #----------------------------------------------------------------------------------------#
  
  
  
  # FROM KrigingGrid.R
  # Note that if you want the heatmap, you have to convert the grid object back to dataframe
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
  
  grid$predicted_rainfall[grid$predicted_rainfall < 0] <- 0
  
  
  
  #----------------------------------------------------------------------------------------#
  
  
  
  # FROM krigingUser.R
  user_longitude <- as.numeric(user_longitude)
  user_latitude <- as.numeric(user_latitude)
  result <- tryCatch({
    user_location <- data.frame(location.longitude = user_longitude, location.latitude = user_latitude)
    coordinates(user_location) <- ~location.longitude + location.latitude
    
    user_kriging_result <- krige(forecasted_rainfall ~ 1, forecast_data, user_location, model = vgram_model_residuals)
    # Calculate the linear regression predictions for the user's location
    user_lm_prediction <- predict(lm_model, newdata = user_location)
    
    # Add the kriging predictions of the residuals to the linear regression predictions
    predicted_rainfall <- user_lm_prediction + user_kriging_result@data$var1.pred
    return(predicted_rainfall)
  }, error = function(e) {
    message("An error occurred: ", e$message)
    return(NULL)
  })
  
  
  #----------------------------------------------------------------------------------------#
  
  return(list(grid = grid, result = result))
}

output <- LR_variogram()
grid <- output$grid
result <- output$result
