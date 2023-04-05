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

source("./forecastData.R")
source("./LRVariogram.R")

forecast_data <- get_forecast_data()
vgram_model_residuals <- LR_variogram()

###### Perform Kriging interpolation for the grid points ######
#-----------------------------------------------------------------#
forecast_grid <- function() { 
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
  return(grid)
}