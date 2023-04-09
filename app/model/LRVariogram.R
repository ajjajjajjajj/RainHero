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
  return(vgram_model_residuals)
}