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

# user_longitude and user_latitude given by the telegram bot
#* @get /forecast_user_location
forecast_user_location <- function(user_longitude, user_latitude) {
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
  
  return(result)
}
