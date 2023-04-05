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
forecast_data <- get_forecast_data()

###### plot prediction value as heatmap over singapore (STATIONS) ######
#----------------------------------------------------------------------#
plotHeatMapStations <- function() {
  sg_poly <- readRDS("./resources/sg_poly_sf.rds")
  stations <- st_as_sf(stations , coords=c("location.longitude", "location.latitude"))
  st_crs(stations) <- 4326
  
  grid_sf <- st_as_sf(grid, coords = c("location.longitude", "location.latitude"), crs = 4326)
  forecast_data <- as.data.frame(forecast_data)
  
  ggplot(sg_poly) +
    geom_sf() +
    geom_point(data = forecast_data, 
               mapping = aes(x = location.longitude, y = location.latitude, color = category, size = 3),
               alpha = 0.6) +
    labs(x = "Longitude", y = "Latitude", title = "Heatmap") +
    guides(size = 'none') +
    labs(color = "Predicted Rainfall (mm)") 
}