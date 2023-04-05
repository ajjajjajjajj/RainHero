require(xts)
require(tidyverse)
require(forecast)
require(classInt)
require(RColorBrewer)
require(rgdal)
require(rgeos)

source("./krigingGrid.R")
grid <- forecast_grid()


###### plot prediction value as heatmap over singapore (GRIDPOINTS) ######
#------------------------------------------------------------------------#
plotHeatMapSingapore <- function() {
  sg_poly <- readRDS("./resources/sg_poly_sf.rds")
  stations <- st_as_sf(stations , coords=c("location.longitude", "location.latitude"))
  st_crs(stations) <- 4326
  
  grid_sf <- st_as_sf(grid, coords = c("location.longitude", "location.latitude"), crs = 4326)
  grid <- as.data.frame(grid)
  ggplot(sg_poly) +
    geom_sf() +
    geom_point(data = grid, 
               #mapping = aes(x = location.longitude, y = location.latitude, color = predicted_rainfall, size = 0.001),
               mapping = aes(x = location.longitude, y = location.latitude, color = predicted_rainfall, size = 0.001),
               alpha = 0.15) +
    labs(x = "Longitude", y = "Latitude", title = "Heatmap") +
    guides(size = FALSE) +
    labs(color = "Predicted Rainfall (mm)") +
    scale_color_gradient2(low = "lightgrey", mid = "lightblue", high = "darkblue")
}