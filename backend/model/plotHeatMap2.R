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
require(grid)
require(gridExtra)

source("./forecastData.R")
#source("./krigingGrid.R")
#source("./krigingUser.R")
source("./LRVariogram2.R")

forecast_data <- get_forecast_data()
#grid <- forecast_grid()
output <- LR_variogram()
grid <- output$grid
result <- output$result

###### plot prediction value as heatmap over singapore (STATIONS) ######
#----------------------------------------------------------------------#
#* @get /heatmap_stations
#* @serializer contentType="image/png"
plotHeatMapStations <- function() {
  sg_poly <- readRDS("./resources/sg_poly_sf.rds")
  stations <- st_as_sf(stations , coords=c("location.longitude", "location.latitude"))
  st_crs(stations) <- 4326
  
  grid_sf <- st_as_sf(grid, coords = c("location.longitude", "location.latitude"), crs = 4326)
  forecast_data <- as.data.frame(forecast_data)
  
  p <- ggplot(sg_poly) +
    geom_sf() +
    geom_point(data = forecast_data, 
               mapping = aes(x = location.longitude, y = location.latitude, color = category, size = 3),
               alpha = 0.6) +
    labs(x = "Longitude", y = "Latitude", title = "Heatmap") +
    guides(size = 'none') +
    labs(color = "Predicted Rainfall (mm)")
  
  # Save the plot to a temporary file
  tmp_file <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_file), add = TRUE)
  png(tmp_file, width = 800, height = 600)
  print(p)
  dev.off()
  
  # Read the temporary file and return its content
  readBin(tmp_file, "raw", n = file.info(tmp_file)$size)
}

###### plot prediction value as heatmap over singapore (GRIDPOINTS) ######
#------------------------------------------------------------------------#
#* @get /heatmap_singapore
#* @serializer contentType="image/png"
plotHeatMapSingapore <- function() {
  sg_poly <- readRDS("./resources/sg_poly_sf.rds")
  stations <- st_as_sf(stations , coords=c("location.longitude", "location.latitude"))
  st_crs(stations) <- 4326
  
  grid_sf <- st_as_sf(grid, coords = c("location.longitude", "location.latitude"), crs = 4326)
  grid <- as.data.frame(grid)
  
  p <- ggplot(sg_poly) +
    geom_sf() +
    geom_point(data = grid, 
               mapping = aes(x = location.longitude, y = location.latitude, color = predicted_rainfall, size = 0.001),
               alpha = 0.15) +
    labs(x = "Longitude", y = "Latitude", title = "Heatmap") +
    guides(size = FALSE) +
    labs(color = "Predicted Rainfall (mm)") +
    scale_color_gradient2(low = "lightgrey", mid = "lightblue", high = "darkblue")
  
  # Save the plot to a temporary file
  tmp_file <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_file), add = TRUE)
  png(tmp_file, width = 800, height = 600)
  print(p)
  dev.off()
  
  # Read the temporary file and return its content
  readBin(tmp_file, "raw", n = file.info(tmp_file)$size)
}
