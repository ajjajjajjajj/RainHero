library(magrittr)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)
library(ggplot2)


# For backend visualisation purposes
stations <- read.csv("weather_stations.csv")
sg_poly <- readRDS("sg_poly_sf.rds")
stations <- st_as_sf(stations , coords=c("location.longitude", "location.latitude"))
st_crs(stations) <- 4326


###################################
# Northernmost point of Singapore #
###################################

max_v_point <- 1.470783

###################################
# Southernmost point of Singapore #
###################################

min_v_point <- 1.158762

##################################
# Easternmost point of Singapore #
##################################

max_h_point <- 104.0885

##################################
# Westernmost point of Singapore #
##################################

min_h_point <- 103.6057

#########################
# Getting a grid system #
#########################

grid_h_points <- seq(min_h_point, max_h_point, length.out=50)
grid_v_points <- seq(min_v_point, max_v_point, length.out=50)
grid <- expand.grid(grid_v_points, grid_h_points)
grid_sf <- st_as_sf(grid, coords = c("Var2", "Var1"), crs = 4326)

# Plot the grid as points
ggplot(sg_poly) +
  geom_sf() + geom_sf(data=stations, col="red", pch=19, size=0.5) + 
  geom_sf(data=grid_sf, size = 0.5, shape = ".", color = "blue")+
  labs(x="Longitude", y="Latitude", title="Weather Station Locations in Singapore")

