library(sf)
library(ggplot2)
library(dplyr)

# Set working directory to where you have the weather_stations.csv file

stations <- read.csv("weather_stations.csv")
sg_poly <- readRDS("sg_poly_sf.rds")
stations <- st_as_sf(stations , coords=c("location.longitude", "location.latitude"))
st_crs(stations) <- 4326
ggplot(sg_poly) +
  geom_sf() + geom_sf(data=stations, col="red", pch=19, size=1) + 
  labs(x="X", y="Y", title="Weather Station Locations in Singapore")

