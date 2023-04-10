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


stations <- read.csv("./resources/weather_stations.csv")
loadStation1 <- function() {
  # copy 1 of station data
  stations1 <- as.data.frame(stations) %>% 
    select(device_id, location.latitude, location.longitude) %>%
    rename(station_id = device_id)
  return(stations1)
}

loadStation2 <- function() {
  # copy 2 of station data
  stations2 <- as.data.frame(stations) %>% 
    select(device_id, location.latitude, location.longitude) %>%
    rename(station_id = device_id)
  return(stations2)
}
