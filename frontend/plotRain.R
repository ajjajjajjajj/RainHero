library(maps)
library(leaflet)
library(tidyverse)
library(sf)

source("./frontend/reshapeRainData.R")
source("./backend/getRainfall.R")

register_google(key = "AIzaSyBKS8VK5IpWotg2RX9OPFU4zBAJxWdNMTo", write = TRUE)

stations <- read.csv("./backend/weather_stations.csv")
coords <- st_as_sf(stations, coords=c("location.longitude", "location.longitude"))

rainfall <- get_rainfall("2023-03-02", "2023-03-02") %>% reshape_rain_data()

world.cities <- maps::world.cities
sg.cities <- world.cities %>%
  filter(country.etc == "Singapore")


my_map <- leaflet(sg.cities) %>%
  addTiles() %>%
  addCircles(radius = 500, weight = 1, color = "red", fillOpacity = 0.5, 
             lat = ~location.latitude, lng = ~location.longitude,
             data = stations, label = ~name) %>%
  addLabelOnlyMarkers(label = ~name, lat = ~location.latitude, lng = ~location.longitude, 
                      data = coords,
                      labelOptions = labelOptions(fontsize = "20px", fontcolor = "black"))
