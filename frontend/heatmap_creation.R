## HeatMap created with Kriging 

library(sp)
library(gstat)

library(dplyr) # for "glimpse"
library(ggplot2)
library(scales) # for "comma"
library(magrittr)

library(rgdal)
library(raster)

data <- read.csv('C:/Users/Charlotte Liau/dsa3101-2220-10-rain/frontend/heatmap_data.csv')
data$X2023.01.22 = as.numeric(data$X2023.01.22)
data[is.na(data)] <- 0
coordinates(data)<- ~latitude + longtitude
data %>% as.data.frame %>% glimpse

# fitting a variogram
lzn.vgm <- variogram(X2023.01.22~1, data)
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900, 1))

# creating the grid 
grid_data <- read.csv('C:/Users/Charlotte Liau/dsa3101-2220-10-rain/frontend/heatmap_data.csv')
station <- data.frame(lat = grid_data$latitude, long = grid_data$longtitude, station = 1:64 )
coordinates(station) <- ~long+ lat
proj4string(station) <-CRS("+init=epsg:4326")
mapview(station)

ori <- SpatialPoints(cbind(100, 1), proj4string =  CRS("+init=epsg:4326"))
ori_t <- spTransform(ori, CRSobj = CRS("+init=epsg:3857"))
ori_t <- spTransform(ori, CRSobj=CRS("+proj=moll +ellps=WGS84"))
