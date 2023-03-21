## HeatMap created with Kriging 

library(sp)
library(gstat)
library(leaflet)
library(mapview)
library(dplyr) # for "glimpse"
library(ggplot2)
library(scales) # for "comma"
library(magrittr)

library(rgdal)
library(raster)

data <- read.csv("C:/Users/Charlotte Liau/dsa3101-2220-10-rain/frontend/heatmap_data.csv")
#View(df)
data$X2023.01.22 = as.numeric(data$X2023.01.22)
data[is.na(data)] <- 0
coordinates(data)<- ~latitude + longtitude
data %>% as.data.frame %>% glimpse

# fitting a variogram
lzn.vgm <- variogram(X2023.01.22~1, data)
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900, 1))

# creating the grid 
grid_data <- read.csv("C:/Users/Charlotte Liau/dsa3101-2220-10-rain/frontend/heatmap_data.csv")
station <- data.frame(lat = grid_data$latitude, long = grid_data$longtitude, station = 1:64 )
coordinates(station) <- ~long+ lat
proj4string(station) <-CRS("+init=epsg:4326")
mapview(station)

ori <- SpatialPoints(cbind(115, 1), proj4string =  CRS("+init=epsg:4326"))
ori_t <- spTransform(ori, CRSobj = CRS("+init=epsg:3857"))
ori_t <- spTransform(ori, CRSobj=CRS("+proj=moll +ellps=WGS84"))

coordinates(ori_t)

# determine the extent of grid
# The origin has been rounded to the nearest 100
x_ori <- round(coordinates(ori_t)[1, 1]/100) * 100
y_ori <- round(coordinates(ori_t)[1, 2]/100) * 100

# Define how many cells for x and y axis
x_cell <- 60
y_cell <- 60

# Define the resolution to be 1000 meters
cell_size <- 1000

# Create the extent
ext <- extent(x_ori, x_ori + (x_cell * cell_size), y_ori, y_ori + (y_cell * cell_size)) 

# Initialize a raster layer
ras <- raster(ext)

# Set the resolution to be
res(ras) <- c(cell_size, cell_size)
ras[] <- 0

# Project the raster
projection(ras) <- CRS("+init=epsg:3857")

# Create interactive map
mapview(station) + mapview(ras)

# Save the raster layer
writeRaster(ras, filename = "ras.tif", format="GTiff", overwrite=TRUE) 

# Convert to spatial pixel
st_grid <- rasterToPoints(ras, spatial = TRUE)
gridded(st_grid) <- TRUE
st_grid <- as(st_grid, "SpatialPixels")

# Kriging starts here 
plot1 <- data %>% as.data.frame %>%
  ggplot(aes(latitude, longtitude)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# this is clearly gridded over the region of interest
plot2 <- st_grid %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

