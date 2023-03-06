#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(DT)

data("world.cities")

setwd("/Users/cynthia/Y3S2/DSA3101/Dataset")
location_ws <- read.csv("weather_stations.csv")
colnames(location_ws) <- c("id", "device id", "name", "latitude", "longitude")
location_ws

world.cities <- maps::world.cities
sg.cities <- world.cities %>%
  filter(country.etc == "Singapore")

my_map <- leaflet(sg.cities) %>%
  addTiles() %>%
  addCircles(radius = 500, weight = 1, color = "red", fillOpacity = 0.5, 
             lat = ~latitude, lng = ~longitude,
             data = location_ws, label = ~name) %>%
  addLabelOnlyMarkers(label = ~name, lat = ~latitude, lng = ~longitude, 
                      data = location_ws,
                      labelOptions = labelOptions(fontsize = "20px", fontcolor = "black"))
ui <- fluidPage(
  leafletOutput("mymap"),
  tableOutput("mytable")
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    my_map
  })
  
  output$mytable <- renderTable({
    location_ws %>%
      select('device id', name)
  })
}

shinyApp(ui, server)
