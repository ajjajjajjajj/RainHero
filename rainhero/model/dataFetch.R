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

install.packages("odbc")
install.packages("dplyr")

library(odbc)
library(dplyr)

fetch_data_from_db <- function() {
  # code to connect to database and fetch data

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "SQLEXPRESS02",
                 Database = "rainfall",
                 UID = "group10",
                 PWD = rstudioapi::askForPassword("password"),
                 Port = 1433)

# For raw rainfall readings
q1 <- tbl(con, "raw_data")

show_query(q1)

# For user-specified prep data for prediction model
q2 <- tbl(con, "prep_data") %>%
  filter( timestamp > start) %>%
  filter( timestamp < end) %>%
  arrange(station_id, date, time)

show_query(q2)
  
  data <- read.table("./resources/placeholder_data.csv") # placeholder for the fetched data
  return(data)
}
