library(magrittr)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)

# Inspiration from https://github.com/andrew-loh/neaSG/blob/master/R/get_rainfall.R
get_rainfall <- function(from, to) {
  requireNamespace("magrittr")
  #Checking inputs
  if(class(from) != "character") stop("Parameter 'from' must be character string")
  if(class(to) != "character") stop("Parameter 'to' must be character string")
  tryCatch({test <- as.Date(from); print(test)},
           error = function(e) {stop("Input all dates in 'YYYY-MM-DD' format")})
  tryCatch({test <- as.Date(to); print(test)},
           error = function(e) {stop("Input all dates in 'YYYY-MM-DD' format")})
  if(as.Date(from) > as.Date(to)) stop("Date put into parameter 'from' is later than date put into parameter 'to'")
  
  #Getting list of individual dates
  from1 <- as.Date(from)
  to1 <- as.Date(to)
  date_list <- seq(from1, to1, by = 1)
  
  #Endpoint for API
  URL <- "https://api.data.gov.sg/v1/environment/rainfall"
  
  #Define function which can then be mapped over list of dates
  fn_result <- function(date) {
    query_params <- list(
      "date" = date
    )
    get_result <- httr::GET(url = URL, query = query_params)
    json <- httr::content(get_result)
    return(json[[2]] %>% purrr::map(as.data.frame) %>% data.table::rbindlist(fill = T))
  }
  
  #Map function over list of dates to get list of daily dataframes
  df_list <- purrr::map(date_list, fn_result)
  
  #Binds list of daily dataframes into one
  df <- data.table::rbindlist(df_list, fill = T)
  # Added this portion to get every 30minutes interval
  df.new <- df[seq(1, nrow(df), 6), ]
  # Portion to remanipulate dataframe into something that's more readable
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("readings."),
      names_to = c("Reading", ".value"),
      names_sep = "\\."
    ) %>%
    select(-Reading)
  
  df_pivoted <- pivot_wider(df_long, id_cols = station_id, names_from = timestamp, values_from = value)
  df_pivoted <- as.data.frame(df_pivoted)
  df_pivoted <- na.omit(df_pivoted)
  rownames(df_pivoted) <- df_pivoted$station_id
  df_pivoted <- df_pivoted[,-1]
  return(df_pivoted)
}

df <- get_rainfall("2023-01-22", "2023-01-23")
View(df)

weather_station <- read.csv("/Users/cynthia/Y3S2/DSA3101/dsa3101-2220-10-rain/weather_stations.csv", header = TRUE)
names(weather_station) <- c("id", "device id", "name", "latitude", "longtitude")

df_rain = df %>%
  select("2023-01-22T07:35:00+08:00") %>%
  rename("2023-01-22" = "2023-01-22T07:35:00+08:00")

row_names = row.names(df_rain)

df_rain = df_rain %>%
  mutate(id = row_names)

# generating heatmap_data
heatmap_data = merge(df_rain, weather_station, by = "id") %>%
  select(c('id', '2023-01-22', 'latitude', 'longtitude'))

heatmap_data = data.frame(lapply(heatmap_data, as.character), stringsAsFactors = FALSE)
write.csv(heatmap_data, "heatmap_data.csv", row.names = TRUE)

library(sp)
library(gstat)

suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})

