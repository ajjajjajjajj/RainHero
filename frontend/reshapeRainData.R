library(jsonlite);
library(reshape2);
library(tidyverse);

reshape_rain_data <- function(data) {
  NUM_COLS <- dim(data)[2];
  df <- data.frame(index=data$timestamp)
  
  for (i in seq(2, NUM_COLS, 2)) {
    
    # select col index of each station
    col_index <- c(i,i+1)
    curr_station_data <- data[,..col_index]
    
    # get station_id from col and assign
    station_id <- curr_station_data[[1,1]]
    colnames(curr_station_data)[2] <- station_id
    
    # bind to result
    df <- df %>% cbind(curr_station_data[,2])
  }
  return(df)
}