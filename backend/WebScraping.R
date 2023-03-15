library(rvest)
library(magrittr)
library(dplyr)
library(fpp2)
library(stringr)
library(xts)

###### function to get rainfall data in a given range #######
#-----------------------------------------------------------#
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
  return(df)
}

##############################################################################################################
### PROBLEM STATEMENT: Queenstown area, every evening, exercise along alexandra canalpark. 30mins exercise ###
### starting at 6pm. if it rains anytime from 6 to 630 she wont go exercise for that day. ####################
### importance: dont get stuck in rain > predict rain but dont rain > not given the path of the storm. #######
##############################################################################################################

######## test with 1 week of data ########
#---------------------------------------#
test <- get_rainfall('2023-03-01', '2023-03-08') #pull one week of data
# test <- get_rainfall(start_date, end_date) # when using real user input
test <- test[seq(1, dim(test)[1], 12)] # reduce data to per hour


###### split timestamp column into date and time #######
#------------------------------------------------------#
test2 <- data.frame(test) # create a clone copy of the test data, wont affect original test data

test2 <- test2 %>% 
  mutate(date = sub("\\T.*", "", timestamp), .after = timestamp) %>%
  mutate(time = sub(".*\\T", "", timestamp), .after = date ) %>%
  mutate(time = sub("\\+.*", "", time)) %>%
  select(!timestamp)

######## Get user inputs and parameters for the model #########
#-------------------------------------------------------------#
region = readline(prompt="Which region are you in? (Eg. North) : ")
e_datetime = format(Sys.time(), "%Y-%m-%dT%23:%05:%00") # set time to all have format as per data
s_datetime = format(seq(Sys.time(), by = "-1 week", length.out = 2)[2], "%Y-%m-%dT%00:%05:%00")

###### filter based on curr_time & curr_date - region ######
#----------------------------------------------------------#
start_date = substr(s_datetime, 1, 10)
start_time = substr(s_datetime, 12, 19)
start = paste(start_date, start_time, sep = " ") # for user input, use this as start

end_date = substr(e_datetime, 1, 10)
end_time = substr(e_datetime, 12, 19)
end = paste(end_date, end_time, sep = " ") # for user input, use this as end

# FOR THIS TEST,
starttime <- test[1,1] # for this test only
endtime <- test[dim(test2)[1], 1]

start1 = substr(starttime, 1, 10)
start2 = substr(starttime, 12, 19)
start3 = paste(start1, start2, sep = " ")

end1 = substr(endtime, 1, 10)
end2 = substr(endtime, 12, 19)
end3 = paste(end1, end2, sep = " ")

###### seperate dataframe of weather station coords ######
#--------------------------------------------------------------#
stations <- read.csv("C://Users/Markwee/Desktop/DSA3101/Station_Records_3.csv") # change to relative path
# classify these stations into NSEW. Based on User input of NSEW, find the 2 weather stations closest to user and its long/lat coords
# filter the rainfall dataset for these weather stations (columns)
# use kriging to interpolate and find the predictions for the area between these two stations. 
# This prediction is fairly close to the users location

#EXAMPLE
target_ws = c("S104", "S105", "S109", "S77") # assume these are weather stations in the region eg. NORTH

####### Change the names of columns to weather station names ########
#-------------------------------------------------------------------#
change_col_names <- function(df) {
  new_names = c("date", "time")
  for (i in 3:dim(df)[2]) {
    if (i%%2 != 0){ # we only want to manipulate the odd number columns thats where the station codes are
      n = as.character(df[1,i])
      colnames(df)[i] = n
      colnames(df)[i+1] = paste(n, "_val")
    }
  }
  return(df)
}

test2 <- change_col_names(test2)

####### Filter out the relevant columns of weather stations ########
#------------------------------------------------------------------#
get_relevant_columns <- function(df, target_ws) {
  indices <- c(1, 2)
  for (i in 3:dim(df)[2]) {
    if (i%%2 != 0 & colnames(df)[i] %in% target_ws) {
      indices <- append(indices, i)
      indices <- append(indices, i+1)
    }
  }
  df <- df[indices]
  return(df)
}
test2 <- get_relevant_columns(test2, target_ws)
test2[is.na(test2)] <- 0 # change NA to 0s

####### aggregate the data in this range #######
#----------------------------------------------#
get_mean <- function(df) {
  df['average'] <- rep(0, dim(df)[1])
  for (i in 1:dim(df)[1]) {
    avg <- c()
    for (j in 3:dim(df)[2]) {
      if (j%%2 == 0) {
        val = df[i,j]
        avg <- append(avg, val)
      }
    }
    df$average[i] <- mean(avg)
  }
  return(df)
}

test2 <- get_mean(test2)

#x<- test[2,1]
#x <- str_extract(x, "(?<=T).*?(?=\\+)")
#test <- test %>% mutate(timestamp = str_extract(timestamp, "(?<=T).*?(?=\\+)")) 

#declare as time series data
timeindex <- seq(from = as.POSIXct(start3), to = as.POSIXct(end3), by = "hour")
timeindex

timeseries <- xts(test2$average, order.by = timeindex)

#firsthour <- 24*(as.Date("2023-03-04 00:00:00") - as.Date("2023-03-03 23:59:59"))
#test <- ts(test[,2], start = c(2023, firsthour), frequency = 24*365)
#plot(test)

# fit an arima model
fit_arima <- auto.arima(timeseries, d=1, D=1, stepwise=FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

# forecast with arima model
fc <- forecast.Arima(fit_arima, h = 100)
plot(fc$fitted, col="blue")
lines(fc$x, col="red")
pred <- predict(fit_arima, n.ahead = 24)
pred
