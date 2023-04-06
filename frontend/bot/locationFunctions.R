require(telegram.bot)

source('botFunctions.R')
source('util.R')
source('searchLocation.R')


# ----- IMPORT SAMPLE FAV LOCS -----
# replace this part with connection to SQL database
df <- read.csv("./resources/test_fav.csv") 
locations <- as.vector(df$Amy)
FAV_LOCATIONS_AMY <- locations[which(locations!="")]



# ----- IMPORT mrt/lrt data -----
stations_df <- read.csv("./resources/mrt_lrt_data.csv")
AVAILABLE_LOCATIONS <- as.vector(stations_df$station_name)
nsew <- c("North", "South", "East", "West")

west <- stations_df[stations_df$Region == 'West', ]
west_stations <- as.vector(west$station_name)
north <- stations_df[stations_df$Region == 'North', ]
north_stations <- as.vector(north$station_name)
south <- stations_df[stations_df$Region == 'South', ]
south_stations <- as.vector(south$station_name)
east <- stations_df[stations_df$Region == 'East', ]
east_stations <- as.vector(east$station_name)


# returns a list containing one button, representing one row of one button 
# parses station data in the following format
# "CB_SHOW_LOCATION_PRED|LOCATION_NAME|LONGITUDE|LATITUDE"
make_location_button <- function(location, callback_id) {
  station_data <- stations_df[which(stations_df$station_name == location),]
  long <- station_data$lng
  lat <- station_data$lat
  data <- create_callback_string(callback_id, location, long, lat)
  return(list(InlineKeyboardButton(location, callback_data = data)))
}

# creates a custom IKM for user with their favorite locations
make_location_ikm <- function(locations) {
  return(sapply(locations, make_location_button, CB_SHOW_LOCATION_PRED, 
                USE.NAMES = F, simplify = F))
}

# creates IKM for each region
make_region_ikm <- function(regions) {
  return(sapply(regions, make_location_button, CB_REGIONS, 
                USE.NAMES = F, simplify = F))
}

# returns the long-lat of an available location
get_location_long_lat <- function(location) {
  if (!is_location_available(location)) {
    return(list())
  } 
  station_info <- stations_df[which(stations_df$station_name == location),]
  return(list(longitude=station_info$lng, latitude=station_info$lat))
}

# returns true if given location is available
is_location_available <- function(location) {
  return(location %in% AVAILABLE_LOCATIONS)
}

# returns true if given region is available
is_region_available <- function(region) {
  return(region %in% nsew)
}


# -----  ALL AVAILABLE LOCATIONS BUTTON TEMPLATE -----
IKM_GET_AVAILABLE_LOCATIONS <- make_location_ikm(AVAILABLE_LOCATIONS)
IKM_FAV_LOCATIONS_AMY <- make_location_ikm(FAV_LOCATIONS_AMY)

IKM_NSEW <- make_region_ikm(nsew)
IKM_WEST <- make_location_ikm(west_stations)
IKM_NORTH <- make_location_ikm(north_stations)
IKM_SOUTH <- make_location_ikm(south_stations)
IKM_EAST <- make_location_ikm(east_stations)

# ----- LOCATION FUNCTIONS ------

# replies user with list of locations from selected region
send_location <- function(bot, update) {
  region <- parse_callback_string(update$callback_query$data)$data[1]
  
  if (!is_region_available(region)) {
    error_msg <- "region is not available - no prediction is found"
    bot$send_message(update$effective_chat()$id, error_msg,
                     reply_markup = InlineKeyboardMarkup(
                       inline_keyboard = BUTTON_BACK_TO_HOME))
  } else {
    ikm_function <- eval(parse(text = paste0("IKM_", toupper(region))))
    reply_buttons <- append(ikm_function,
                            BUTTON_BACK_TO_HOME)
    bot$send_message(update$effective_chat()$id,
                     paste0("Choose a location from ", region),
                     reply_markup = InlineKeyboardMarkup(
                       inline_keyboard = reply_buttons))
  }
}
   

# replies the user with a prediction for an available location
# if the location is not available, sends an error message
# expects data in the following format:
# "CB_SHOW_LOCATION_PRED|LOCATION_NAME|LONGITUDE|LATITUDE"
send_location_prediction <- function(bot, update) {
  parsed_cb_data <- parse_callback_string(update$callback_query$data)
  location <- parsed_cb_data$data[1]
  long <- parsed_cb_data$data[2]
  lat <- parsed_cb_data$data[3]
  text <- paste("This is the output for",
                location, "long:", long, "lat:", lat)
  bot$send_message(update$effective_chat()$id,
                    text, 
                   reply_markup = InlineKeyboardMarkup(
                     inline_keyboard = BUTTON_BACK_TO_HOME))# to replace text with a template

}


# returns a list of buttons representing each search hit for the location search
# each button contains a callback string in the following format
# "CB_SHOW_LOCATION_PRED|LOCATION_NAME|LONGITUDE|LATITUDE"
# this should call the function mapped to the callback key model_get_prediction
# long and lat are truncated to 6 decimal places, which gives an precision 
# of about 11 cm, sufficient for our purposes
# truncation is necessary to keep to the 64 byte limit for callback_data
# long becomes 10 char long, lat becomes 8 char long

handle_location_input <- function(bot, update) {
  search_key <- str_remove(update$effective_message()$text, "^/location")
  search_result <- get_location_matches(search_key)
  reply_buttons <- list()
  nhits <- nrow(search_result)
  if (nhits > 0) {
    for (i in 1:nhits) {
      curr_row <- search_result[i,]
      
      location_address <- curr_row$SEARCHVAL[[1]]
      long <- as.numeric(curr_row$LONGITUDE[[1]])
      lat <- as.numeric(curr_row$LATITUDE[[1]])
      
      long_trunc <- format(round(long, 6), nsmall = 6)
      lat_trunc <- format(round(lat, 6), nsmall = 6)
      
      location_address_trunc <- location_address
      if (nchar(location_address) > 33) {
        location_address_trunc <- paste(str_sub(location_address, 1, 30),
                                        "...", sep= "")
      }
      
      callback_data <- create_callback_string(CB_SHOW_LOCATION_PRED, 
                                              location_address_trunc,
                                              long_trunc,
                                              lat_trunc)
      reply_buttons[[i]] <- list(InlineKeyboardButton(location_address, 
                                              callback_data = callback_data))
      
    }
    text <- paste("Found", nhits, "location(s). Pick a location to see its prediction!")
  } else {
    text <- "No matching locations found, please try alternative spellings, or use a postal code instead."
  }
  bot$sendMessage(update$effective_chat()$id, text, 
                  reply_markup = InlineKeyboardMarkup(
                    inline_keyboard = reply_buttons))
}

# ----- CALLBACK FUNCTION MAPPINGS ----- 


LOC_FUNCTION_KEYS <- c(CB_SHOW_LOCATION_PRED, CB_REGIONS)
LOC_FUNCTIONS <- c(send_location_prediction, send_location)
