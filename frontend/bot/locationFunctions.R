require(telegram.bot)

source('botFunctions.R')
source('util.R')


# ----- IMPORT SAMPLE FAV LOCS -----
# replace this part with connection to SQL database
df <- read.csv("../test_fav.csv") 
locations <- as.vector(df$Amy)
FAV_LOCATIONS_AMY <- locations[which(locations!="")]



# ----- IMPORT mrt/lrt data -----
stations_df <- read.csv("../mrt_lrt_data.csv")
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
# appends the data together into a string with a separator "|"
make_location_button <- function(location, callback_id) {
  data <- create_callback_string(callback_id, location)
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
  region <- parse_callback_string(update$callback_query$data)$data
  
  if (!is_region_available(region)) {
    error_msg <- "region is not available - no prediction is found"
    bot$send_message(update$effective_chat()$id, error_msg,
                     reply_markup = InlineKeyboardMarkup(
                       inline_keyboard = BUTTON_BACK_TO_HOME))
  } else {
    ikm_function <- eval(parse(text = paste0("IKM_", toupper(region))))
    reply_buttons <- append(BUTTON_BACK_TO_HOME, 
                            ikm_function)
    bot$send_message(update$effective_chat()$id,
                     paste0("Choose a location from ", region),
                     reply_markup = InlineKeyboardMarkup(
                       inline_keyboard = reply_buttons))
  }
}
   

# replies the user with a prediction for an available location
# if the location is not available, sends an error message
send_location_prediction <- function(bot, update) {
  location <- parse_callback_string(update$callback_query$data)$data
  
  if (!is_location_available(location)) {
    error_msg <- "location is not available - no prediction is found"
    bot$send_message(update$effective_chat()$id, error_msg,
                     reply_markup = InlineKeyboardMarkup(
                       inline_keyboard = BUTTON_BACK_TO_HOME))
  } else {
    text <- paste("This is the output for ", location)
    bot$send_message(update$effective_chat()$id,
                      text, 
                     reply_markup = InlineKeyboardMarkup(
                       inline_keyboard = BUTTON_BACK_TO_HOME))# to replace text with a template
  }
}


# ----- CALLBACK FUNCTION MAPPINGS ----- 


LOC_FUNCTION_KEYS <- c(CB_SHOW_LOCATION_PRED, CB_REGIONS)
LOC_FUNCTIONS <- c(send_location_prediction, send_location)
