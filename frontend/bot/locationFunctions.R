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

# returns true if given location is available
is_location_available <- function(location) {
  return(location %in% AVAILABLE_LOCATIONS)
}


# -----  ALL AVAILABLE LOCATIONS BUTTON TEMPLATE -----
IKM_GET_AVAILABLE_LOCATIONS <- make_location_ikm(AVAILABLE_LOCATIONS)
IKM_FAV_LOCATIONS_AMY <- make_location_ikm(FAV_LOCATIONS_AMY)



# ----- LOCATION FUNCTIONS ------

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


LOC_FUNCTION_KEYS <- c(CB_SHOW_LOCATION_PRED)
LOC_FUNCTIONS <- c(send_location_prediction)
