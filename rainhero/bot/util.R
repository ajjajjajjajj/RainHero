require(telegram.bot)
require(stringr)

# ----- DOCKER -----
URL_MODEL <- "http://model:8000/predict/forecast_user_location" # model container runs on port 8000
URL_NOWCAST_PIC <- "http://model:8000/map/heatmap_singapore"

# ----- UTIL FUNCTIONS -----
# creates a callback string. requires callback_id to be a string 
create_callback_string <- function(callback_id, ...) {
  if (class(callback_id) != "character") {
    stop("callback_id must be of type 'character'")
  }
  v <- list(...)
  result <- callback_id
  if (length(v) == 0) {
    return(result)
  }
  for (i in 1:length(v)) {
    result <- paste(result,v[[i]],sep="|")
  }
  return(result)
}


# parses a callback string. returns a list of strings
parse_callback_string <- function(callback) {
  callback_values <- str_split(callback, "\\|")[[1]]
  result_list <- list() %>% append(list(id = callback_values[1]))
  if (length(callback_values) > 1) {
    result_list <- result_list %>% 
      append(list(data = callback_values[2:length(callback_values)]))
  }
  return(result_list)
}


# ----- CALLBACK KEYS -----
CB_SHOW_LOCATION_PRED <- 'loc_pred'
CB_REGIONS <- 'loc_region'

# ----- CALLBACK KEYS ----- 
GENERAL_HOME <- 'general_home'
CB_GET_PIC <- 'general_get_pic' 
CB_GET_GIF <- 'general_get_gif'
CB_GET_FAV <- 'general_get_fav' # get readings for favorite locations (text)
CB_SET_LOCATION <- 'general_set_loc' # let's use this for the pre-defined locations
CB_SET_NEW <- 'general_set_new_loc' # let's use this for custom locations (up to user)
CB_FIND_LOC <- 'general_find_loc' # list current locations
CB_HELP <- 'general_help'

BUTTON_BACK_TO_HOME <- list(list(
  InlineKeyboardButton("Back to home 🏠", callback_data = GENERAL_HOME)))

