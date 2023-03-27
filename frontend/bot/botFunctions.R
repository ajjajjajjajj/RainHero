require(telegram.bot)

# Functions for main menu are defined in this file.
# If your functions have a sub menu or call additional functions, 
# please create a separate R file and import them via source()


# ----- InlineKeyboardMarkup standard templates -----
IKM_BACK_TO_HOME <- InlineKeyboardMarkup(
  inline_keyboard = list(
    list(InlineKeyboardButton("Back to home", callback_data = CB_HOME))))
IKM_START_MENU <- InlineKeyboardMarkup(
  inline_keyboard = list(
    list(InlineKeyboardButton("Get nowcast pic", 
                              callback_data = CB_GET_PIC)),
    list(InlineKeyboardButton("Get nowcast GIF", 
                              callback_data = CB_GET_GIF)),
    list(InlineKeyboardButton('View predictions for favorite locations', 
                              callback_data = CB_GET_FAV)),
    list(InlineKeyboardButton('Set favourite from available locations',
                              callback_data = CB_SET_LOCATION)),
    list(InlineKeyboardButton('Enter new location', 
                              callback_data = CB_SET_NEW)),
    list(InlineKeyboardButton('View predictions for available locations', 
                              callback_data = CB_FIND_LOC)),
    list(InlineKeyboardButton('Help', callback_data = CB_HELP))))

# IKM_FAV_LOC <- InlineKeyboardMarkup(
#   inline_keyboard = list(
#     list(InlineKeyboardButton("Hougang",
#                               callback_data = CB_HOUGANG)),
#     list(InlineKeyboardButton("Serangoon",
#                               callback_data = CB_SERANGOON)),
#     list(InlineKeyboardButton("Punggol",
#                               callback_data = CB_PUNGGOL))
#   )
# )

# ----- FUNCTION DEFINITIONS -----
get_nowcast_picture <- function(bot, update) {
  # replace with imple
  chat_id = update$effective_chat()$id
  photo_url <- "https://telegram.org/img/t_logo.png"
  bot$sendPhoto(
    chat_id = chat_id,
    photo = photo_url,
    caption = "Telegram Logo"
  )
}

get_nowcast_gif <- function(bot, update) {
  
  chat_id = update$effective_chat()$id
  animation_url = "https://cdn.dribbble.com/users/244018/screenshots/1506924/reddit-dude.gif"
  
  bot$sendAnimation(chat_id = chat_id,
                    animation = animation_url,)
}

##### 
df <- read.csv("/Users/cynthia/Y3S2/DSA3101/practice/test_fav.csv")
locations <- as.vector(df$Amy)
locations <- locations[which(locations!="")]

locfunction <- function(vec){
  list(InlineKeyboardButton(vec, callback_data <- str(paste0("CB_", vec))))
}

get_favourite_predictions <- function(bot, update) {
  # replace with imple
  IKM_FAV_LOC <- InlineKeyboardMarkup(inline_keyboard = lapply(locations, locfunction))
  bot$send_message(update$effective_chat()$id, 
                   'These are your favourite locations',
                   reply_markup = IKM_FAV_LOC)
}


set_predefined_location <- function(bot, update) {
  # replace with imple
  bot$send_message(update$effective_chat()$id, 
                   'set_predefined_location not implemented',
                   reply_markup = IKM_BACK_TO_HOME)
}

set_new_location <- function(bot, update) {
  # replace with imple
  bot$send_message(update$effective_chat()$id, 
                   'set_new_location not implemented',
                   reply_markup = IKM_BACK_TO_HOME)
}

# reading the dataset where it contains all the mrt station names
stations_df <- read.csv("/Users/cynthia/Y3S2/DSA3101/practice/mrt_data.csv")
stations = as.vector(stations_df$station_name)

find_predefined_locations <- function(bot, update) {
  ikm_locations <- InlineKeyboardMarkup(inline_keyboard = lapply(stations, locfunction))
  # send message with predefined locations template
  bot$send_message(update$effective_chat()$id, 
                   "Choose a location from the list below:",
                   reply_markup = ikm_locations)
}


# home menu that is displayed. mapped to '/start as well (see bot.R)'
home <- function(bot, update) {
  text <- "hola amigott choose an option below to get started!"
  bot$send_message(update$effective_chat()$id, 
                   text, 
                   reply_markup = IKM_START_MENU)
}

# help page. accessible from start menu only. contains one button to return 
# to start menu
rain_help <-  function(bot, update) {
  text <- "this is the help page"
  bot$send_message(update$effective_chat()$id, 
                   text, 
                   reply_markup = IKM_BACK_TO_HOME)
}

JURONGEAST <- function(bot, update) {
  bot$send_message(update$effective_chat()$id, 
                   'This is the output for Jurong East')
}

HOUGANG <- function(bot, update) {
  bot$send_message(update$effective_chat()$id, 
                   'This is the output for Hougang')
}

SERANGOON <- function(bot, update) {
  bot$send_message(update$effective_chat()$id, 
                   'This is the output for Serangoon')
}

PUNGGOL <- function(bot, update) {
  bot$send_message(update$effective_chat()$id, 
                   'This is the output for Punggol')
}

# ----- CALLBACK DATA MAPPINGS -----
# callback data is sent when buttons are clicked, and each of them should map
# to a specific function

# callback keys
CB_HOME <- 'home'
CB_GET_PIC <- 'get_pic' 
CB_GET_GIF <- 'get_gif'
CB_GET_FAV <- 'get_fav' # get readings for favorite locations (text)
CB_SET_LOCATION <- 'set_loc' # let's use this for the pre-defined locations
CB_SET_NEW <- 'set_new_loc' # let's use this for custom locations (up to user)
CB_FIND_LOC <- 'find_loc' # list current locations
CB_HELP <- 'help'

# fav locs
CB_HOUGANG <- 'HOUGANG'
CB_SERANGOON <- 'SERANGOON'
CB_PUNGGOL <- 'PUNGGOL'

CB_JURONGEAST <- 'JURONG_EAST'

callback_keys <- c(CB_GET_PIC, CB_GET_GIF, CB_GET_FAV, 
                   CB_SET_LOCATION, CB_SET_NEW, CB_FIND_LOC, CB_HELP, CB_HOME,
                   CB_HOUGANG, CB_SERANGOON, CB_PUNGGOL, CB_JURONGEAST)

callback_functions <- c(get_nowcast_picture, get_nowcast_gif, 
                        get_favourite_predictions, set_predefined_location, 
                        set_new_location, find_predefined_locations, rain_help,
                        home, HOUGANG, SERANGOON, PUNGGOL, JURONGEAST)


# ----- CALLBACK -----
# takes in a callback key defined in next section and calls the function
call_callback_function <- function(cb_key, bot, update) {
  do.call(callback_functions[[which(callback_keys == cb_key)]], list(bot, update))
}

# call_callback_function_loc <- function(cb_key, bot, update, locations) {
#   do.call(callback_functions[[which(callback_keys == cb_key)]], list(bot, update))
# }

# ----- UTILITY FUNCTIONS -----
is_valid_cb_function <- function(cb_key) {
  return(cb_key %in% callback_keys)
}
