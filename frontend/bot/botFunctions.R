require(telegram.bot)

# Functions for main menu are defined in this file.
# If your functions have a sub menu or call additional functions, 
# please create a separate R file and import them via source()

source('util.R')

# ----- InlineKeyboardMarkup standard templates -----
BUTTON_BACK_TO_HOME <- list(list(
  InlineKeyboardButton("Back to home", callback_data = GENERAL_HOME)))
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


# ----- FUNCTION DEFINITIONS -----
get_nowcast_picture <- function(bot, update) {
  # replace with imple
  chat_id = update$effective_chat()$id
  photo_url <- "https://telegram.org/img/t_logo.png" # change after connecting to database
  bot$sendPhoto(
    chat_id = chat_id,
    photo = photo_url,
    reply_markup = InlineKeyboardMarkup(inline_keyboard = BUTTON_BACK_TO_HOME)
  )
}

get_nowcast_gif <- function(bot, update) {
  
  chat_id = update$effective_chat()$id
  animation_url = "https://cdn.dribbble.com/users/244018/screenshots/1506924/reddit-dude.gif" # change after connecting to database
  
  bot$sendAnimation(chat_id = chat_id,
                    animation = animation_url, 
                    reply_markup = InlineKeyboardMarkup(inline_keyboard = BUTTON_BACK_TO_HOME))
 
                   
}




# shows the user the list of locations they have in their favourites
# clicking on the button leads to a page showing the prediction result
get_favourite_predictions <- function(bot, update) {
  reply_buttons <- append(BUTTON_BACK_TO_HOME, 
                          IKM_FAV_LOCATIONS_AMY)
  bot$send_message(update$effective_chat()$id, 
                   'These are your favourite locations',
                   reply_markup = InlineKeyboardMarkup(
                     inline_keyboard = reply_buttons))
}

# shows the user the list of available locations
# clicking on the button leads to a page showing the prediction result
find_predefined_locations <- function(bot, update) {
  reply_buttons <- append(BUTTON_BACK_TO_HOME, 
                          IKM_NSEW)
  bot$send_message(update$effective_chat()$id, 
                   "Choose a region:",
                   reply_markup = InlineKeyboardMarkup(
                     inline_keyboard = reply_buttons))
}


set_predefined_location <- function(bot, update) {
  # replace with imple
  bot$send_message(update$effective_chat()$id, 
                   'set_predefined_location not implemented',
                   reply_markup = InlineKeyboardMarkup(
                     inline_keyboard = BUTTON_BACK_TO_HOME))
}

set_new_location <- function(bot, update) {
  # replace with imple
  bot$send_message(update$effective_chat()$id, 
                   'set_new_location not implemented',
                   reply_markup = InlineKeyboardMarkup(
                     inline_keyboard = BUTTON_BACK_TO_HOME))
}


# home display for start of session. NOT a callback function.
start_home <- function(bot, update) {
  print(paste("Chat session started with", update$effective_user()$first_name,
              ", username:", update$effective_user()$username))
  text <- "Hohoho! I am RainHero and today I am here to save you from the rain!
  \nView 30-minute predictions for rain across locations in Singapore!
  \nSelect one of the options below to get started."
  bot$send_message(update$effective_chat()$id, 
                   text, 
                   reply_markup = IKM_START_MENU)
}



# displays the home menu.
home <- function(bot, update) {
  text <- "Select among the options below to find rain predictions."
  bot$send_message(update$effective_chat()$id, 
                   text, 
                   reply_markup = IKM_START_MENU)
}

# help page. accessible from start menu only. contains one button to return 
# to start menu
rain_help <-  function(bot, update) {
  text <- "this is the help page"
  print(update$effective_chat()$id)
  bot$send_message(update$effective_chat()$id, text, 
                   reply_markup = InlineKeyboardMarkup(
                     inline_keyboard = BUTTON_BACK_TO_HOME))
}

# ----- CALLBACK DATA MAPPINGS -----
# callback data is sent when buttons are clicked, and each of them should map
# to a specific function

MAIN_FUNCTION_KEYS <- c(CB_GET_PIC, CB_GET_GIF, CB_GET_FAV, 
                        CB_SET_LOCATION, CB_SET_NEW, CB_FIND_LOC, CB_HELP, GENERAL_HOME)

MAIN_CALLBACK_FUNCTIONS <- c(get_nowcast_picture, get_nowcast_gif, 
                             get_favourite_predictions, set_predefined_location, 
                             set_new_location, find_predefined_locations, rain_help,
                             home)


# ----- CALLBACK -----
# takes in a callback key defined in next section and calls the function
call_callback_function <- function(cb_key, relevant_keys, relevant_functions,
                                   bot, update) {
  do.call(relevant_functions[[which(relevant_keys == cb_key)]],
          list(bot, update))
}

# ----- UTILITY FUNCTIONS -----
is_valid_cb_function <- function(cb_key, relevant_keys) {
  return(cb_key %in% relevant_keys)
}