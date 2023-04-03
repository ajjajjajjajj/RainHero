require(telegram.bot)

source('./botFunctions.R')
source('./locationFunctions.R')
source('./util.R')

# To run the bot, select all in this file and hit run.
# Then, open your telegram and send '/start'
# Use this link to access test bot: https://t.me/rainpredictor_testbot
# Am not sure what will happen if more than 1 person runs this at the same time tbh


# Functions are defined in botFunctions.R.
# Implementations we discussed should be done there.
# If your functions have a sub menu, or call additional functions,
# please create a separate R file and import them via source()


# Testing Bot access token
TOKEN <- "6283453880:AAGtUv5MlmZtbMxMZJL5HdYaD9QZv5pQ5sQ"

# Command names
# User will use these to access the commands
# for 'start', the user types '/start'
COMMAND_START <- 'start'

# initialises and adds commands to the Updater (which creates update objects)
updater <- Updater(token = TOKEN)
updater <- updater + CommandHandler(COMMAND_START, home)

# maps callbacks to the right function, throws an alert for invalid callbacks
general_callback_query <- function(bot, update) {
  print("calling general function")
  callback <- parse_callback_string(update$callback_query$data)
  id <- callback$id
  data <- callback$data
  
  if (is_valid_cb_function(id, MAIN_FUNCTION_KEYS)) {
    call_callback_function(id, MAIN_FUNCTION_KEYS, MAIN_CALLBACK_FUNCTIONS,
                           bot, update)
  } else {
    bot$answerCallbackQuery(update$effective_chat()$id, 
                            'Callback data does not map to main functions',
                            show_alert = T)
  }
}

location_callback_query <- function(bot, update) {
  print("calling location function")
  callback <- parse_callback_string(update$callback_query$data)
  id <- callback$id
  loc <- callback$data
  if (is_valid_cb_function(id, LOC_FUNCTION_KEYS)) {
    call_callback_function(id, LOC_FUNCTION_KEYS, LOC_FUNCTIONS,
                           bot, update)
  } else {
    bot$answerCallbackQuery(update$effective_chat()$id, 
                            'Callback data does not map to location functions',
                            show_alert = T)
  }
}

updater <- updater + 
  CallbackQueryHandler(general_callback_query, pattern="^general") +    
  CallbackQueryHandler(location_callback_query, pattern="^location")


# function to have the bot start listening to user input. if this is not
# run the bot does nothing
updater$start_polling()

