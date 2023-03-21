require(telegram.bot)

source('./botFunctions.R')

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
COMMAND_HELP <- 'help'

# initialises and adds commands to the Updater (which creates update objects)
updater <- Updater(token = TOKEN)
updater <- updater + CommandHandler('start', home) +
  CommandHandler('help', rain_help)

# maps callbacks to the right function, throws an alert for invalid callbacks
callback_query_map <- function(bot, update) {
  data <- update$callback_query$data
  if (is_valid_cb_function(data)) {
    call_callback_function(data, bot, update)
  } else {
    bot$answerCallbackQuery(update$effective_chat()$id, 
                            'callback_data doesnt map to anything',
                            show_alert = T)
  }
}
updater <- updater + CallbackQueryHandler(callback_query_map)

# function to have the bot start listening to user input. if this is not
# run the bot does nothing
updater$start_polling()
