# /pic

pic <- function(bot, update) {
  chat_id = update$message$chat_id
  photo_url <- "https://telegram.org/img/t_logo.png"
  
  bot$sendPhoto(
    chat_id = chat_id,
    photo = photo_url,
    caption = "Telegram Logo"
  )
}

gif <- function(bot, update) {
  chat_id = update$message$chat_id
  animation_url = "https://cdn.dribbble.com/users/244018/screenshots/1506924/reddit-dude.gif"
  
  bot$sendAnimation(chat_id = chat_id,
                    animation = animation_url,)
}