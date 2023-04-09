
##### test function for api #####
#-------------------------------#
#* @get /get_image
get_image <- function() {
  file <- "../bot/resources/logo.jpeg"
  return(readBin(file, "raw", n = file.info(file)$size))
}

