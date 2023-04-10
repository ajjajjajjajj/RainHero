
##### test function for api #####
#-------------------------------#
#* @get /heatmap_singapore
#* @serializer contentType list(type='image/png')

get_image <- function() {
  file <- "resources/placeholder_heatmap.png"
  return(readBin(file, "raw", n = file.info(file)$size))
}

