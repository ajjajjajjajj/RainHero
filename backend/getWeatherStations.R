library(magrittr)
library(dplyr)
library(httr)
library(jsonlite)

# Credit to https://github.com/andrew-loh/neaSG/blob/master/R/get_weatherstns.R
get_weatherstns <- function() {
  requireNamespace("magrittr")
  #Endpoint for API
  URL <- "https://api.data.gov.sg/v1/environment/rainfall"
  
  #Define function which can then be mapped over list of dates
  query_params <- list(
    "date" = Sys.Date()
  )
  get_result <- httr::GET(url = URL, query = query_params)
  json <- httr::content(get_result)
  
  return(purrr::map(json[[1]][[1]], function(x) as.data.frame(x)) %>% data.table::rbindlist())
}