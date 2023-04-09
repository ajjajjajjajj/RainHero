require(httr)


## matches a search key with a location in Singapore using the OneMap API

# search key examples the columns are given below
# "SEARCHVAL": "PCF SPARKLETOTS PRESCHOOL@SENGKANG WEST 415A (CC)",
# "BLK_NO": "415A",
# "ROAD_NAME": "FERNVALE LINK",
# "BUILDING": "PCF SPARKLETOTS PRESCHOOL@SENGKANG WEST 415A (CC)",
# "ADDRESS": "415A FERNVALE LINK PCF SPARKLETOTS PRESCHOOL@SENGKANG WEST 415A (CC) SINGAPORE 791415",
# "POSTAL": "791415",
# "X": "33124.6575873252",
# "Y": "41214.8146092203",
# "LATITUDE": "1.38900620596043",
# "LONGITUDE": "103.879367596607",
# "LONGTITUDE": "103.879367596607"
get_location_matches <- function(search_key) {
  search_location <- URLencode(search_key)
  url <- paste('https://developers.onemap.sg/commonapi/search?searchVal=', 
               search_location, '&returnGeom=Y&getAddrDetails=Y', sep="")
  response <- GET(url)
  if (status_code(response) != 200) { # successful request
    print(paste("get_location_matches failed with status code", 
                status_code(response)))
    return(data.frame())
  } 
  tryCatch({
    search_result <- content(response, as = "parsed")$results
    if (is.null(search_result)) {
      return(data.frame())
    }
    result_table <- as.data.frame(do.call(rbind, search_result))
    return(result_table)
  }, error=function(err) {
    print(err)
    return(data.frame())
  })
}

