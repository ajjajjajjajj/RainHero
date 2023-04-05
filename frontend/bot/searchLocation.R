require(httr)

search_location <- URLencode('445B Clementi')
url <- paste('https://developers.onemap.sg/commonapi/search?searchVal=', 
             search_location, '&returnGeom=Y&getAddrDetails=Y', sep="")
search_result <- GET(url)