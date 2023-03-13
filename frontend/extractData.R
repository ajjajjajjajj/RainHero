root_s <- "https://api.data.gov.sg/v1/environment/"

days <- sprintf("%02d", 1:31)
for(dd in days) {
  uu <- paste(root_s,
                "air-temperature?date=",
                "2022-", "12-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/airtemp/", "2022", "12", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
}

for(dd in days) {
  uu <- paste(root_s,
              "relative-humidity?date=",
              "2022-", "12-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/humidity/", "2022", "12", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
}

for(dd in days) {
  uu <- paste(root_s,
              "wind-direction?date=",
              "2022-", "12-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/winddir/", "2022", "12", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))

  curl -X GET "https://api.data.gov.sg/v1/environment/relative-humidity?date=2020-02-02" -H "accept: application/json"
}

for(dd in days) {
  uu <- paste(root_s,
              "wind-speed?date=",
              "2022-", "12-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/windspeed/", "2022", "12", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
  
  curl -X GET "https://api.data.gov.sg/v1/environment/relative-humidity?date=2020-02-02" -H "accept: application/json"
}

for(dd in days) {
  uu <- paste(root_s,
              "air-temperature?date=",
              "2023-", "01-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/airtemp/", "2023", "01", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
}

for(dd in days) {
  uu <- paste(root_s,
              "relative-humidity?date=",
              "2023-", "01-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/humidity/", "2023", "01", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
}

for(dd in days) {
  uu <- paste(root_s,
              "wind-direction?date=",
              "2023-", "01-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/winddir/", "2023", "01", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
  
  curl -X GET "https://api.data.gov.sg/v1/environment/relative-humidity?date=2020-02-02" -H "accept: application/json"
}

for(dd in days) {
  uu <- paste(root_s,
              "wind-speed?date=",
              "2023-", "01-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/windspeed/", "2023", "01", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
  
  curl -X GET "https://api.data.gov.sg/v1/environment/relative-humidity?date=2020-02-02" -H "accept: application/json"
}


days <- sprintf("%02d", 1:28)
for(dd in days) {
  uu <- paste(root_s,
              "air-temperature?date=",
              "2023-", "02-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/airtemp/", "2023", "02", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
}

for(dd in days) {
  uu <- paste(root_s,
              "relative-humidity?date=",
              "2023-", "02-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/humidity/", "2023", "02", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
}

for(dd in days) {
  uu <- paste(root_s,
              "wind-direction?date=",
              "2023-", "02-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/winddir/", "2023", "02", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
  
  curl -X GET "https://api.data.gov.sg/v1/environment/relative-humidity?date=2020-02-02" -H "accept: application/json"
}

for(dd in days) {
  uu <- paste(root_s,
              "wind-speed?date=",
              "2023-", "02-", dd, sep="") 
  outname <- paste("C:/Users/hanis/OneDrive/Desktop/Y3S2/DSA3101/proj/datagov/windspeed/", "2023", "02", dd, ".csv", sep="")
  cat(outname, "\n") #catch any errors so it continues and not crash
  try(download.file(uu, outname))
  
  curl -X GET "https://api.data.gov.sg/v1/environment/relative-humidity?date=2020-02-02" -H "accept: application/json"
}
