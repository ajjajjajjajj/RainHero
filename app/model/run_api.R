require(plumber)

api <- pr()

map <- plumb("plotHeatMap.R")
predictions <- plumb("krigingUser.R")

api$mount("map", map)
api$mount("predict", predictions)

api$run(port = 8000, host="0.0.0.0")