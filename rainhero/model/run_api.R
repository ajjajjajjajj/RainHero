require(plumber)

api <- pr()

map <- plumb("plotHeatMapTest.R")
predictions <- plumb("krigingUserTest.R")

api$mount("map", map)
api$mount("predict", predictions)

api$run(port = 8000, host="0.0.0.0")
