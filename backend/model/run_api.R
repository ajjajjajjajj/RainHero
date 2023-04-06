require(plumber)

api <- plumber::plumb("./plotHeatMap.R")
api$run(port = 8000)