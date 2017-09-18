library(raster)
library(rgdal)

files <- c("/Users/gilberto/Dropbox/brickBuilder/sinop_ndvi.tif",
           "/Users/gilberto/Dropbox/brickBuilder/sinop_evi.tif")

bands <- c("ndvi", "evi")

scale_factors <- c(0.0001, 0.0001)

timelines <- read.csv("/Users/gilberto/Dropbox/brickBuilder/mod13Q1-timeline",
                      header = FALSE)
timelines <- lubridate::as_date (timelines$V1)

raster.tb <- sits_STRaster (files, timelines, bands, scale_factors)

longitude <- -55.49035
latitude  <- -11.65125

data.tb <- sits_fromRaster(raster.tb, longitude = longitude, latitude = latitude)
