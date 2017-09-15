library(raster)
library(rgdal)

files <- c("/Users/gilberto/Dropbox/brickBuilder/sinop_ndvi.tif",
           "/Users/gilberto/Dropbox/brickBuilder/sinop_evi.tif")

bands <- c("ndvi", "evi")

timelines <- read.csv("/Users/gilberto/Dropbox/brickBuilder/mod13Q1-timeline",
                      header = FALSE)
timelines <- lubridate::as_date (timelines$V1)

raster.tb <- sits_raster (files, timelines, bands)
