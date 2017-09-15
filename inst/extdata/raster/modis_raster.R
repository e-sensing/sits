library(raster)
library(rgdal)
sinop.nvdi <- raster ("/Users/gilberto/Dropbox/brickBuilder/sinop_ndvi.tif")

timelines <- read.csv("/Users/gilberto/Dropbox/brickBuilder/mod13Q1-timeline",
                      header = FALSE)
timelines <- lubridate::as_date (timelines$V1)

bands <- c("ndvi")

raster.tb <- sits_raster ("/Users/gilberto/Dropbox/brickBuilder/sinop_ndvi.tif",
                          timelines, bands)
