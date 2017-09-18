files <- c(system.file("extdata/raster/sinop/sinop_crop_ndvi.tif",package = "sits"),
           system.file("extdata/raster/sinop/sinop_crop_evi.tif", package = "sits"))

bands <- c("ndvi", "evi")

scale_factors <- c(0.0001, 0.0001)

timeline <- read.csv(system.file("extdata/raster/sinop/mod13Q1-timeline",package = "sits"),
                      header = FALSE)
timeline <- lubridate::as_date (timeline$V1)

raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)

longitude <- -55.31578
latitude  <- -11.66757

data.tb <- sits_fromRaster(raster.tb, longitude = longitude, latitude = latitude)

sits_plot(data.tb)

data2.tb <- sits_fromRaster(raster.tb, file = system.file("extdata/samples/samples_sinop_raster_crop.csv",package = "sits"))

sits_plot(data2.tb)


