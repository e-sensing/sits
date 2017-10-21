#files <- c("/Users/gilbertocamara/Dropbox/brickBuilder/Sinop_ndvi.tif",
#           "/Users/gilbertocamara/Dropbox/brickBuilder/Sinop_evi.tif")

files <- c("/Users/gilbertocamara/sits/inst/extdata/raster/sinop/sinop-crop-ndvi.tif",
         "/Users/gilbertocamara/sits/inst/extdata/raster/sinop/sinop-crop-evi.tif")

bands <- c("ndvi", "evi")

scale_factors <- c(0.0001, 0.0001)

timeline <- read.csv("/Users/gilbertocamara/Dropbox/BrickBuilder/mod13Q1-timeline-2000-2017.csv", header = FALSE)
timeline <- lubridate::as_date (timeline$V1)

raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)

longitude <- -55.48177
latitude  <- -11.67966

data.tb <- sits_getdata(raster.tb, longitude = longitude, latitude = latitude)

sits_plot(data.tb)

data2.tb <- sits_getdata(raster.tb, file = system.file("extdata/samples/samples_sinop_raster_crop.csv",package = "sits"))

sits_plot(data2.tb)

# retrieve the samples from EMBRAPA
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi"))

# define the patterns from data
patterns.tb <- sits_patterns(embrapa.tb, timeline)

# distances from data
distances.tb <- sits_distances_from_data(embrapa.tb, patterns.tb)

# estimate an SVM model for this training data
model.ml <- sits_svm (distances.tb, cost = 1000, kernel = "radial",tolerance = 0.001, epsilon = 0.1)

# classify a raster image
#system.time({sits_classify_raster (raster.tb, file = "/Users/gilbertocamara/Dropbox/BrickBuilder/sinop-class",
#                     patterns.tb, model.ml, multicores = 2)})

sits_classify_raster (raster.tb, file = "/Users/gilbertocamara/Dropbox/BrickBuilder/sinop-class",
                    patterns.tb, model.ml, multicores = 1)


