files <- c("./inst/extdata/raster/sinop/sinop-crop-ndvi.tif",
           "./inst/extdata/raster/sinop/sinop-crop-evi.tif")

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

# retrieve the samples from EMBRAPA
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi"))

# define the patterns from data
patterns.tb <- sits_patterns_from_data(embrapa.tb)

# distances from data
distances.tb <- sits_distances_from_data(embrapa.tb, patterns.tb)

# estimate an SVM model for this training data
model.ml <- sits_svm (distances.tb, cost = 1000, kernel = "radial",tolerance = 0.001, epsilon = 0.1)

# classify a raster image
sits_classify_raster (raster.tb, file = "./inst/extdata/raster/sinop/sinop-crop-class",
                      patterns.tb, model.ml, dist_method = sits_distances_from_data())




