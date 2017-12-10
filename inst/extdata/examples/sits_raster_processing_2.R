# Read ndvi and evi data from dropbox


# select the files for processing
files <- c("./MT.tif")
# select the bands
bands <- c("ndvi")
# define the scale factors
scale_factors <- c(0.0001)
# define the timeline
timeline <- read.csv("./MT.txt", header = FALSE)
timeline <- lubridate::as_date (timeline$V1)

# create a raster metadata file based on the information about the files
raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)

# print the raster metadata
print(raster.tb)

# select a location to read
longitude <- -55.48177
latitude  <- -12.67966


# test the data by reading a point
point.tb <- sits_getdata(raster.tb, longitude = longitude, latitude = latitude)
# plot the time series for the point
sits_plot(point.tb)

# retrieve the samples from EMBRAPA (used as training sets for classification)
samples.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

#select the bands for classification
samples.tb <- sits_select(samples.tb, bands = c("ndvi"))


# define the patterns from data
patterns.tb <- sits_patterns(samples.tb)

# plot the patterns
sits_plot(patterns.tb)

# classify the raster image

system.time({sits_classify_raster (file = "./embrapa-class",
                                   raster.tb, samples.tb,
                                   ml_method = sits_svm (cost = 1000, kernel = "radial", tolerance = 0.001, epsilon = 0.1),
                                   blocksize = 300000, multicores = 2)})

