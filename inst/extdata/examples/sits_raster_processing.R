# Read ndvi and evi data from dropbox

# these are the symbolic links for the files at dropbox
ndvi <- paste0("https://www.dropbox.com/s/epqfo5vdu1cox6i/Sinop_ndvi.tif?raw=1")
evi <- paste0("https://www.dropbox.com/s/xb9embetftxyr6w/Sinop_evi.tif?raw=1")

# read the files to a local directory
download.file(ndvi, dest="./Sinop_ndvi.tif")
download.file(evi, dest ="./Sinop_evi.tif")

# select the files for processing
files <- c("./Sinop_ndvi.tif", "./Sinop_evi.tif")
# select the bands
bands <- c("ndvi", "evi")
# define the scale factors
scale_factors <- c(0.0001, 0.0001)
# define the timeline
timeline <- read.csv(system.file("extdata/raster/sinop/mod13Q1-timeline-2000-2017.csv", package = "sits"), header = FALSE)
timeline <- lubridate::as_date (timeline$V1)

# create a raster metadata file based on the information about the files
raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)

# print the raster metadata
print(raster.tb)

# select a location to read
longitude <- -55.48177
latitude  <- -11.67966

# test the data by reading a point
point.tb <- sits_getdata(raster.tb, longitude = longitude, latitude = latitude)
# plot the time series for the point
sits_plot(point.tb)

# read a number of time series from the data
series.tb <- sits_getdata(raster.tb, file = system.file("extdata/samples/samples_sinop_raster.csv",package = "sits"))

# plot the data
sits_plot(series.tb)

# retrieve the samples from EMBRAPA (used as training sets for classification)
samples.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

#select the bands for classification
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi"))

# define the patterns from data
patterns.tb <- sits_patterns(samples.tb)

# plot the patterns
sits_plot(patterns.tb)

# obtain the distances from data to use as training data
distances.tb <- sits_distances_from_data(samples.tb)

# estimate an SVM model for this training data
model.ml <- sits_svm (distances.tb, cost = 1000, kernel = "radial", tolerance = 0.001, epsilon = 0.1)

# classify the raster image
sits_classify_raster (file = "./sinop-class", raster.tb, samples.tb, model.ml, blocksize = 300000,
                      multicores = 2)


