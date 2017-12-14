# Read ndvi and evi data from dropbox

# these are the symbolic links for the files at dropbox
ndvi <- paste0("https://www.dropbox.com/s/epqfo5vdu1cox6i/Sinop_ndvi.tif?raw=1")
evi <- paste0("https://www.dropbox.com/s/xb9embetftxyr6w/Sinop_evi.tif?raw=1")

# read the files to a local directory
download.file(ndvi, dest="./Sinop_ndvi.tif")
download.file(evi, dest ="./Sinop_evi.tif")

# select the files for processing
files <- c("./Sinop_ndvi.tif", "./Sinop_evi.tif")

# define the timeline
timeline <- read.csv(system.file("extdata/mod13q1/timeline.csv", package = "sits"), header = FALSE)
timeline <- lubridate::as_date (timeline$V1)

# create a raster metadata file based on the information about the files
raster.tb <- sits_STRaster (files, timeline, bands = c("ndvi", "evi"), scale_factors = c(0.0001, 0.0001))

# retrieve the samples from EMBRAPA (used as training sets for classification)
samples.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

#select the bands for classification
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi"))

# classify the raster image
sits_classify_raster (file = "./sinop-class", raster.tb, samples.tb,
                      ml_method = sits_svm (cost = 1000, kernel = "radial", tolerance = 0.001, epsilon = 0.1),
                      blocksize = 300000, multicores = 2)


