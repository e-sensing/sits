# Read ndvi and evi data from dropbox

# these are the symbolic links for the files at dropbox
ndvi_file <- paste0("https://www.dropbox.com/s/epqfo5vdu1cox6i/Sinop_ndvi.tif?raw=1")
evi_file <- paste0("https://www.dropbox.com/s/xb9embetftxyr6w/Sinop_evi.tif?raw=1")

# read the files to a local directory
download.file(ndvi_file, destfile = "./Sinop_ndvi.tif")
download.file(evi_file,  destfile = "./Sinop_evi.tif")

# links to files
ndvi <- "./Sinop_ndvi.tif"
evi  <- "./Sinop_evi.tif"

# select the files for processing
files <- c(ndvi, evi)

# define the timeline
data(timeline_mod13q1)
timeline <- lubridate::as_date(timeline_mod13q1$V1)

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "Sinop",
                           timeline = timeline, bands = c("ndvi", "evi"), files = files)

# retrieve the samples from EMBRAPA (used as training sets for classification)
data(samples_MT_9classes)

#select the bands for classification
samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi"))

point.tb <- sits_getdata(coverage = raster.tb, longitude = -55.368, latitude = -11.694)

# classify the raster image
sits_classify_raster(file = "./sinop-class", raster.tb, samples.tb,
                     ml_method = sits_svm(cost = 10, kernel = "radial", tolerance = 0.001, epsilon = 0.1),
                     read_lines = 500, multicores = 2)


