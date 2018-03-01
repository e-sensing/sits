# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

# Read ndvi and evi data from dropbox
# these are the symbolic links for the files at dropbox

ndvi_file <- paste0("https://www.dropbox.com/s/2ckisfw7s5oyncp/Sinop_ndvi.tif?raw=1")
evi_file <- paste0("https://www.dropbox.com/s/xo83mrn3jphkbmg/Sinop_evi.tif?raw=1")

# read the files to a local directory
download.file(ndvi_file, destfile = "./Sinop_ndvi.tif")
download.file(evi_file,  destfile = "./Sinop_evi.tif")

# links to files
ndvi <- "./Sinop_ndvi.tif"
evi  <- "./Sinop_evi.tif"

# select the files for processing
files <- c(ndvi, evi)

# define the timeline
data(timeline_modis_392)

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "Sinop",
                           timeline = timeline_modis_392, bands = c("ndvi", "evi"), files = files)

#select the bands for classification
samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi"))

point.tb <- sits_getdata(coverage = raster.tb, longitude = -55.368, latitude = -11.694)

# classify the raster image
sits_classify_raster(file = "./sinop-class", raster.tb, samples.tb,
                     ml_method = sits_svm(cost = 10, kernel = "radial", tolerance = 0.001, epsilon = 0.1),
                     blocksize = 250000, multicores = 2)



