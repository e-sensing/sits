# Read ndvi and evi data from dropbox

# these are the symbolic links for the files at dropbox

l8_221065_evi <- paste0("LC8SR-MOD13Q1-MYD13Q1_221065_2013-08-29_evi_BRICK.tif")
l8_221065_ndvi <- paste0("LC8SR-MOD13Q1-MYD13Q1_221065_2013-08-29_ndvi_BRICK.tif")
# read the files to a local director

# select the files for processing
files <- c(l8_221065_ndvi, l8_221065_evi)

data_dir <- paste0("/Users/gilberto/raster/")
l8_224_73_ndvi <- paste0(data_dir,"224073_ndvi_part.tif")
l8_224_73_evi <- paste0(data_dir,"224073_evi_part.tif")

files <- c(l8_224_73_ndvi, l8_224_73_evi)

# define the timeline
#data(timeline_mod13q1)
#timeline <- lubridate::as_date(timeline_mod13q1$V1)
timeline <- sits:::sits.env$config["RASTER_timeline"]$RASTER_timeline$MOD13Q1

timeline1 <- timeline [timeline >= lubridate::as_date("2016-08-28")]
timeline1 <- timeline1[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(files = files, name = "L8MOD-224_73_2016",
                           timeline = timeline1, bands = c("ndvi", "evi"))

point.tb <- sits_getdata(raster.tb, latitude = -18.70352, longitude = -53.46834)

# retrieve the samples from EMBRAPA (used as training sets for classification)

samples <- paste0("https://www.dropbox.com/s/p1fhzokfwdoum2m/cerrado_13_classes_col6_adj.rda?raw=1")

download.file(samples, destfile = "./cerrado_13classes_col6_adj.rda")
load(file = "./cerrado_13classes_col6_adj.rda")

#select the bands for classification
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi"))

# classify the raster image
sits_classify_raster(file = "./sinop-class", raster.tb, samples.tb,
                     ml_method = sits_svm(cost = 10, kernel = "radial", tolerance = 0.001, epsilon = 0.1),
                     blocksize = 300000, multicores = 2)


