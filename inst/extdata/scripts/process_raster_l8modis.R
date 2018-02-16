# Read ndvi and evi data from dropbox

# these are the symbolic links for the files at dropbox

l8_221065_evi <- paste0("LC8SR-MOD13Q1-MYD13Q1_221065_2013-08-29_evi_BRICK.tif")
l8_221065_ndvi <- paste0("LC8SR-MOD13Q1-MYD13Q1_221065_2013-08-29_ndvi_BRICK.tif")
l8_221065_nir <- paste0("LC8SR-MOD13Q1-MYD13Q1_221065_2013-08-29_nir_BRICK.tif")
# read the files to a local director


data_dir <- paste0("/home/gilberto/l8-modis/")
ndvi <- paste0(data_dir, l8_221065_ndvi)
evi <- paste0(data_dir,l8_221065_evi)
nir <- paste0(data_dir,l8_221065_nir)

files <- c(ndvi, evi)

# define the timeline
#data(timeline_mod13q1)
#timeline <- lubridate::as_date(timeline_mod13q1$V1)
timeline <- sits:::sits.env$config["RASTER_timeline"]$RASTER_timeline$MOD13Q1

timeline1 <- timeline [timeline >= lubridate::as_date("2013-08-29")]
timeline1 <- timeline1[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(files = files, name = "L8MOD-221_65_2013-2014",
                           timeline = timeline1, bands = c("ndvi", "evi"))

point.tb <- sits_getdata(raster.tb, latitude = -7.23, longitude = -46.15)

# retrieve the samples from EMBRAPA (used as training sets for classification)

samples <- paste0("https://www.dropbox.com/s/p1fhzokfwdoum2m/cerrado_13_classes_col6_adj.rda?raw=1")

download.file(samples, destfile = "./cerrado_13classes_col6_adj.rda")
load(file = "./cerrado_13classes_col6_adj.rda")

#select the bands for classification
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi", "nir"))

# classify the raster image
sits_classify_raster(file = "./L8_MOD_221-065-class", raster.tb, samples.tb,
                     ml_method = sits_svm(),
                     read_lines = 200, multicores = 16)


