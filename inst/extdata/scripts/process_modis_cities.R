# Read ndvi and evi data from dropbox

library(sits)
library(rgdal)
library(raster)

cities
# these are the symbolic links for the files at dropbox

evi <- paste0("h13v10-evi.tiff")
ndvi <- paste0("h13v10-ndvi.tiff")
nir <- paste0("h13v10-nir.tiff")
mir <- paste0("h13v10-mir.tiff")
# read the files to a local director


data_dir <- paste0("/home/gilberto/raster/")
ndvi <- paste0(data_dir, ndvi)
evi <- paste0(data_dir,evi)
nir <- paste0(data_dir,nir)
mir <- paste0(data_dir,mir)

files <- c(ndvi, evi, nir, mir)

# define the timeline


timeline.csv <- read.csv(paste0(data_dir,"timeline.csv"), header = FALSE)
timeline <- lubridate::as_date(timeline.csv$V1)

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(files = files, name = "MODIS-h13-v10-2000-2017",
                           timeline = timeline, bands = c("ndvi", "evi", "nir", "mir"))


# # retrieve the samples from EMBRAPA (used as training sets for classification)
# samples <- paste0("https://www.dropbox.com/s/16fgr2ds9ilj3o0/cerrado_13classes_modis_col6.rda?raw=1")
#
# download.file(samples, destfile = "./cerrado_13classes_col6.rda")
# load(file = "./cerrado_13classes_col6.rda")

#select the bands for classification
samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi", "nir", "mir"))

sits_labels(samples.tb)

# classify the raster image
sits_classify_raster(file = "/home/gilberto/raster/MOD-h13v10-class", raster.tb, samples.tb,
                     ml_method = sits_svm(),
                     blocksize = 960000, multicores = 20)


