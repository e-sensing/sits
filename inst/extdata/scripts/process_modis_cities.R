# Read ndvi and evi data from dropbox

library(sits)
library(rgdal)
library(raster)

cities <- c("SaoFelixDoAraguaia_MT")

blue_SFA <- paste0("https://www.dropbox.com/s/b9gxkk4fkroemzm/SaoFelixDoAraguaia_MT_blue_reflectance.tif?raw=1")
evi_SFA  <- paste0("https://www.dropbox.com/s/mtx4c024aspt1sv/SaoFelixDoAraguaia_MT_EVI.tif?raw=1")
mir_SFA  <- paste0("https://www.dropbox.com/s/5du4nxwatdvb6bt/SaoFelixDoAraguaia_MT_MIR_reflectance.tif?raw_1")
ndvi_SFA <- paste0("https://www.dropbox.com/s/z6dgfc48ezvfqv6/SaoFelixDoAraguaia_MT_NDVI.tif?raw=1")
nir_SFA  <- paste0("https://www.dropbox.com/s/dmvv33tqujoo39y/SaoFelixDoAraguaia_MT_NIR_reflectance.tif?raw=1")
red_SFA  <- paste0("https://www.dropbox.com/s/dl5fz29qmxxqzbv/SaoFelixDoAraguaia_MT_red_reflectance.tif?raw=1")
timeline_SFA <- paste0("https://www.dropbox.com/s/j1jbx1rozettwmo/SaoFelixDoAraguaia_MT_timeline.csv?raw=1")
# these are the symbolic links for the files at dropbox

# these are the links to access data via GDAL
vsi_ndvi_SFA <- paste0("/vsicurl/", ndvi_SFA)
files <- c(vsi_ndvi_SFA)

# define the timeline
download.file(timeline_SFA, destfile = "./timeline.csv")
timeline.csv <- read.csv("./timeline.csv", header = FALSE)
timeline <- lubridate::as_date(timeline.csv$V1)

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(files = files, name = "MODIS-Sao-Felix-Araguaia",
                           timeline = timeline, bands = c("ndvi"))

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


