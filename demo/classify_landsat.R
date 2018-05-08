# Classification of LANDSAT data
# these are the symbolic links for the files at dropbox

library(sits)
library(keras)

message("Processing of a mixed Landsat 8 - MODIS data set")
message("Processing takes a while, please be patient")


evi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/LC8MOD_evi.tif")
ndvi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/LC8MOD_ndvi.tif")
nir_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/LC8MOD_nir.tif")

files <- c(ndvi_file, evi_file, nir_file)
bands <- c("ndvi", "evi", "nir")

# define the timeline

timeline <- timeline_2000_2017 [timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-222_068_2015-2016",
                           missing_values = c(0.0, 0.0, 0.0),
                           minimum_values = c(0.0, 0.0, 0.0),
                           scale_factors  = c(0.0001, 0.0001, 0.0001),
                           timeline = timeline, bands = bands, files = files)

# retrieve a sample set with 64,500 samples

samples_file <- paste0("https://www.dropbox.com/s/5xzd19kob5o2hsf/samples_Cerrado_64545series_13classes.rda?raw=1")
dest_file  <- paste0(tempdir(),"/samples_Cerrado_64545series_13classes.rda")
download.file(samples_file, destfile = dest_file)
load(dest_file)

# build an SVM model using a fraction of the samples
samples.tb <- sits_select(samples_Cerrado_64545series_13classes, bands = c("ndvi", "evi", "nir"))

dl_model <- sits_train(samples.tb,
                       ml_method = sits_deeplearning(
                           units = c(512,512,512),
                           dropout_rates = c(0.50,0.40, 0.30),
                           epochs = 300,
                           batch_size = 128))

# classify the raster image
raster_class.tb <- sits_classify_raster(file = paste0(tempdir(),"/L8_MOD_222-068-class"), raster.tb,
                                        ml_model = dl_model, memsize = 160, multicores = 40)

sits_plot_raster(raster_class.tb[1,], title = "LANDSAT-MODIS-222-068-2015-2016")

