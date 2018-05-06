# Classification of LANDSAT data
# these are the symbolic links for the files at dropbox

library(sits)
library(e1071)

message("Processing of a mixed Landsat 8 - MODIS data set")
message("Processing takes a while, please be patient")
evi_file <- paste0("/vsicurl/https://www.dropbox.com/s/dykv1o4ut1d09ok/LC8MODIS_222_068_2015_evi.tif?raw=1")
ndvi_file <- paste0("/vsicurl/https://www.dropbox.com/s/p7z69cjo87xgveu/LC8MODIS_222_068_2015_ndvi.tif?raw=1")
nir_file <- paste0("/vsicurl/https://www.dropbox.com/s/e8lonfuxn6a205d/LC8MODIS_222_068_2015_nir.tif?raw=1")
mir_file <- paste0("/vsicurl/https://www.dropbox.com/s/wvp7y95gy2y1e4n/LC8MODIS_222_068_2015_swir2.tif?raw=1")

files <- c(ndvi_file, evi_file, nir_file, mir_file)
bands <- c("ndvi", "evi", "nir", "mir")

# define the timeline

timeline <- timeline_2000_2017 [timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-222_068_2015-2016",
                           missing_values = c(0.0, 0.0, 0.0, 0.0),
                           minimum_values = c(0.0, 0.0, 0.0, 0.0),
                           scale_factors  = c(0.0001, 0.0001, 0.0001, 0.001),
                           timeline = timeline, bands = bands, files = files)

# retrieve a sample set with 64,500 samples

samples_file <- paste0("https://www.dropbox.com/s/5xzd19kob5o2hsf/samples_Cerrado_64545series_13classes.rda?raw=1")
dest_file  <- paste0(tempdir(),"/samples_Cerrado_64545series_13classes.rda")
download.file(samples_file, destfile = dest_file)
load(dest_file)

# build an SVM model using a fraction of the samples
samples.tb <- sits_select(samples_Cerrado_64545series_13classes, bands = c("ndvi", "evi", "nir", "mir"))
samples.tb <- sits_sample(samples.tb, frac = 0.5)

svm_model <- sits_train(samples.tb, ml_method = sits_svm(normalize = FALSE))

message("Please select memory size avaliable for processing in GB (tipical values 1 - 100)")
memsize <- as.integer(readline(prompt = "Enter a memsize value: "))

message("Please select number of cores for processing")
multicores <- as.integer(readline(prompt = "Enter a value: "))

# classify the raster image
raster_class.tb <- sits_classify_raster(file = paste0(tempdir(),"/L8_MOD_222-068-class"), raster.tb,
                                        ml_model = svm_model, memsize = memsize, multicores = multicores)

sits_plot_raster(raster_class.tb[1,], title = "LANDSAT-MODIS-222-068-2015-2016")

