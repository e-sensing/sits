library(sits)
library(keras)
# install_keras()

message("Processing of a mixed Landsat 8 - MODIS data set")
message("Please ensure that you have enough memory available")


# select a file with 65,000 samples
samples_file <- paste0("https://www.dropbox.com/s/uwp7d2m7n71ur2p/samples_25042018.rda?raw=1")
download.file(samples_file, destfile = paste0(tempdir(),"/samples_Cerrado_25042018.rda"))
load(file = paste0(tempdir(),"/samples_Cerrado_25042018.rda"))

# reduce the number of samples to 30% of total (speeds up demo)
samples.tb <- sits_sample(samples_Cerrado_25042018.tb, frac = 0.3)

# select only the ndvi and evi bands
samples.tb <- sits_select_bands(samples.tb, bands = c("ndvi", "evi", "nir"))

# train the deep learning model
dl_model <-  sits_train(samples.tb,
                        ml_method = sits_deeplearning(
                             units            = c(512, 512, 512),
                             activation       = 'elu',
                             dropout_rates    = c(0.50, 0.40, 0.35),
                             optimizer = keras::optimizer_adam(),
                             epochs = 300,
                             batch_size = 128,
                             validation_split = 0.2))

sits_keras_diagnostics(dl_model)

# select the bands "ndvi", "evi", "nir" and "mir"

evi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/LC8MOD_evi.tif")
ndvi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/LC8MOD_ndvi.tif")
nir_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/LC8MOD_nir.tif")

files <- c(ndvi_file, evi_file, nir_file)

# define the timeline

timeline <- timeline_2000_2017[timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-222-068_2015-2016",
                           missing_values = c(0.0, 0.0, 0.0),
                           minimum_values = c(0.0, 0.0, 0.0),
                           scale_factors  = c(0.0001, 0.0001, 0.0001),
                           timeline = timeline, bands = c("ndvi", "evi", "nir"), files = files)

# classify the raster image
raster_class.tb <- sits_classify_raster(file = "./L8_MOD_222-068-class", raster.tb,
                                        ml_model = dl_model, memsize = 5, multicores = 2)

sits_plot_raster(raster_class.tb[1,], title = "LANDSAT-MODIS-222-068-2015-2016")
