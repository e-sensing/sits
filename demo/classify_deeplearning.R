library(sits)
library(keras)
# install_keras()

message("Processing of a mixed Landsat 8 - MODIS data set")
message("Please ensure that you have enough memory available")

# Retrieve the set of samples for the Cerrado region (provided by EMBRAPA)

# select a file with 61,000 samples
samples_file <- paste0("https://www.dropbox.com/s/5llfs8w371u6lng/samples_Cerrado_01042018.rda?raw=1")
download.file(samples_file, destfile = "./samples_Cerrado_01042018.rda")
load(file = "./samples_Cerrado_01042018.rda")

# select only the ndvi and evi bands
samples.tb <- sits_select_bands(samples_Cerrado_01042018.tb, bands = c("ndvi", "evi", "nir", "mir"))

# train the deep learning model
dl_model <-  sits_train(samples.tb,
                        ml_method = sits_deeplearning(
                             units            = c(512, 512, 512, 512, 512),
                             activation       = 'elu',
                             dropout_rates    = c(0.50, 0.40, 0.35, 0.30, 0.20),
                             optimizer = keras::optimizer_adam(),
                             epochs = 500,
                             batch_size = 128,
                             validation_split = 0.2))

sits_keras_diagnostics()

# select the bands "ndvi", "evi", "nir" and "mir"

l8m_222068_evi_file <- paste0("/vsicurl/https://www.dropbox.com/s/4tpuwb56pjjo12h/LCMOD_2015-08-29_evi_800.tif?raw=1")
l8m_222068_ndvi_file <- paste0("/vsicurl/https://www.dropbox.com/s/s9n7vsaoos366vb/LCMOD_2015-08-29_ndvi_800.tif?raw=1")
l8m_222068_nir_file <- paste0("/vsicurl/https://www.dropbox.com/s/d54f7gyjr575d6f/LCMOD_2015-08-29_nir_800.tif?raw=1")
l8m_222068_mir_file <- paste0("/vsicurl/https://www.dropbox.com/s/zm6fpitk5opg6be/LCMOD_2015-08-29_swir2_800.tif?raw=1")

files <- c(l8m_222068_ndvi_file, l8m_222068_evi_file, l8m_222068_nir_file, l8m_222068_mir_file)

# define the timeline

timeline <- timeline_2000_2017[timeline_2000_2017 >= lubridate::as_date("2015-08-29")]
timeline <- timeline[1:23]

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "L8MOD-222-068_2015-2016",
                           missing_values = c(0.0, 0.0, 0.0, 0.0),
                           minimum_values = c(0.0, 0.0, 0.0, 0.0),
                           scale_factors  = c(0.0001, 0.0001, 0.0001, 0.0001),
                           timeline = timeline, bands = c("ndvi", "evi", "nir", "mir"), files = files)

# classify the raster image
# note: it is important to use the original samples and to select the normalization option
raster_class.tb <- sits_classify_raster(file = "./L8_MOD_222-068-class", raster.tb,
                                        ml_model = dl_model, memsize = 6, multicores = 1)

sits_plot_raster(raster_class.tb[1,], title = "LANDSAT-MODIS-222-068-2015-2016")
