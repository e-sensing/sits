library(sits)
library(keras)
# install_keras()

# select a file with samples
data("samples_mt_9classes")

samples <- sits_select_bands(samples_mt_9classes, ndvi, evi)

# train the deep learning model
dl_model <-  sits_train(samples,
                        ml_method = sits_deeplearning(
                             units            = c(512, 512, 512),
                             activation       = 'elu',
                             dropout_rates    = c(0.50, 0.40, 0.35),
                             optimizer = keras::optimizer_adam(),
                             epochs = 200,
                             batch_size = 128,
                             validation_split = 0.2))

sits_keras_diagnostics(dl_model)

# select the bands "ndvi", "evi"
evi_file <- paste0("/vsicurl/https://modis-cities.s3-sa-east-1.amazonaws.com/Sinop_MT/evi_2014.tif")
ndvi_file <- paste0("/vsicurl/https://modis-cities.s3-sa-east-1.amazonaws.com/Sinop_MT/ndvi_2014.tif")

files <- c(ndvi_file, evi_file)
# define the timeline
timeline_2013_2014 <- scan("http://modis-cities.s3.amazonaws.com/Sinop_MT/timeline_2014.txt", what = "Date", quiet = TRUE)

# create a raster metadata file based on the information about the files
raster.tb <- sits_cube(service = "RASTER", name = "Sinop",
                       timeline = timeline_2013_2014, bands = c("ndvi", "evi"), files = files)

# classify the raster image
raster_class.tb <- sits_classify(raster.tb, ml_model = dl_model, memsize = 4, multicores = 2,
                                 out_prefix = "./Sinop-class")

# plot the raster image
sits_plot_raster(raster_class.tb, time = 1, title = "Sinop")

# smooth the result with a bayesian filter
raster_class_bayes.tb <- sits::sits_bayes_postprocess(raster_class.tb,
                                                  file = "./smooth")

# plot the smoothened image
sits_plot_raster(raster_class_bayes.tb, time = 1, title = "Sinop")
