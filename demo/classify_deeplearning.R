library(sits)
library(keras)
# install_keras()

if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

samples_ndvi_evi <- sits_select_bands(samples_mt_4bands, ndvi, evi)

# train the deep learning model
dl_model <-  sits_train(samples_ndvi_evi,
                        ml_method = sits_deeplearning(
                             layers           = c(512, 512, 512),
                             activation       = 'relu',
                             dropout_rates    = c(0.50, 0.40, 0.35),
                             epochs = 100,
                             batch_size = 128,
                             validation_split = 0.2))

sits_keras_diagnostics(dl_model)

# select the bands "ndvi", "evi" from the "inSitu" package
evi_file <- system.file("extdata/Sinop", "Sinop_evi_2014.tif", package = "inSitu")
ndvi_file <- system.file("extdata/Sinop", "Sinop_ndvi_2014.tif", package = "inSitu")

files <- c(ndvi_file, evi_file)
# define the timeline
time_file <- system.file("extdata/Sinop", "timeline_2014.txt", package = "inSitu")
timeline_2013_2014 <- scan(time_file, character())

# create a raster metadata file based on the information about the files
sinop <- sits_cube(name = "Sinop",  timeline = timeline_2013_2014, bands = c("ndvi", "evi"), files = files)

# classify the raster image
sinop_probs <- sits_classify(sinop, ml_model = dl_model, memsize = 4, multicores = 2)

# label the classified image
sinop_label <- sits_label_classification(sinop_probs)

# plot the raster image
plot(sinop_label, time = 1, title = "Sinop-2013-2014")

# smooth the result with a bayesian filter
sinop_bayes <- sits_label_classification(sinop_probs, smoothing = "bayesian")

# plot the smoothened image
plot(sinop_bayes, time = 1, title = "Sinop-smooth")
