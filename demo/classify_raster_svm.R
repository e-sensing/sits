# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

#select the bands for classification
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

#select the bands for classification
samples <- inSitu::br_mt_1_8K_9classes_6bands
samples_ndvi_evi <- sits_select_bands(samples, ndvi, evi)

# build the classification model
svm_model <- sits_train(samples_ndvi_evi, ml_method = sits_svm())

# Read ndvi and evi data from the inSitu package
# select the bands "ndvi", "evi"
# select the bands "ndvi", "evi" from the "inSitu" package
evi_file <- system.file("extdata/Sinop", "Sinop_evi_2014.tif", package = "inSitu")
ndvi_file <- system.file("extdata/Sinop", "Sinop_ndvi_2014.tif", package = "inSitu")

files <- c(ndvi_file, evi_file)
# define the timeline
time_file <- system.file("extdata/Sinop", "timeline_2014.txt", package = "inSitu")
timeline_2013_2014 <- scan(time_file, character())

# create a raster metadata file based on the information about the files
raster.tb <- sits_cube(name = "Sinop",  timeline = timeline_2013_2014, bands = c("ndvi", "evi"), files = files)


# classify the raster image
raster_class.tb <- sits_classify(raster.tb, ml_model = svm_model, memsize = 4, multicores = 2,
                                 out_prefix = "./Sinop-class")

# plot the raster image
sits_plot_raster(raster_class.tb, time = 1, title = "Sinop")

# smooth the result with a bayesian filter
raster_class_bayes.tb <- sits::sits_bayes_smooth(cube = raster_class.tb)

# plot the smoothened image
sits_plot_raster(raster_class_bayes.tb, time = 1, title = "Sinop")

