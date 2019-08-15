# This is a demonstration of classification of a two cube areas
# The cube images are a MODIS data set of 480x480 pixels throughout 17 years

#select the bands for classification
#select the bands for classification
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

#select the bands for classification
samples <- inSitu::br_mt_1_8K_9classes_6bands
samples_ndvi <- sits_select_bands(samples, ndvi)


# build the classification model
svm_model <- sits_train(samples_ndvi, ml_method = sits_svm())

# create a raster metadata file based on the information about the files
cube_modis <- sits_cube(service = "EOCUBES",
                        name = "MOD13Q1/006",
                        bands = "ndvi",
                        tiles_names = "h13v10",
                        from = "2016-01-01")

# classify the raster image
raster_class.tb <- sits_classify(cube_modis, ml_model = svm_model, memsize = 4, multicores = 2,
                                 out_prefix = "./h13v10-class", )

# plot the first classified image
sits_plot_raster(raster_class.tb, time = 1, title = "Classification 2016/2017")

# smooth the images
raster_smooth.tb <- sits_bayes_smooth(cube = raster_class.tb)

# plot the first smoothed classified image
sits_plot_raster(raster_smooth.tb, time = 1, title = "Classification 2016/2017")
