# This is a demonstration of classification of a two cube areas
# The cube images are a MODIS data set of 480x480 pixels throughout 17 years

#select the bands for classification
samples_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)


# build the classification model
svm_model <- sits_train(samples_ndvi, ml_method = sits_svm())

# create a raster metadata file based on the information about the files
cube_modis <- sits_cube(service = "EOCUBES",
                        name = "MOD13Q1/006",
                        bands = "ndvi",
                        tiles_names = "h13v10",
                        from = "2016-09-01",
                        to = "2017-08-31")

# classify the raster image
raster_probs <- sits_classify(cube_modis, ml_model = svm_model, memsize = 4, multicores = 2,
                                 output_dir = "./h13v10-class")

# smooth the images
raster_smooth <- sits_label_classification(raster_probs, smoothing = "bayesian")

# plot the first classified image
plot(raster_smooth, time = 1, title = "Classification 2016/2017")
