# This is a demonstration of classification of a two cube areas
# The cube images are a MODIS data set of 480x480 pixels throughout 17 years

# create a raster metadata file based on the information about the files
cov.tb <- sits_coverage(service = "EOCUBES", name = "MOD13Q1/006",
                        bands = c("ndvi", "evi"))

# define the timeline
data(timeline_modis_392)

#select the bands for classification
samples.tb <- sits_select_bands(samples_mt_9classes, ndvi, evi)

# build the classification model
svm_model <- sits_train(samples.tb, ml_method = sits_svm())

# classify the raster image
raster_class.tb <- sits_classify_raster(file = "./test-class", cov.tb,
                                        ml_model = svm_model, memsize = 4, multicores = 2)

# plot the first classified image
sits_plot_raster(raster_class.tb[19,], title = "2000/2001")
