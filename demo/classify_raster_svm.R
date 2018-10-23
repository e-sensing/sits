# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

# Read ndvi and evi data from dropbox
# these are the symbolic links for the files at dropbox
ndvi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/Sinop_ndvi.tif")
evi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/Sinop_evi.tif")

# define the timeline
data(timeline_modis_392)

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "Sinop",
                           timeline = timeline_modis_392,
                           bands = c("ndvi", "evi"),
                           files = c(ndvi_file, evi_file))

#select the bands for classification
samples.tb <- sits_select_bands(samples_MT_9classes, ndvi, evi)

# build the classification model
svm_model <- sits_train(samples.tb, ml_method = sits_svm())

# classify the raster image
raster_class.tb <- sits_classify_raster(file = "./sinop-class", raster.tb,
                                        ml_model = svm_model, memsize = 4, multicores = 2)

# plot the first classified image
sits_plot_raster(raster_class.tb[1,], title = "SINOP MT - 2000/2001")


