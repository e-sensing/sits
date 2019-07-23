# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data
library(sits)
library(ranger)
# Read ndvi and evi data from dropbox
# these are the symbolic links for the files at dropbox
ndvi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/Sinop_ndvi.tif")
evi_file <- paste0("/vsicurl/https://s3-sa-east-1.amazonaws.com/landsat-modis/Sinop_evi.tif")

# define the timeline
data(timeline_modis_392)

# create a raster metadata file based on the information about the files
raster.tb <- sits_cube(service = "RASTER", name = "Sinop",
                           timeline = timeline_modis_392,
                           bands = c("ndvi", "evi"),
                           files = c(ndvi_file, evi_file))

#select the bands for classification
samples.tb <- sits_select_bands(samples_mt_9classes, ndvi, evi)

# build the classification model
rfor_model <- sits_train(samples.tb, ml_method = sits_rfor(num.trees = 2000))

# classify the raster image
raster_class.tb <- sits_classify(raster.tb, rfor_model, memsize = 4, multicores = 2,
                                 out_prefix = "./sinop-class")

# plot the first classified image
sits_plot_raster(raster_class.tb, time = 1, title = "SINOP MT - 2000/2001")


