# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

# Read ndvi and evi data from dropbox
# these are the symbolic links for the files at dropbox
ndvi_file <- paste0("/vsicurl/https://www.dropbox.com/s/guqsnbpdxtujicr/Sinop_ndvi.tif?raw=1")
evi_file <- paste0("/vsicurl/https://www.dropbox.com/s/caus49bw9romblz/Sinop_evi.tif?raw=1")

# define the timeline
data(timeline_modis_392)

# create a raster metadata file based on the information about the files
raster.tb <- sits_coverage(service = "RASTER", name = "Sinop",
                           timeline = timeline_modis_392,
                           bands = c("ndvi", "evi"),
                           files = c(ndvi_file, evi_file))

#select the bands for classification
samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi"))

# build the classification model
svm_model <- sits_train(samples.tb, ml_method = sits_svm(normalize = TRUE, cost = 10, kernel = "radial", tolerance = 0.001, epsilon = 0.1))

# classify the raster image
raster_class.tb <- sits_classify_raster(file = "./sinop-class", raster.tb, ml_model = svm_model, memsize = 4, multicores = 2)

# plot the first classified image
sits_plot_raster(raster_class.tb[1,], title = "SINOP MT - 2000/2001")


