# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data
library(sits)
library(ranger)

#select the bands for classification
samples.tb <- sits_select_bands(samples_mt_9classes, ndvi, evi)

# build the classification model
rfor_model <- sits_train(samples.tb, ml_method = sits_rfor(num.trees = 2000))

# Read ndvi and evi data from AWS
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
raster_class.tb <- sits_classify(raster.tb, ml_model = rfor_model, memsize = 4, multicores = 2,
                                 out_prefix = "./Sinop-class")

# plot the raster image
sits_plot_raster(raster_class.tb, time = 1, title = "Sinop")

# smooth the result with a bayesian filter
raster_class_bayes.tb <- sits::sits_bayes_postprocess(raster_class.tb,
                                                      file = "./smooth")

# plot the smoothened image
sits_plot_raster(raster_class_bayes.tb, time = 1, title = "Sinop")

