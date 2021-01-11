# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

# select the bands for classification
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
      }
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

# select the bands for classification
samples_ndvi_evi <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))

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
sinop <- sits_cube(
    type = "BRICK",
    satellite = "TERRA",
    sensor = "MODIS",
    name = "Sinop",
    timeline = timeline_2013_2014,
    bands = c("ndvi", "evi"),
    files = files
)

# classify the raster image
sinop_probs <- sits_classify(sinop,
    ml_model = svm_model,
    memsize = 2,
    multicores = 1,
    output_dir = tempdir()
)

# smoothen with bayesian filter
sinop_bayes <- sits_smooth(sinop_probs, output_dir = tempdir())
# label the classified image
sinop_label <- sits_label_classification(sinop_bayes, output_dir = tempdir())

# plot the smoothened image
plot(sinop_label, time = 1, title = "Sinop-smooth")
