# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

if (!requireNamespace("sitsdata", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("e-sensing/sitsdata")
}
# load the sitsdata library
library(sitsdata)

data(br_mt_1_8K_9classes_6bands)
samples_ndvi_evi <- sits_select(br_mt_1_8K_9classes_6bands, bands = c("NDVI", "EVI"))

# build the classification model
svm_model <- sits_train(samples_ndvi_evi, ml_method = sits_svm())

# Read ndvi and evi data from the sitsdata package
# create a data cube to be classified
# Cube is composed of MOD13Q1 images from the Sinop region in Mato Grosso (Brazil)
data_dir <- system.file("extdata/sinop", package = "sitsdata")
sinop <- sits_cube(
    source = "LOCAL",
    name = "sinop-2014",
    satellite = "TERRA",
    sensor = "MODIS",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "band", "date")
)


# classify the raster image
sinop_probs <- sits_classify(sinop,
    ml_model = svm_model,
    memsize = 6,
    multicores = 2,
    output_dir = tempdir()
)

# smoothen with bayesian filter
sinop_bayes <- sits_smooth(sinop_probs, output_dir = tempdir())
# label the classified image
sinop_label <- sits_label_classification(sinop_bayes, output_dir = tempdir())

# plot the smoothened image
plot(sinop_label)
