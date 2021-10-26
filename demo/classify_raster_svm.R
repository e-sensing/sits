# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
    stop(paste0("Please install package sitsdata\n",
                "Please call devtools::install_github('e-sensing/sitsdata')"),
         call. = FALSE)
}

# load the sitsdata library
library(sitsdata)

data("samples_matogrosso_mod13q1")

samples_ndvi_evi <- sits_select(
    data  = samples_matogrosso_mod13q1,
    bands = c("NDVI", "EVI")
)

# build the classification model
svm_model <- sits_train(
    data      = samples_ndvi_evi,
    ml_method = sits_svm()
)

# Read ndvi and evi data from the sitsdata package
# create a data cube to be classified
# Cube is composed of MOD13Q1 images from the Sinop region in Mato Grosso (Brazil)
data_dir <- system.file("extdata/sinop", package = "sitsdata")
sinop <- sits_cube(
    source     = "LOCAL",
    name       = "sinop-2014",
    origin     = "BDC",
    collection = "MOD13Q1-6",
    data_dir   = data_dir,
    delim      = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
)

# classify the raster image
sinop_probs <- sits_classify(
    data       = sinop,
    ml_model   = svm_model,
    memsize    = 6,
    multicores = 2,
    output_dir = tempdir()
)

# smoothen with bayesian filter
sinop_bayes <- sits_smooth(
    cube       = sinop_probs,
    type       = "bayes",
    output_dir = tempdir()
)

# label the classified image
sinop_label <- sits_label_classification(
    cube       = sinop_bayes,
    output_dir = tempdir()
)

# plot the smoothened image
plot(sinop_label)
