# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data
library(sits)

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
    stop(paste0(
        "Please install package sitsdata\n",
        "Please call devtools::install_github('e-sensing/sitsdata')"
    ),
    call. = FALSE
    )
}

# load the sitsdata library
library(sitsdata)

# select the bands for classification
data("samples_matogrosso_mod13q1")

samples_ndvi_evi <- sits_select(
    data  = samples_matogrosso_mod13q1,
    bands = c("EVI", "NDVI")
)

# build the classification model
xgb_model <- sits_train(
    data      = samples_ndvi_evi,
    ml_method = sits_xgboost()
)

# create a data cube to be classified
# Cube is composed of MOD13Q1 images from the Sinop region in Mato Grosso (Brazil)
data_dir <- system.file("extdata/sinop", package = "sitsdata")
sinop <- sits_cube(
    source     = "BDC",
    collection = "MOD13Q1-6",
    data_dir   = data_dir,
    delim      = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
)

# classify the raster image
sinop_probs <- sits_classify(
    data       = sinop,
    ml_model   = xgb_model,
    memsize    = 8,
    multicores = 2,
    output_dir = tempdir()
)

# smooth the result with a Bayesian filter
sinop_bayes <- sits_smooth(
    cube       = sinop_probs,
    type       = "bayes",
    output_dir = tempdir()
)

sinop_label <- sits_label_classification(
    cube       = sinop_bayes,
    output_dir = tempdir()
)

# plot the probabilities
plot(sinop_probs)

# plot the smoothened images
plot(sinop_bayes)

# plot the classified map
plot(sinop_label)

map_1 <- sits_view(sinop, red = "EVI", green = "NDVI", blue = "EVI", time = 23)

sits_view(sinop_label, map = map_1)
