# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data

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

data("samples_matogrosso_mod13q1")

samples_ndvi_evi <- sits_select(
    data  = samples_matogrosso_mod13q1,
    bands = c("NDVI", "EVI")
)

# build the classification model
rfor_model <- sits_train(samples_ndvi_evi,
    ml_method = sits_rfor()
)

# Read ndvi and evi data from the sitsdata package
# create a data cube to be classified
# Cube MOD13Q1 images from the Sinop region in Mato Grosso (Brazil)
data_dir <- system.file("extdata/sinop", package = "sitsdata")
sinop <- sits_cube(
    source     = "BDC",
    collection = "MOD13Q1-6",
    data_dir   = data_dir,
    delim      = "_",
    parse_info = c("X1", "tile", "band", "date")
)

# classify the raster image
sinop_probs <- sits_classify(
    data       = sinop,
    ml_model   = rfor_model,
    memsize    = 8,
    multicores = 2
)

# smoothen with bayesian filter
sinop_bayes <- sits_smooth(
    cube       = sinop_probs,
    type       = "bayes",
    memsize    = 8,
    multicores = 2
)
# calculate uncertainty
sinop_uncert <- sits_uncertainty(
    cube       = sinop_bayes,
    type       = "entropy",
    memsize    = 8,
    multicores = 2
)
# calculate uncertainty
sinop_uncert_m <- sits_uncertainty(
    cube       = sinop_bayes,
    type       = "least",
    memsize    = 8,
    multicores = 2
)
# calculate uncertainty
sinop_uncert_margin <- sits_uncertainty(
    cube       = sinop_bayes,
    type       = "margin",
    memsize    = 8,
    multicores = 2
)
# label the classified image
sinop_label <- sits_label_classification(
    cube       = sinop_bayes,
    memsize    = 8,
    multicores = 2
)

# plot the smoothened image
plot(sinop_label)
