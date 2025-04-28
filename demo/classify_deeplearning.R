library(sits)

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
    stop("Please install package sitsdata\n",
        "Please call devtools::install_github('e-sensing/sitsdata')",
        call. = FALSE
    )
}

# load the sitsdata library
library(sitsdata)

# load a time series samples for the Mato Grosso region
data("samples_matogrosso_mod13q1")

samples_ndvi_evi <- sits_select(
    data = samples_matogrosso_mod13q1,
    bands = c("NDVI", "EVI")
)

# train a deep learning model using multi-layer perceptrons
dl_model <- sits_train(
    samples      = samples_ndvi_evi,
    ml_method    = sits_tempcnn()
)

# create a data cube to be classified
# Cube MOD13Q1 images from the Sinop region in Mato Grosso (Brazil)
data_dir <- system.file("extdata/sinop", package = "sitsdata")
sinop <- sits_cube(
    source = "BDC",
    collection = "MOD13Q1-6.1",
    data_dir = data_dir
)

# classify the raster image
sinop_probs <- sits_classify(
    data       = sinop,
    ml_model   = dl_model,
    memsize    = 12,
    multicores = 2,
    output_dir = tempdir()
)

# smoothen with bayesian filter
sinop_bayes <- sits_smooth(
    cube       = sinop_probs,
    output_dir = tempdir()
)

# label the classified image
sinop_label <- sits_label_classification(
    cube = sinop_bayes,
    output_dir = tempdir()
)

# plot the classified image
plot(sinop_label)
