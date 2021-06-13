library(sits)
library(keras)

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("e-sensing/sitsdata")
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
    data      = samples_ndvi_evi,
    ml_method = sits_mlp()
)

sits_keras_diagnostics(dl_model)

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
    parse_info = c("X1", "X2", "tile", "band", "date")
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
    type       = "bayes",
    output_dir = tempdir()
)

# label the classified image
sinop_label <- sits_label_classification(
    cube = sinop_bayes,
    output_dir = tempdir()
)

# plot the smoothed image
plot(sinop_bayes)

# plot the classified image
plot(sinop_label)


