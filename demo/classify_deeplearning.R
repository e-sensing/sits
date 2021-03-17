library(sits)
library(keras)

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("e-sensing/sitsdata")
}
library(sitsdata)
# load a time series samples for the Mato Grosso region
data(br_mt_1_8K_9classes_6bands)

samples_ndvi_evi <- sits_select(br_mt_1_8K_9classes_6bands, bands = c("NDVI", "EVI"))

# train the deep learning model
dl_model <- sits_train(samples_ndvi_evi,
    ml_method = sits_deeplearning(
        layers = c(512, 512, 512),
        activation = "relu",
        dropout_rates = c(0.50, 0.40, 0.35),
        epochs = 100,
        batch_size = 128,
        validation_split = 0.2
    )
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
    parse_info = c("X1", "X2", "band", "date")
)
# classify the raster image
sinop_probs <- sits_classify(sinop,
                             ml_model = dl_model,
                             memsize = 12,
                             multicores = 2,
                             output_dir = tempdir()
)

# smoothen with bayesian filter
sinop_bayes <- sits_smooth(sinop_probs, output_dir = tempdir())
# plot the smoothed image
plot(sinop_bayes)

# label the classified image
sinop_label <- sits_label_classification(sinop_bayes, output_dir = tempdir())
# plot the classified image
plot(sinop_label)


