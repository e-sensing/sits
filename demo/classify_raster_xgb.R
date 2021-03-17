# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data
library(sits)
library(randomForest)

if (!requireNamespace("sitsdata", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
      }
    devtools::install_github("e-sensing/sitsdata")
}
library(sitsdata)

# select the bands for classification
data(br_mt_1_8K_9classes_6bands)
samples_ndvi_evi <- sits_select(br_mt_1_8K_9classes_6bands, bands = c("EVI", "NDVI"))

# build the classification model
xgb_model <- sits_train(samples_ndvi_evi, ml_method = sits_xgboost())

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
                             ml_model = xgb_model,
                             memsize = 8,
                             multicores = 2,
                             output_dir = tempdir())
plot(sinop_probs)

# smooth the result with a Bayesian filter
sinop_bayes <- sits_smooth(type = "bayes",
                           sinop_probs,
                           output_dir = tempdir())
plot(sinop_bayes)

# smooth the result with a Gaussian filter
sinop_gauss <- sits_smooth(type = "gaussian",
                           sinop_probs,
                           output_dir = tempdir())
plot(sinop_gauss)

# smooth the result with a bilinear filter
sinop_bilin <- sits_smooth(type = "bilinear",
                           sinop_probs,
                           output_dir = tempdir())
plot(sinop_bilin)

sinop_label <- sits_label_classification(sinop_bayes,
                                         output_dir = tempdir())

# plot the smoothened image
map_1 <- plot(sinop, red = "evi", green = "ndvi", blue = "evi", time = 23)
plot(sinop_label, map = map_1)
