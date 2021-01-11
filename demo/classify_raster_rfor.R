# This is a demonstration of classification of a raster area
# The raster image is a MODIS data set covering the municipality of Sinop
# with two bands (NDVI and EVI) using MODIS collection 5 data
library(sits)
library(ranger)

if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
      }
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

# select the bands for classification
samples <- inSitu::br_mt_1_8K_9classes_6bands
samples_ndvi_evi <- sits_select(samples, bands = c("EVI", "NDVI"))

# build the classification model
rfor_model <- sits_train(samples_ndvi_evi, ml_method = sits_rfor(num_trees = 2000))

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
    bands = c("NDVI", "EVI"),
    files = files
)

# classify the raster image
sinop_probs <- sits_classify(sinop,
                             ml_model = rfor_model,
                             memsize = 24,
                             multicores = 2,
                             output_dir = tempdir()
                             )
plot(sinop_probs)

# smooth the result with a bayesian filter
sinop_bayes <- sits_smooth(type = "bayes", sinop_probs, output_dir = tempdir())

sinop_label <- sits_label_classification(sinop_bayes,
                                         output_dir = tempdir()
)

map_1 <- plot(sinop, red = "evi", green = "ndvi", blue = "evi", time = 23)
# plot the smoothened image
plot(sinop_label, map = map_1, time = 1, title = "Sinop-Bayes")
