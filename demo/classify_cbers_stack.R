# This is a demonstration of classification of using
# stack data (images organised by tiles)
# The input is a CBERS-4 data set covering an area in the Cerrado
# of the state of Bahia (Brazil)
# with two bands (NDVI and EVI)
library(sits)
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
      }
    devtools::install_github("e-sensing/inSitu")
}
# load the inSitu library
library(inSitu)
# load the samples
data("cbers_samples_022024")
# obtain the timeline
timeline <- sits_timeline(cbers_samples_022024)
# get the start and end dates
start_date <- as.Date(timeline[1])
end_date <- as.Date(timeline[length(timeline)])
# set up the bands
bands <- c("NDVI", "EVI")
# define the local directory to load the images
local_dir <- system.file("extdata/CBERS/CB4_64_16D_STK/022024", package = "inSitu")

# define the local CBERS data cube
cbers_cube <- sits_cube(
    type = "STACK",
    name = "cbers_022024",
    satellite = "CBERS-4",
    sensor = "AWFI",
    resolution = "64m",
    data_dir = local_dir,
    parse_info = c("X1", "X2", "X3", "X4", "X5", "date", "X7", "band")
)

# plot the image (first and last instances)
plot(cbers_cube, red = "EVI", green = "NDVI", blue = "EVI", time = 1)
plot(cbers_cube, red = "EVI", green = "NDVI", blue = "EVI", time = 23)
# select the ndvi and evi bands
cbers_samples_2bands <- sits_select(cbers_samples_022024, bands = c("NDVI", "EVI"))
# train a random forest model
rfor_model <- sits_train(cbers_samples_2bands, sits_rfor())

# classify the data (remember to set the appropriate memory size)
cbers_probs <- sits_classify(cbers_cube,
                             rfor_model,
                             memsize = 6,
                             maxcores = 2,
                             output_dir = tempdir())

# plot the probabilities for each class
plot(cbers_probs)

# smoothen the probabibilities with bayesian estimator
cbers_bayes <- sits_smooth(cbers_probs, output_dir = tempdir())
# label the resulting probs maps
cbers_label <- sits_label_classification(cbers_bayes, output_dir = tempdir())

plot(cbers_label)
