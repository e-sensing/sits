# This is a demonstration of classification of using
# images from the Brazil Data Cube with the STAC catalogue
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
cbers_samples_022024 <- sits_select(cbers_samples_022024,
                                    bands = c("NDVI", "EVI"))
bands <- sits_bands(cbers_samples_022024)

# define a CBERS data cube using the Brazil Data Cube
cbers_cube <- sits_cube(
    type = "BDC",
    name = "cbers_022024",
    satellite = "CBERS-4",
    sensor = "AWFI",
    bands = bands,
    tiles = "022024",
    collection = "CB4_64_16D_STK-1",
    start_date = start_date,
    end_date = end_date
)

# region of interest
roi <- c(
  "xmin" = 5970958, "xmax" = 6034958,
  "ymin" = 9876672, "ymax" = 9940672
)

# plot the image (last instances) - save the mapview for the
# future
map1 <- plot(cbers_cube, red = "EVI", green = "NDVI", blue = "EVI", time = 23)

# train an SVM model
svm_model <- sits_train(cbers_samples_022024, sits_svm())

# classify the data (remember to set the appropriate memory size)
cbers_probs <- sits_classify(cbers_cube,
                             svm_model,
                             roi = roi,
                             output_dir = tempdir(),
                             memsize = 6,
                             multicores = 2
)
# plot the classification result
plot(cbers_probs)
# label each pixel with the highest probability
cbers_label <- sits_label_classification(cbers_probs, output_dir = tempdir())
# plot the labelled image
plot(cbers_label, map = map1)
# post process probabilities map with bayesian smoothing
cbers_bayes <- sits_smooth(cbers_probs, output_dir = tempdir())
# plot the new probs
plot(cbers_bayes)
# label the smoothed image
cbers_lbayes <- sits_label_classification(cbers_bayes, output_dir = tempdir())
# plot the labeled image with bayesian smoothing
plot(cbers_lbayes, map = map1)
