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

# region of interest
roi <- c(
    "lat_min" = -12.06618108, "lat_max" = -11.06869077,
    "lon_min" = -46.67404746, "lon_max" = -45.09037986
)

# define the local CBERS data cube
cbers <- sits_cube(
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


# plot the image (last instances) - save the mapview for the
# future
map1 <- plot(cbers, red = "EVI", green = "NDVI", blue = "EVI", time = 23)

# train an XGB model
svm_model <- sits_train(cbers, sits_svm())

# classify the data (remember to set the appropriate memory size)
cbers_probs <- sits_classify(cbers_cube,
                             svm_model,
                             roi = roi,
                             output_dir = tempdir(),
                             memsize = 24,
                             multicores = 4
)
plot(cbers_probs)

cbers_label <- sits_label_classification(cbers_probs, smoothing = "bayesian",
                                         output_dir = tempdir())

plot(cbers_label, map = map1)
