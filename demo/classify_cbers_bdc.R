# This is a demonstration of classification of using
# images from the Brazil Data Cube with the STAC catalogue
# The input is a CBERS-4 data set covering an area in the Cerrado
# of the state of Bahia (Brazil)
# with two bands (NDVI and EVI)
library(sits)

if (!requireNamespace("sitsdata", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("e-sensing/sitsdata")
}

# load the sitsdata library
library(sitsdata)

# load the samples
data("samples_cerrado_cbers")

# set up the bands
bands <- c("NDVI", "EVI")

# select only the samples for the chosen bands
cbers_samples_2bands <- sits_select(
    data = samples_cerrado_cbers,
    bands = bands
)

# define the start and end dates for selection the images
timeline_samples <- sits_timeline(cbers_samples_2bands)
start_date <- timeline_samples[1]
end_date <- timeline_samples[length(timeline_samples)]

# define a CBERS data cube using the Brazil Data Cube
cbers_cube <- sits_cube(
    source     = "BDC",
    collection = "CB4_64_16D_STK-1",
    bands      = bands,
    tiles      = "022024",
    start_date = start_date,
    end_date   = end_date
)

# train an RFOR model
rfor_model <- sits_train(
    data      = cbers_samples_2bands,
    ml_method = sits_rfor()
)

# classify the data (remember to set the appropriate memory size)
cbers_probs <- sits_classify(
    data       = cbers_cube,
    ml_model   = rfor_model,
    output_dir = "/Users/gilbertocamara/cbers/",
    memsize    = 16,
    multicores = 4,
    verbose = TRUE,
    progress = TRUE
)

# plot the classification result
plot(cbers_probs)

# post process probabilities map with bayesian smoothing
cbers_bayes <- sits_smooth(
    cube       = cbers_probs,
    type       = "bayes",
    output_dir = "/Users/gilbertocamara/cbers/",
    memsize    = 16,
    multicores = 4,
    verbose = TRUE,
    progress = TRUE
)
# plot the classification result after smoothing
plot(cbers_bayes)

# label the smoothed image
cbers_lbayes <- sits_label_classification(
    cube       = cbers_bayes,
    output_dir = "/Users/gilbertocamara/cbers/",
    memsize    = 16,
    multicores = 4
)

# plot the labelled image
plot(cbers_lbayes)

#view the classification results together with the original maps
sits_view(x     = cbers_cube,
          red   = "EVI",
          green = "NDVI",
          blue  = "EVI",
          times  = c(1, 23),
          class_cube = cbers_lbayes
)
