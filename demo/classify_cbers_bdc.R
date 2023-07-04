# This is a demonstration of classification of using
# images from the Brazil Data Cube with the STAC catalogue
# The input is a CBERS-4 data set covering an area in the Cerrado
# of the state of Bahia (Brazil)
# with two bands (NDVI and EVI)
library(sits)

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
    stop(
        paste0(
            "Please install package sitsdata\n",
            "Please call devtools::install_github('e-sensing/sitsdata')"
        ),
        call. = FALSE
    )
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
    collection = "CB4-16D-2",
    bands      = bands,
    tiles      = "007004",
    start_date = start_date,
    end_date   = end_date
)

# train an RFOR model
rfor_model <- sits_train(
    samples   = cbers_samples_2bands,
    ml_method = sits_rfor()
)

# classify the data (remember to set the appropriate memory size)
cbers_probs <- sits_classify(
    data = cbers_cube,
    ml_model = rfor_model,
    memsize = 16,
    multicores = 4,
    output_dir = tempdir(),
    verbose = TRUE,
    progress = TRUE
)

# plot the classification result
plot(cbers_probs)

# post process probabilities map with bayesian smoothing
cbers_bayes <- sits_smooth(
    cube = cbers_probs,
    memsize = 16,
    multicores = 4,
    output_dir = tempdir()
)
# plot the classification result after smoothing
plot(cbers_bayes)

# label the smoothed image
cbers_lbayes <- sits_label_classification(
    cube       = cbers_bayes,
    memsize    = 16,
    multicores = 4,
    output_dir = tempdir()
)

# plot the labelled image
plot(cbers_lbayes)

timeline <- sits_timeline(cbers_cube)

# view the classification results together with the original maps
sits_view(
    x = cbers_cube,
    red = "EVI",
    green = "NDVI",
    blue = "EVI",
    dates = c(timeline[1], timeline[length(timeline)]),
    class_cube = cbers_lbayes
)
