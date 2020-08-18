# This is a demonstration of classification of using
# stack data (images organised by tiles)
# The input is a CBERS-4 data set covering an area in the Cerrado
# of the state of Bahia (Brazil)
# with two bands (NDVI and EVI)
library(sits)
library(ranger)

if (!requireNamespace("inSitu", quietly = TRUE)) {
	if (!requireNamespace("devtools", quietly = TRUE))
		install.packages("devtools")
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
end_date   <- as.Date(timeline[length(timeline)])
# set up the bands
bands <- c("ndvi", "evi")
# define the local directory to load the images
local_dir <- system.file("extdata/CBERS", package = "inSitu")

# define the local CBERS data cube
cbers_cube <- sits_cube(type = "BDC-TILE",
					   name = "cbers_022024",
					   satellite = "CBERS-4",
					   sensor = "AWFI",
					   bands  = bands,
					   cube   = "CB4_64_16D_STK",
					   tile   = "022024",
					   data_access = "local",
					   start_date  = start_date,
					   end_date    = end_date,
					   .local = local_dir)

# plot the image (first NDVI as red, first EVI as green and blue)
plot(cbers_cube, red = 1, green = 24, blue = 24)

# select the ndvi and evi bands
cbers_samples_2bands <- sits_select_bands(cbers_samples_022024, ndvi, evi)
# train a random forest model
rfor_model <- sits_train(cbers_samples_2bands, sits_rfor())

# classify the data (remember to set the appropriate memory size)
cbers_probs <- sits_classify(cbers, rfor_model, memsize = 24)

# plot the probabilities for each class
plot(cbers_probs)

cbers_label <- sits_label_classification(cbers_probs, smoothing = "bayesian")

plot(cbers_label)
