# This is a demonstration of classification of a Sentinel-2 image
# tile T20LKP in Rondonia Brazil
library(sits)
library(ranger)

if (!requireNamespace("inSitu", quietly = TRUE)) {
	if (!requireNamespace("devtools", quietly = TRUE))
		install.packages("devtools")
	devtools::install_github("e-sensing/inSitu")
}
library(inSitu)
# load the samples for the Sentinel data set
data(samples_S2_T20LKP_2018_2019)
# get the timeline
timeline <- sits_timeline(samples_S2_T20LKP_2018_2019)
start_date <- as.Date(timeline[1])
end_date   <- as.Date(timeline[length(timeline)])

# get the files and the bands
s2_dir <- system.file("extdata/Sentinel/T20LKP", package = "inSitu")
s2_bricks <- list.files(s2_dir)
s2_files <- paste0(s2_dir,"/",s2_bricks)
bands <- c("B03", "B04", "B08", "B11")

# define the cube
s2_cube <- sits_cube(type = "BRICK",
					 name = "T20LKP",
					 satellite = "SENTINEL-2",
					 sensor    = "MSI",
					 timeline  = timeline,
					 bands     = bands,
					 files     = s2_files)

# plot the first date as a SWIR composite (B11, B08, B04)
map_1 <- plot(s2_cube, red = "B11", green = "B08", blue = "B04", time = 1)

# plot the last date as a SWIR composite (B11, B08, B04)
map_2 <- plot(s2_cube, red = "B11", green = "B08", blue = "B04", time = 36)

# train a random forest model
samples_s2_4bands <- sits_select_bands(samples_S2_T20LKP_2018_2019, B03, B04, B08, B11)
xgb_model <- sits_train(samples_s2_4bands,
						sits_xgboost(learning_rate     = 0.15,
									 min_split_loss    = 1,
									 max_depth         = 5,
									 min_child_weight  = 1,
									 max_delta_step    = 1,
									 subsample         = 0.8,
									 nfold             = 5,
									 nrounds           = 50,
									 verbose = TRUE))

# classify the cube using an rfor model
s2_probs <- sits_classify(s2_cube, xgb_model, memsize = 12)
# plot the probabilities
plot(s2_probs)

# label the probability cube
s2_label <- sits_label_classification(s2_probs, smoothing = "bayesian")
# plot the labelled images
plot(s2_label, map = map_2)
