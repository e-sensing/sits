# This is a demonstration of classification of a Sentinel-2 image
# tile T20LKP in Rondonia Brazil
library(sits)
library(ranger)

if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
      }
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)
# load the samples for the Sentinel data set
data(samples_S2_T20LKP_2018_2019)
# get the timeline
timeline <- sits_timeline(samples_S2_T20LKP_2018_2019)
start_date <- as.Date(timeline[1])
end_date <- as.Date(timeline[length(timeline)])

# get the files and the bands
s2_dir <- system.file("extdata/Sentinel/T20LKP", package = "inSitu")
s2_bricks <- list.files(s2_dir)
s2_files <- paste0(s2_dir, "/", s2_bricks)
s2_bands <- c("B03", "B04", "B08", "B11")

# define the cube
s2_cube <- sits_cube(
    type = "BRICK",
    name = "T20LKP",
    satellite = "SENTINEL-2",
    sensor = "MSI",
    timeline = timeline,
    bands = s2_bands,
    files = s2_files
)

# plot the first date as a SWIR composite (B11, B08, B04)
# map_1 <- plot(s2_cube, red = "B11", green = "B08", blue = "B04", time = 1)

# plot the last date as a SWIR composite (B11, B08, B04)
# map_2 <- plot(s2_cube, red = "B11", green = "B08", blue = "B04", time = 36)

# train a random forest model
samples_s2_3bands <- sits_select(samples_S2_T20LKP_2018_2019,
                                 bands = c("B03", "B08", "B11"))
rfor_model <- sits_train(samples_s2_3bands, sits_rfor())

# classify the cube using an rfor model
s2_probs <- sits_classify(s2_cube,
                          rfor_model,
                          memsize = 24,
                          multicores = 4,
                          output_dir = tempdir()
)
# plot the probabilities
plot(s2_probs)

s2_bayes <- sits_smooth(s2_probs, output_dir = tempdir())

s2_label <- sits_label_classification(s2_bayes, output_dir = tempdir())

# plot the labelled images
plot(s2_label)
