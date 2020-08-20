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
cbers_samples_022024 <- sits_rename(cbers_samples_022024,
				c("BAND13", "evi", "BAND14", "ndvi", "BAND16",  "BAND15"))
cbers_samples_022024 <- sits_select_bands(cbers_samples_022024, ndvi, evi)
bands <- sits_bands(cbers_samples_022024)
# define the local directory to load the images
local_dir <- "/gfs/Repository/Mosaic"

# define the local CBERS data cube
cbers_cube <- sits_cube(type = "BDC_TILE",
                        name = "cbers_022024",
                        satellite = "CBERS-4",
                        sensor = "AWFI",
                        bands  = bands,
                        cube   = "CB4_64_16D_STK_v1",
                        tile   = "022024",
                        data_access = "local",
                        start_date  = start_date,
                        end_date    = end_date,
                        .local = local_dir)

# plot the image (first and last instances) - save the mapview for the
# future
mapview1 <- plot(cbers_cube, red = "evi", green = "ndvi", blue = "evi", time = 1)
# show
mapview1
# plot the last image
plot(cbers_cube, red = "evi", green = "ndvi", blue = "evi", time = 23)

# train a random forest model
rfor_model <- sits_train(cbers_samples_022024, sits_rfor())

# classify the data (remember to set the appropriate memory size)
cbers_probs <- sits_classify(cbers_cube, rfor_model, memsize = 40)

# plot the probabilities for each class
plot(cbers_probs)

cbers_label <- sits_label_classification(cbers_probs, smoothing = "bayesian")

plot(cbers_label)

sits_db_connect("./cubes.sql")
conn <- sits_db_connect("./cubes.sql")
sits_db_write(conn, "cbers_probs", cbers_probs)
sits_db_write(conn, "cbers_label", cbers_label)
sits_db_write(conn, "cbers_cube", cbers_cube)
