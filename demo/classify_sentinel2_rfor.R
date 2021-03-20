# This is a demonstration of classification of a Sentinel-2 image
# tile T20LKP in Rondonia Brazil from data on AWS

# Users need to provide AWS credentials using environment variables.
# Sys.setenv(
#    "AWS_ACCESS_KEY_ID"     = <your_access_key>,
#    "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,
#    "AWS_DEFAULT_REGION"    = <your AWS region>,
#    "AWS_ENDPOINT" = "sentinel-s2-l2a.s3.amazonaws.com",
#    "AWS_REQUEST_PAYER"     = "requester"
# )

# Sentinel-2/2A level 2A files in AWS are organized by sensor
# resolution. The AWS bands in 10m resolution are "B02", "B03", "B04", and "B08".
# The 20m bands are "B02", "B03", "B04", "B05", "B06", "BO7", B08", "B8A", "B11", and "B12".
# All 12 bands are available at 60m resolution. For creating data cubes from
# Sentinel-2/2A, users have to specify the `s2_resolution` parameter.

library(sits)
library(profvis)
# define the cube
s2_cube <- sits_cube(source = "AWS",
                     name = "T20LKP_2018_2019",
                     collection = "sentinel-s2-l2a",
                     tiles = c("20LKP", "20LLP"),
                     bands = c("B03", "B04", "B08", "B11", "B12", "SCL"),
                     start_date = as.Date("2018-07-12"),
                     end_date = as.Date("2019-07-28"),
                     s2_resolution = 60
)
# s2_regular_cube <- sits_regularize(
#     cube = s2_cube,
#     name = "T20LKP_2018_2019_regular",
#     dir_images = "~/sentinel2/images",
#     period = "P16D",
#     agg_method = "median",
#     cloud_mask = TRUE
# )


csv_file <- system.file("/extdata/samples/samples_amazonia_sentinel2.csv", package = "sits")

samples_S2_T20LKP_2018_2019 <- sits_get_data(s2_cube, file = csv_file, .n_pts_csv = 30)

# plot the first date as a SWIR composite (B11, B08, B04)
# map_1 <- plot(s2_cube, red = "B11", green = "B08", blue = "B04", time = 1)

# plot the last date as a SWIR composite (B11, B08, B04)
# map_2 <- plot(s2_cube, red = "B11", green = "B08", blue = "B04", time = 36)

# train a random forest model
samples_s2_3bands <- sits_select(samples_S2_T20LKP_2018_2019,
                                 bands = c("B03", "B08", "B11"))

# train the deep learning model
dl_model <- sits_train(samples_s2_3bands,
                       ml_method = sits_deeplearning(
                           layers = c(512, 512, 512, 512, 512, 512),
                           activation = "elu",
                           dropout_rates = c(0.60, 0.55, 0.50, 0.45, 0.40, 0.35),
                           epochs = 400,
                           batch_size = 128,
                           validation_split = 0.1
                       )
)

# classify the cube using an rfor model
s2_probs <- sits_classify(s2_cube,
                          dl_model,
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
