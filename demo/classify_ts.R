# satellite image time series package (SITS)
# example of the classification of a time series
library(sits)

# In this example, we are going to train a ML model
# and then will classify a point

# use a sample with the bands "ndvi", "evi", "nir", and "mir"
# select a random forest model
rfor_model <- sits_train(
    samples   = samples_modis_ndvi,
    ml_method = sits_rfor()
)

# Retrieve a time series
data("point_mt_6bands")

# select the bands "ndvi", "evi", "nir", and "mir"
point_tb <- sits_select(
    data  = point_mt_6bands,
    bands = "NDVI"
)

# classify the point
class_tb <- sits_classify(
    data     = point_tb,
    ml_model = rfor_model
)

# plot the classification
plot(class_tb, bands = "NDVI")
