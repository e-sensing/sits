# satellite image time series package (SITS)
# example of the classification of a time series
library(sits)

# In this example, we are going to train a ML model
# and then will classify a point

# use a sample with the bands "ndvi", "evi", "nir", and "mir"
# select a random forest model
rfor_model <- sits_train(samples_mt_4bands, ml_method = sits_rfor())

# Retrieve a time series
data(point_mt_6bands)

# select the bands "ndvi", "evi", "nir", and "mir"
point.tb <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI", "NIR", "MIR"))

# classify the point
class.tb <- sits_classify(point.tb, rfor_model)

# plot the classification
plot(class.tb, bands = c("ndvi", "evi", "mir"))
