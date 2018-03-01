# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# In this example, we are going to train a ML model and then will classify a point retrieved
# from the WTSS server and then a set of samples retrieved from the server
# we will show how to set the classification info

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
data(samples_MT_9classes)

# print the bands available
sits_bands (samples_MT_9classes)

# select the bands "ndvi", "evi", "nir", and "mir"
samples.tb <- sits_select (samples_MT_9classes, bands = c("ndvi","evi","nir","mir"))

# Retrieve a time series
data ("point_MT_6bands")

# select the bands "ndvi", "evi", "nir", and "mir"
point.tb <- sits_select (point_MT_6bands, bands = c("ndvi", "evi", "nir", "mir"))

# classify the point
class.tb <- sits_classify(point.tb, samples.tb)

# plot the classification
sits_plot (class.tb)

