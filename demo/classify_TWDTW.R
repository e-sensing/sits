# satellite image time series package (SITS)
# example of the classification of a time series using the TWDTW algorithm
library(sits)

# Get a 17 year time series

data("point_MT_6bands")
# plot the series
sits_plot (point_MT_6bands)

# retrieve a set of samples from an RDA file
data(samples_MT_9classes)

# obtain a set of patterns for these samples
patterns.tb <- sits_patterns(samples_MT_9classes)

sits_plot (patterns.tb)

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
matches <- sits_TWDTW_classify(point_MT_6bands, patterns.tb, bands = c("ndvi", "evi"),
                               alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

