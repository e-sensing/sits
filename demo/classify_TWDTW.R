# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

data("ts_2000_2016")
# plot the series
sits_plot (sits_select(ts_2000_2016, bands = c("ndvi", "evi")))

# retrieve a set of samples from an RDA file
data(samples_MT_9classes)

# obtain a set of patterns for these samples
patterns.tb <- sits_patterns(samples_MT_9classes)

sits_plot (patterns.tb)

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
matches <- sits_TWDTW_classify(ts_2000_2016, patterns.tb, bands = c("ndvi", "evi"),
                               alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)
