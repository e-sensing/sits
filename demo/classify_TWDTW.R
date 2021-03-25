# satellite image time series package (SITS)
# example of the classification of a time series using the TWDTW algorithm
library(sits)

# Get a 17 year time series
data(point_mt_6bands)
# plot the series
plot(point_mt_6bands)

# retrieve a set of samples for MatoGrosso
# obtain a set of patterns for these samples
patterns <- sits_patterns(samples_modis_4bands)

plot(patterns)

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
matches <- sits_twdtw_classify(point_mt_6bands, patterns,
    bands = c("ndvi", "evi", "nir", "mir"),
    alpha = -0.1, beta = 100, theta = 0.5, keep = TRUE
)
