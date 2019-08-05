# satellite image time series package (SITS)
# example of the classification of a time series using the TWDTW algorithm
library(sits)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#select the bands for classification
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

#select the bands for classification
samples <- inSitu::br_mt_1_8K_9classes_6bands

# Get a 17 year time series
data(point_mt_6bands)
# plot the series
sits_plot(point_mt_6bands)

# retrieve a set of samples from an RDA file
data(samples)

# obtain a set of patterns for these samples
patterns.tb <- sits_patterns(samples)

sits_plot(patterns.tb)

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
matches <- sits_twdtw_classify(point_mt_6bands, patterns.tb, bands = c("ndvi", "evi", "nir", "mir"),
                               alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

