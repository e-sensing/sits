# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

point.tb <- readRDS(system.file("extdata/time_series/point.rds", package = "sits"))
# plot the series
sits_plot (sits_select(point.tb, bands = c("ndvi", "evi")))

# retrieve a set of samples from an RDS file
embrapa_mt.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

# obtain a set of patterns for these samples
patterns.tb <- sits_patterns(embrapa_mt.tb)

sits_plot (patterns.tb)

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
matches <- sits_TWDTW_matches(point.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

# plot the alignments of the time series
sits_plot_TWDTW_alignments (matches)

#classify the time series matches using yearly intervals
results.tb <- sits_TWDTW_classify(matches, series.tb, start_date = "2000-08-01", end_date = "2016-07-31",
                                  interval = "12 month")

# plot the classification of the time series by yearly intervals
sits_plot_TWDTW_classification (matches)
