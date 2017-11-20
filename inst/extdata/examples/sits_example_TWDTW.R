# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
coverage.tb <- sits_coverageWTSS(URL,"mod13q1_512")
timeline  <- sits_timeline (coverage.tb)

# choose a coverage
coverage <- "mod13q1_512"
# recover all bands
bands <- c("ndvi", "evi", "nir")

# a point in the transition forest pasture in Northern MT
long <- -55.57320
lat <- -11.50566

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the series
sits_plot (sits_select(series.tb, bands = c("ndvi", "evi")))

# retrieve a set of samples from an RDS file
embrapa_mt.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

# obtain a set of patterns for these samples
patterns.tb <- sits_patterns(embrapa_mt.tb)

sits_plot (patterns.tb, type = "patterns")

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
matches <- sits_TWDTW_matches(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

# plot the alignments of the time series
sits_plot_TWDTW_alignments (matches)

#classify the time series matches using yearly intervals
results.tb <- sits_TWDTW_classify(matches, series.tb, start_date = "2000-08-01", end_date = "2016-07-31",
                                  interval = "12 month")

# plot the classification of the time series by yearly intervals
sits_plot_TWDTW_classification (matches)
