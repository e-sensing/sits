# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
sits_coverageWTSS(URL,"mod13q1_512")


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
sits_plot (series.tb)

# smooth the time series using the Whittaker smoother

smooth.tb <- series.tb %>%
     sits_whittaker(lambda = 2.0) %>%
     sits_plot()

# merge the raw and smoothed series and plot the “ndvi” and “ndvi.whit” bands
series.tb %>%
     sits_merge(., smooth.tb) %>%
     sits_select (bands = c("ndvi", "ndvi.whit")) %>%
     sits_plot()

# retrieve a set of samples from a JSON file
patterns.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")

sits_plot (patterns.tb, type = "patterns")

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
matches.tb <- sits_TWDTW_matches(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

# plot the alignments of the time series
sits_plot (matches.tb, patterns.tb, type = "alignments")

#classify the time series matches using yearly intervals
results.tb <- sits_TWDTW_classify(matches.tb, patterns.tb, start_date = "2000-08-01", end_date = "2016-07-31",
                                  interval = "12 month")

# plot the classification of the time series by yearly intervals
sits_plot (results.tb, patterns.tb, type = "classification")
