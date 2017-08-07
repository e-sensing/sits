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

# a point
long <- -59.60500
lat <-  -10.23667

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the “evi” band
series.tb %>%
     sits_select (bands = "evi") %>%
     sits_plot ()

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json", package="sits"))
# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
results.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

# # plot the classification
sits_plot(results.tb, type = "classification")
# # plot the alignments
sits_plot(results.tb, type = "alignments")
# # plot the matches for the class
sits_plot(results.tb, type = "matches", label = "SA", k = 4)

long <- -55.01768
lat <-  -15.35588

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the “evi” band
series.tb %>%
     sits_select (bands = "evi") %>%
     sits_plot ()

# classify samples using TWDTW
results.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

# # plot the classification
sits_plot(results.tb, type = "classification")
# # plot the alignments
sits_plot(results.tb, type = "alignments")