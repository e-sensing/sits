# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
sits_coverageWTSS(URL,"mixl8mod")


# choose a coverage
coverage <- "mixl8mod"
# recover all bands
bands <- c("ndvi", "evi", "nir")

long <- -53.41151061
lat <-  -6.525220837

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = coverage, bands = bands)

# retrieve a set of samples from a JSON file
patterns.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")

sits_plot(series.tb)

matches.tb <- sits_TWDTW_matches(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, span = 0)
sits_plot(matches.tb, type = "classification")
sits_plot(matches.tb, type = "alignments")

class.tb <- sits_TWDTW_classify (matches.tb, interval = "6 month")

class.tb$best_matches

