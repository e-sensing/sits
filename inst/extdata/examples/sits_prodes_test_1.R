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


# obtain a time series from the WTSS server for this point
series1.tb <- sits_getdata(file = "./inst/extdata/samples/deforestation_points_226_64.csv", URL = URL, coverage = coverage, bands = bands, n_max = 20, ignore_dates = TRUE)

#series.tb <- sits_getdata(file = "./inst/extdata/samples/deforestation_points_226_64.json")
# retrieve a set of samples from a JSON file
patterns.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")

#sits_save(series.tb, "./inst/extdata/samples/deforestation_points_226_64.json")
#sits_plot (patterns.tb, type = "patterns")

matches.tb <- sits_TWDTW_matches(series1.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, span = 0)
sits_plot(matches.tb, type = "classification", start_date = "2013-08-01", end_date = "2017-07-31", interval = "12 month")

class.tb <- sits_TWDTW_classify (matches.tb, start_date = "2013-08-01", end_date = "2017-07-31", interval = "12 month")

assessment <- sits_accuracy(results.tb)
