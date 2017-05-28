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
<<<<<<< HEAD
long <- -58.48733
lat <-  -10.14707
=======
long <- -58.93260
lat <-  -9.91081

# points
# (-58.60918, -10.55992)
# (-58.63919, -10.74036)
# (-58.79581,  -9.91111)
# (-58.93260,  -9.91081)
# (-58.45774,  -10.19968)
# (-58.48733,  -10.14707)

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# plot all the bands, plot them, and save the smoothed bands in a new table
# plot the “evi” band
series.tb %>%
     sits_select (bands = "evi") %>%
     sits_plot ()
# plot the “ndvi” band
series.tb %>%
     sits_select (bands = "ndvi") %>%
     sits_plot ()

# retrieve a set of samples from a JSON file
patterns.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Damien_Ieda_Rodrigo_17classes_3bands.json")

sits_plot (patterns.tb, type = "patterns")

results.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

# plot the results of the classification
sits_plot (results.tb, type = "classification")
sits_plot (results.tb, type = "alignments")

results2.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 150, theta = 0.5)

# plot the results of the classification
sits_plot (results2.tb, type = "classification")
sits_plot (results2.tb, type = "alignments")

