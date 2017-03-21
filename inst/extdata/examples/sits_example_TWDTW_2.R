# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

# a complicated point
long <- -55.51810
lat <-  -11.63884

coverage <- "mod13q1_512"

bands <- c("ndvi", "evi", "nir")

point.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = coverage, bands = bands)

# read a pattern table from a JSON file
patterns2.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_Damien_Ieda_Rodrigo_14classes_3bands_Original_Labels_Sep.json", package="sits"))

# plot patterns
sits_plot (patterns2.tb, type = "patterns")

# classify samples using TWDTW
results.tb <- sits_TWDTW(point.tb, patterns2.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)

# # plot the classification
sits_plot(results.tb, type = "classification")
# # plot the alignments
sits_plot(results.tb, type = "alignments")
# # plot the matches for the class
sits_plot(results.tb, type = "matches", label = "SA", k = 4)

