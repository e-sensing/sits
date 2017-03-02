# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

# then, configure the WTSS service
inpe <- sits_configWTSS (URL,
                         coverage = "mod13q1_512",
                         bands = c("ndvi", "evi", "nir"))

# a complicated point
long <- -55.51810
lat <-  -11.63884

point.tb <- sits_getdata(longitude = long, latitude = lat, wtss = inpe)

# read a pattern table from a JSON file
patterns2.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Damien_Rodrigo_11classes_6bands_centroid_Sep.json")

# plot patterns
sits_plot (patterns2.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_classify(point.tb, patterns2.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

# # plot the classification
plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
plot(x = matches, type = "alignments")
