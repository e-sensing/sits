# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
sits_infoWTSS(URL)

# then, configure the WTSS service
coverage <-  "mod13q1_512"
bands    <-  c("ndvi", "evi", "nir")

samples1.tb <- sits_getdata(file = system.file("extdata/samples/pasture.csv", package="sits"), URL = URL, coverage = coverage, bands = bands)

sits_plot(samples1.tb)

clusters.tb <- sits_cluster(samples1.tb, bands = bands, method = "dendogram")

patterns1.tb <- sits_patterns (samples1.tb)

sits_plot (patterns1.tb)

patterns2.tb <- sits_patterns (samples1.tb, method = "dendogram")

sits_plot (patterns2.tb, type= "patterns")

patterns3.tb <- sits_patterns (samples1.tb, method = "dendogram", apply_gam = TRUE)

sits_plot (patterns3.tb, type= "patterns")
