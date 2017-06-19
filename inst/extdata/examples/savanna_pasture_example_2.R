#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
sits_coverageWTSS(URL,"mod13q1_512")

bands <- c("ndvi", "evi")

#get the samples
samples.tb <- sits_getdata (file = "./inst/extdata/samples/savanna_pasture_samples_2.csv",
                            URL = URL, coverage = "mod13q1_512", bands = bands)

samples.tb[1,] %>%
     sits_select (bands = "evi") %>%
     sits_smooth() %>%
     sits_plot()

samples.tb[1,] %>%
     sits_select (bands = "evi") %>%
     sits_plot()

patterns.tb <- sits_patterns (samples.tb)

sits_plot(patterns.tb, type = "patterns")

patterns2.tb <- sits_patterns(samples.tb, method = "dendogram", n_clusters = 2, apply_gam = TRUE)

sits_plot(patterns2.tb, type = "patterns")

long <- -52.83097
lat <- -14.35957

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

results.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

results2.tb <- sits_TWDTW(series.tb, patterns2.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

# plot the results of the classification
sits_plot (results2.tb, type = "classification")
sits_plot (results2.tb, type = "alignments")
