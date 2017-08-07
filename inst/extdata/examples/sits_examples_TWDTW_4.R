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
long <- -58.60918
lat <-  -10.55992

# outro ponto interessante: -58.63919,-10.74036

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# smooth all the bands, plot them, and save the smoothed bands in a new table
series_s.tb <- series.tb %>%
     sits_smooth() %>%
     sits_rename (c("ndvi_smooth", "evi_smooth", "nir_smooth")) %>%
     sits_plot()

# merge the raw and smoothed series and plot the “red” and “red_smooth” bands
series.tb %>%
     sits_merge(., series_s.tb) %>%
     sits_select (bands = c("evi", "evi_smooth")) %>%
     sits_plot()

# retrieve patterns from a JSON file
matogrosso.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_MatoGrosso_18052017.json", package="sits"))

sits_plot (matogrosso.tb, type = "patterns")

results.tb <- sits_TWDTW(series.tb, matogrosso.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

# plot the results of the classification
sits_plot (results.tb, type = "classification")
sits_plot (results.tb, type = "alignments")

matogrosso2.tb <- dplyr::filter (matogrosso.tb, label != "Sugarcane")

results2.tb <- sits_TWDTW(series.tb, matogrosso2.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

# plot the results of the classification
sits_plot (results2.tb, type = "classification")
sits_plot (results2.tb, type = "alignments")