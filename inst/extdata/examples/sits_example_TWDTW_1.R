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

# a complicated point
long <- -55.01768
lat <-  -15.35588

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the “evi” band
series.tb %>%
     sits_select (bands = "evi") %>%
     sits_plot ()

# smooth the “evi” band and plot it
series.tb %>%
     sits_select (bands = "evi") %>%
     sits_smooth() %>%
     sits_plot()

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

damien_ieda.tb <- sits_getdata(file = system.file("extdata/samples/samples_Damien_Ieda_12classes_6bands_Water.json", package="sits"))

# retrieve a set of samples from a JSON file
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))

matogrosso.tb <- sits_prune (matogrosso.tb)

patt_mt.tb <- sits_patterns(matogrosso.tb)

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_MatoGrosso.json", package="sits"))

# plot patterns
sits_plot (patterns.tb, type = "patterns")

results.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

# plot the results of the classification
sits_plot (results.tb, type = "classification")
sits_plot (results.tb, type = "alignments")
