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
<<<<<<< HEAD

# bands used in this example
bands <- c("ndvi", "evi", "blue", "red", "nir", "mir")

# a complicated point
long <- -55.51810
lat <-  -11.63884

=======
# choose a coverage
coverage <- "mod13q1_512"
# recover all bands
bands <- c("ndvi", "evi", "blue", "red", "nir", "mir")

# a complicated point
long <- -55.01768
lat <-  -15.35588

# obtain a time series from the WTSS server for this point
>>>>>>> 2ac7a2559c1b6bce389d0a2a8fcc3c4aa4f6ae64
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
     sits_rename (c("ndvi_smooth", "evi_smooth", "blue_smooth", "red_smooth", "nir_smooth", "mir_smooth")) %>%
     sits_plot()

# merge the raw and smoothed series and plot the “red” and “red_smooth” bands
series.tb %>%
     sits_merge(., series_s.tb) %>%
     sits_select (bands = c("red", "red_smooth")) %>%
     sits_plot()

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_MatoGrosso.json", package="sits"))

# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
results.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

# plot the results of the classification
sits_plot (results.tb, type = "classification")
sits_plot (results.tb, type = "alignments")
