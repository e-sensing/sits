# satellite image time series
# devtools::install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

# get information about a specific coverage
mod13_info <- sits_coverageWTSS(URL,"mod13q1_512")

# then, configure the WTSS service
inpe <- sits_configWTSS (URL,
                 coverage = "mod13q1_512",
                 bands = c("ndvi", "evi", "nir"))

# a complicated point
long <- -55.51810
lat <-  -11.63884

series.tb <- sits_getdata(longitude = long, latitude = lat, wtss = inpe)

series.tb %>%
     sits_select (bands = "evi") %>%
     sits_plot ()

series.tb %>%
     sits_select (bands = "evi") %>%
     sits_smooth() %>%
     sits_plot()

series_s.tb <- series.tb %>%
     sits_smooth() %>%
     sits_rename (c("ndvi_smooth", "evi_smooth"))

series.tb %>%
     sits_merge(., series_s.tb) %>%
     sits_select (bands = c("evi", "evi_smooth")) %>%
     sits_plot()

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_Damien_Ieda_Rodrigo_14classes_3bands.json", package="sits"))

# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
results.tb <- sits_TWDTW(series.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

sits_plot (results.tb, type = "classification")
sits_plot (results.tb, type = "alignments")


