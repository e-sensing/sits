# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

#select a coverage
coverage <- "mod13q1_512"

# get information about a specific coverage
sits_coverageWTSS(URL,coverage)
#select the bands used for classification
bands <- c("ndvi")

lat <- -14.9191
long <- -59.11777778

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the series
sits_plot (series.tb)

long <- -53.65243708
lat <- -6.419774781


# obtain a time series from the WTSS server for this point
series1.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mixl8mod", bands = bands)

# plot the series
sits_plot (series1.tb)

series1.tb %>%
     sits_kf () %>%
     sits_select (bands = c("ndvi.kf.estimation")) %>%
     sits_plot()

series1.tb %>%
     sits_envelope (window_size = 3) %>%
     sits_envelope (window_size = 3) %>%
     sits_kf () %>%
     sits_select (bands = c("ndvi.upper.lower.kf.estimation", "ndvi.lower.upper.kf.estimation" )) %>%
     sits_plot()
