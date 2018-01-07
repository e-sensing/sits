# Example of accessing a time series using the WTSS (web time series service)

# Get information about the WTSS (web time series service)

# Set the URL of the WTSS service
URL <- "http://www.dpi.inpe.br/tws/wtss"
# Obtain information about the coverages available
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
coverage.tb <- sits_coverageWTSS(URL,"mod13q1_512")

# choose a coverage
coverage <- "mod13q1_512"
bands <-  c("ndvi", "evi", "nir", "mir")

# select a point in the transition from forest to pasture in Northern MT
longitude <- -47.0516
latitude  <- -10.7241

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(longitude = longitude, latitude = latitude, bands = bands,
                         URL = URL, coverage = coverage)

# plot the series
sits_plot (sits_select (point.tb, bands = c("ndvi", "evi", "nir", "mir")))

