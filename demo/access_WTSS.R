# Example of accessing a time series using the WTSS (web time series service)

# Get information about the WTSS (web time series service)

# Obtain information about the coverages available in the WTSS service
wtss_inpe <- sits_infoWTSS()

# get information about a specific coverage
coverage.tb <- sits_coverageWTSS("mod13q1_512")

# choose a coverage
coverage <- "mod13q1_512"
bands <-  c("ndvi", "evi", "nir", "mir")

# select a point in the transition from forest to pasture in Northern MT
longitude <- -47.0516
latitude  <- -10.7241

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(longitude = longitude, latitude = latitude, bands = bands,
                         service = "WTSS", coverage = coverage)

# plot the series
sits_plot(sits_select(point.tb, bands = c("ndvi", "evi", "nir", "mir")))

