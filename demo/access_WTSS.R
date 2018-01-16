# Example of accessing a time series using the WTSS (web time series service)

# Obtain information about the coverages available in the WTSS service
wtss_inpe <- sits_infoWTSS()

# get information about a specific coverage
coverage.tb <- sits_coverageWTSS(product = "MOD13Q1", coverage = "mod13q1_512")


# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(longitude = -47.0516, latitude = -10.7241, bands = c("ndvi", "evi", "nir", "mir"),
                         service = "WTSS", coverage = "mod13q1_512")

# plot the series
sits_plot(point.tb)

