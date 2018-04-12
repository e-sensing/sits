# Example of accessing a time series using the WTSS (web time series service)
message("WTSS is a web time series service developed by INPE")
# Obtain information about the coverages available in the WTSS service
library(sits)

# get information about a specific coverage
coverage_wtss.tb <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(coverage_wtss.tb, longitude = -47.0516, latitude = -10.7241,
                         bands = c("ndvi", "evi", "nir", "mir"))

# plot the series
sits_plot(point.tb)

