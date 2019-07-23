# Example of accessing a time series using the EOCubes package
message("EOCubes is a time series package developed by INPE")

# Obtain information about the collections available in the WTSS service
library(sits)

# retrieve a specific collection
cube_modis.tb <- sits_cube(service = "EOCUBES", name = "MOD13Q1/006")

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_get_data(cube_modis.tb,
                          longitude = -46.5, latitude = -11.5,
                          bands = c("ndvi", "evi"),
                          start_date = "2016-09-01", end_date = "2017-09-01")

# plot the series
sits_plot(point.tb)

