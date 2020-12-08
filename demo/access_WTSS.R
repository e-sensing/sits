# Example of accessing a time series using the WTSS (web time series service)
message("WTSS is a web time series service developed by INPE")
# Obtain information about the cubes available in the WTSS service
library(sits)

# get information about a specific data cube
cube_wtss.tb <- sits_cube(
    type = "WTSS",
    URL = "http://www.esensing.dpi.inpe.br/wtss/",
    name = "MOD13Q1"
)

if (purrr::is_null(cube_wtss.tb)) {
      stop("WTSS server not responding")
  }

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_get_data(cube_wtss.tb,
    longitude = -47.0516, latitude = -10.7241,
    bands = c("NDVI", "EVI", "NIR", "MIR")
)

# plot the series
plot(point.tb)
