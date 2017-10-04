library (sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
sits_coverageWTSS(URL,"mod13q1_512")

# retrieve a set of points
coverage <- "mod13q1_512"
bands <- c("ndvi", "evi", "nir", "mir")

series.tb <- sits_getdata (file = "./inst/extdata/samples/cerrado_pasture_10_points.csv", coverage = coverage,
                           bands = bands, URL = URL)


