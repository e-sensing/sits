# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

wtss_inpe <- sits_infoWTSS()

# get information about a specific coverage
coverage.tb <- sits_coverageWTSS("MOD13Q1")

# choose a coverage
coverage <- "MOD13Q1"
bands <-  c("ndvi", "evi", "red", "nir", "blue", "mir")

# select a point in the transition from forest to pasture in Northern MT
longitude <- -58.8967
latitude  <- -13.7214


# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(latitude = latitude, longitude = longitude, bands = bands,
                         service = "WTSS", coverage = coverage)

# plot the series (only the ndvi and evi bands)

sits_plot(point.tb)
point2.tb <- sits_select_bands(point.tb, bands = c("ndvi", "evi"))
sits_plot(point2.tb)


# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
samples.tb <- sits_getdata(file = system.file("extdata/samples/samples_matogrosso.csv", package = "sits"),
                           service = "WTSS", bands = bands, coverage = coverage,
                           start_date = "2000-02-18", end_date = "2016-12-18")
