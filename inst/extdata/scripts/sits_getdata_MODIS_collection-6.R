# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

wtss_inpe <- sits_infoWTSS()

# get information about a specific coverage
coverage.tb <- sits_coverage(service = "WTSS-INPE-2", product = "MOD13Q1", coverage = "MOD13Q1")

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(latitude = -13.7214, longitude = -58.8967,
                         bands = c("ndvi", "evi", "red", "nir", "blue", "mir"),
                         service = "WTSS-INPE-2", coverage = "MOD13Q1")

# plot the series (only the ndvi and evi bands)

sits_plot(sits_select_bands(point.tb, bands = c("ndvi", "evi")))


# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
samples.tb <- sits_getdata(file = system.file("extdata/samples/samples_matogrosso.csv", package = "sits"),
                           service = "WTSS-INPE-2", coverage = "MOD13Q1")
