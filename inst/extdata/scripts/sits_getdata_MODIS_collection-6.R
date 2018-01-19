# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

wtss_inpe <- sits_infoWTSS()

modis_col5.tb <- sits_coverage(service = "WTSS-INPE-1", product = "MOD13Q1", name = "mod13q1_512")

# get information about a specific coverage
modis_col6.tb <- sits_coverage(service = "WTSS-INPE-2", product = "MOD13Q1", name = "MOD13Q1")


# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(latitude = -8.5442, longitude = -47.6258,
                         bands = c("ndvi", "evi", "red", "nir", "blue", "mir"),
                         coverage = modis_col6.tb)

# plot the series (only the ndvi and evi bands)

sits_plot(sits_select_bands(point.tb, bands = c("ndvi", "evi")))


# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
samples.tb <- sits_getdata(file = system.file("extdata/samples/cerrado_13classes_col6.csv", package = "sits"),
                           coverage = modis_col6.tb, .n_start = 2001, .n_max = 3000, .n_save = 100)

save(samples.tb, file = "./inst/extdata/time_series/cerrado_13_classes_col6_2001_3000.rda")

