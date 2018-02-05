# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

wtss_inpe <- sits_infoWTSS()

# get information about a specific coverage
modis_col6.tb <- sits_coverage(service = "WTSS-INPE-2", product = "MOD13Q1", name = "MOD13Q1")



# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
samples.tb <- sits_getdata(file = system.file("extdata/samples/cerrado_13classes_col6_adj.csv", package = "sits"),
                           coverage = modis_col6.tb, .n_start = 1, .n_max = Inf, .n_save = 100)

save(samples.tb, file = "./inst/extdata/time_series/cerrado_13_classes_col5.rda")

