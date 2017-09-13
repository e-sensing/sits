library(sits)

prodes.tb <- sits_import(system.file("extdata/samples/prodes_series_226_64.json.gz", package = "sits"))

sits_summary(prodes.tb)

sits_plot(prodes.tb[1,])


sits_getdata(longitude = -53.4115, latitude = -6.5252, bands = c("ndvi", "evi", "nir", "mir"), coverage = "mod13q1_512")
