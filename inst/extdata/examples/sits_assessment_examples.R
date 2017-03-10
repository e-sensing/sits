library (sits)

# select samples for pasture and savanna
cerrado.tb <- sits_getdata (file = system.file("extdata/samples/cerrado.json", package="sits"))

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_Rodrigo_7classes_6bands.json", package="sits"))

bands = c("ndvi", "evi")
results.tb <- sits_TWDTW(cerrado.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

assessment <- sits_assess (results.tb)

assessment

