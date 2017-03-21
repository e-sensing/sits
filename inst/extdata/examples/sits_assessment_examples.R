library (sits)

# select samples for pasture and savanna
cerrado.tb <- sits_getdata (file = system.file("extdata/samples/cerrado.json", package="sits"))

cerrado1.tb <- cerrado.tb[1:40,]
cerrado2.tb <- cerrado.tb[700:746,]

cerrado_s.tb <- dplyr::bind_rows(cerrado1.tb, cerrado2.tb)

sits_save (cerrado_s.tb, "./inst/extdata/samples/cerrado_small.json")

val <- sits_validate(cerrado_s.tb, times = 10, p = 0.2)

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_Rodrigo_7classes_6bands.json", package="sits"))

bands = c("ndvi", "evi")
results.tb <- sits_TWDTW(cerrado.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

assessment <- sits_assess (results.tb)

assessment

