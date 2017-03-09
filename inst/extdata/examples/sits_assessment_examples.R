library (sits)

# select samples for pasture and savanna
cerrado.tb <- sits_getdata (file = "./inst/extdata/Samples/cerrado.json")


# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Rodrigo_7classes_6bands.json")

bands = c("ndvi", "evi")
results.tb <- sits_TWDTW(cerrado.tb, patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

area <- 0.216 * 0.216

confusion.mx <- sits_assess (results.tb, area)

savanna.tb <- dplyr::filter (cerrado.tb, label == "Savanna")

sits_plot (savanna.tb[1:10,], type = "one_by_one")

sits_plot (savanna.tb[1:50,], type = "allyears")

sits_plot (cerrado.tb[1:200,], type = "together")

clusters_sav.tb <- sits_cluster (savanna.tb[1:50,], type = "dendogram", n_clusters = 4)

cerrado_pat.tb <- sits_patterns(cerrado.tb)

sits_plot(cerrado_pat.tb, type = "patterns")
