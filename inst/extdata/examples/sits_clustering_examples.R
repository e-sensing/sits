library (sits)

# select samples for pasture and savanna
cerrado.tb <- sits_getdata (file = system.file("extdata/samples/cerrado6.json", package="sits"))


savanna.tb <- dplyr::filter (cerrado.tb, label == "Cerrado")

sits_plot (savanna.tb[1:10,], type = "one_by_one")

sits_plot (savanna.tb[1:50,], type = "allyears")

sits_plot (cerrado.tb[1:200,], type = "together")

cerrado_pat.tb <- sits_patterns(cerrado.tb, method = "centroids")

sits_plot(cerrado_pat.tb, type = "patterns")
