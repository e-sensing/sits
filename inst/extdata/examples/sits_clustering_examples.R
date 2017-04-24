library (sits)

# select samples for pasture and savanna
cerrado.tb <- sits_getdata (file = system.file("extdata/samples/cerrado.json", package="sits"))

pat_cerrado.tb <- sits_patterns(cerrado.tb)

sits_plot(pat_cerrado.tb, type = "patterns")

pat_cerrado2.tb <- sits_patterns(cerrado.tb, method = "dendogram")

sits_plot(pat_cerrado2.tb, type = "patterns")

pat_cerrado3.tb <- sits_patterns(cerrado.tb, method = "dendogram", apply_gam = TRUE)

sits_plot(pat_cerrado3.tb, type = "patterns")

savanna.tb <- dplyr::filter (cerrado.tb, label == "Cerrado")

sits_plot (savanna.tb[1:10,], type = "one_by_one")

sits_plot (savanna.tb[1:50,], type = "allyears")

sits_plot (cerrado.tb[1:200,], type = "together")

cerrado_pat.tb <- sits_patterns(cerrado.tb, method = "dendo&gam")

sits_plot(cerrado_pat.tb, type = "patterns")
