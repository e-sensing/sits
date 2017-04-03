library (sits)

# select samples for pasture and savanna
cerrado.tb <- sits_getdata (file = system.file("extdata/samples/cerrado.json", package="sits"))


savanna.tb <- dplyr::filter (cerrado.tb, label == "Cerrado")

sits_plot (savanna.tb[1:10,], type = "one_by_one")

sits_plot (savanna.tb[1:50,], type = "allyears")

sits_plot (cerrado.tb[1:200,], type = "together")

cerrado_members.tb <- sits_cluster(mt.tb, bands = c("evi", "ndvi", "nir", "mir"), show = FALSE, return_members = TRUE, min_clu_perc = 0.1)
tidyr::unnest(cerrado_members.tb)

cerrado_pat.tb <- sits_patterns(cerrado.tb, method = "dendo&gam")

sits_plot(cerrado_pat.tb, type = "patterns")
