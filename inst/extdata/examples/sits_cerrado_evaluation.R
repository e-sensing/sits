# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# comparing patterns obtained from Generalized Additive Models and from Clustering
library(sits)

# retrieve a set of samples from a JSON file

cerrado.tb <- sits_getdata(file = "./inst/extdata/samples/cerrado.json")
cerrado.tb <- sits_smooth(cerrado.tb)

sits_plot(cerrado.tb, type = "together")
sits_plot(cerrado.tb[1:30, ])
cerrado_l.tb <- dplyr::filter(cerrado.tb, label == "Cerrado")
cerrado_p.tb <- dplyr::filter(cerrado.tb, label == "Pasture")
train1.tb <- cerrado_l.tb[1:20, ]
train2.tb <- cerrado_p.tb[1:20, ]
val1.tb <- cerrado_l.tb[-(1:20), ]
val2.tb <- cerrado_p.tb[-(1:20), ]
train.tb <- dplyr::bind_rows(train1.tb, train2.tb)
val.tb <- dplyr::bind_rows(val1.tb, val2.tb)
patterns1.tb <- sits_patterns(train.tb)
sits_plot(patterns1.tb, type = "patterns")
results.tb <- sits_TWDTW(val.tb, patterns1.tb, bands)
sits_plot(results.tb[1:5, ], type = "classification")
confusion.mx <- sits_assess(results.tb, 1)

# clusters.tb <- sits_cluster(train.tb, n_clusters = 2)
clusters1.tb <- sits_cluster(train1.tb, n_clusters = 2)
clusters2.tb <- sits_cluster(train2.tb, n_clusters = 2)
clusters.tb <- dplyr::bind_rows(clusters1.tb, clusters2.tb)
clusters.tb[1,]$label <- "Cerrado"
clusters.tb[2,]$label <- "Pasture"
results1.tb <- sits_TWDTW(val.tb, clusters.tb, bands)
confusion2.mx <- sits_assess(results1.tb, 1)
