# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# Verifying the separability between labels using unsupervised clustering algorithm.

library(sits)
mt.tb <- sits_getdata (file = system.file("extdata/samples/matogrosso.json", package="sits"))
sits_labels(mt.tb)

mt.tb <- sits_significant_labels(mt.tb, 0.02)
sits_labels(mt.tb)

mt.tb <- sits_prune(mt.tb)

new_class.tb <- sits_cluster(data.tb = mt.tb, method = "kohonen-dendogram", grouping_method = "ward.D2",
                             bands = c("evi", "ndvi", "nir", "mir"), n_clusters = 15,
                             show = TRUE, return_members = TRUE, unsupervised = TRUE,
                             koh_xgrid = 10, koh_ygrid = 10, koh_rlen = 1200, koh_alpha = c(0.2, 0.04))

new_class.tb

sits_labels(new_class.tb)

sits_cluster_separability(new_class.tb)

sits_separability_measure(new_class.tb, per_cluster = TRUE)

sits_separability_measure(new_class.tb, per_cluster = FALSE)
