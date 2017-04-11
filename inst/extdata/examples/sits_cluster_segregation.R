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

new_class.tb <- sits_cluster(mt.tb, method = "koho&dogram", grouping_method = "ward.D2",
                             bands = c("evi", "ndvi", "nir", "mir"), n_clusters = 15,
                             show = TRUE, return_members = TRUE, unsupervised = TRUE,
                             koh_xgrid = 15, koh_ygrid = 15, koh_rlen = 2000, koh_alpha = c(0.2, 0.04))

sits_cluster_segregation(new_class.tb)

sits_segregation_measure(new_class.tb, per_cluster = TRUE)

sits_segregation_measure(new_class.tb, per_cluster = FALSE)
