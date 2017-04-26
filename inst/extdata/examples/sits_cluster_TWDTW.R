# R script 2017-04-25
# sits package by Victor Maus

# Verifying the separability between labels using unsupervised clustering algorithm and TWDTW distance

library(sits)
mt.tb <- sits_getdata (file = system.file("extdata/samples/matogrosso.json", package="sits"))
sits_labels(mt.tb)

mt.tb <- sits_significant_labels(mt.tb, 0.02)
sits_labels(mt.tb)

mt.tb <- sits_prune(mt.tb)

mt.tb <- slice(mt.tb, 1:100)

log_fun = logisticWeight(-0.1, 50)

new_class.tb <- sits_cluster(mt.tb, method = "dendogram", grouping_method = "ward.D2",
                             bands = c("evi", "ndvi", "nir", "mir"), dist_method = "TWDTW",
                             show = TRUE, weight.fun = log_fun, span = 300)

sits_labels(new_class.tb)

sits_cluster_segregation(new_class.tb)

sits_segregation_measure(new_class.tb, per_cluster = TRUE)

sits_segregation_measure(new_class.tb, per_cluster = FALSE)

