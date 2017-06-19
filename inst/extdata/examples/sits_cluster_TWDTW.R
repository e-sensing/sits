# R script 2017-05-02, verifying the separability between labels using TWDTW clustering and logistic weight
# by Victor Maus

library(sits)

# Load sample time series
mt.tb <- sits_getdata (file = system.file("extdata/samples/matogrosso.json", package="sits"))
sits_labels(mt.tb)

# Remove insignificant labels
mt.tb <- sits_significant_labels(mt.tb, 0.02)
sits_labels(mt.tb)

# Prunes the times series
mt.tb <- sits_prune(mt.tb)

# Set TWDTW weight function
log_fun = logisticWeight(-0.1, 50)

# TWDTW clustering
proc_time =
     system.time(new_class.tb <- sits_cluster(mt.tb, method = "dendogram", bands = c("evi", "ndvi", "nir", "mir"),
                                              dist_method = "TWDTW", show = TRUE, weight.fun = log_fun, span = 300))


sits_labels(new_class.tb)

