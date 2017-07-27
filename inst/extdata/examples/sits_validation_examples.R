# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# comparing patterns obtained from Generalized Additive Models and from Clustering
library(sits)

# retrieve a set of samples from a JSON file
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits"))

cerrado1.tb <- dplyr::bind_rows(head(cerrado.tb, n = 20), tail (cerrado.tb, n = 20))


# perform accuracy assessment
cm_gam <- sits_cross_validate (cerrado.tb, method = "gam", bands = c("ndvi","evi"), times = 25, perc = 0.3)


cm_som <- sits_cross_validate (cerrado.tb, method = "koho&dogram", bands = c("evi", "ndvi"), times = 1, perc = 0.3, n_clusters = 5,
                     koh_xgrid = 12, koh_ygrid = 12, koh_rlen = 1500, koh_alpha = c(0.07, 0.01),
                     min_clu_perc = 0.1, apply_gam = TRUE, tw_alpha = -0.1, tw_beta = 45)


# Accuracy (PCC): 94.1460506706408%
# Cohen's Kappa: 0.882
# Users accuracy:
# Cerrado Pasture
# 96.2    91.7
#
# Producers accuracy:
# Cerrado Pasture
# 93.1    95.5
# Confusion matrix
# y
# x         Cerrado Pasture
# Cerrado   17322    1286
# Pasture     678   14264

