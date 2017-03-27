# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# comparing patterns obtained from Generalized Additive Models and from Clustering
library(sits)

# retrieve a set of samples from a JSON file
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado6.json", package="sits"))

cerrado1.tb <- dplyr::bind_rows(head(cerrado.tb, n = 20), tail (cerrado.tb, n = 20))

# perform accuracy assessment
cm <- sits_validate (cerrado1.tb, method = "gam", bands = c("ndvi","evi", "nir"), times = 20, perc = 0.1)

cm <- sits_relabel (cerrado1.tb, file = "./conf_matrix.json")

conversion1.lst = tibble::lst("Cerrado" = "Savanna", "Pasture" = "Pastagem")
cm <- sits_relabel (cerrado1.tb, file = "./conf_matrix.json", conv = conversion1.lst)

# perform accuracy assessment
cm <- sits_validate (cerrado.tb, method = "gam", bands = c("ndvi","evi", "nir"), times = 50, perc = 0.1)

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

