# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# cross-validation for FAO Bolivia Study

library(sits)

# retrieve a set of samples from a JSON file
damien_ieda.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits"))

matogrosso.tb <- sits_prune(matogrosso.tb)
# perform accuracy assessment - já foi feito
#cm_gam <- sits_cross_validate (matogrosso.tb, method = "gam", bands = c("ndvi","evi", "nir"),
#times = 100, perc = 0.5, file = "./mt_cm.json")

patterns_dend.tb <- sits_patterns(matogrosso.tb, method = "dendogram", bands = c("ndvi","evi", "nir"))

sits_plot (patterns_dend.tb, type = "patterns")

