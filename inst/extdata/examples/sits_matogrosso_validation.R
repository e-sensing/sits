
# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# cross-validation for FAO Bolivia Study

library(sits)

# retrieve a set of samples from a JSON file
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))

matogrosso.tb <- sits_prune(matogrosso.tb)
# perform accuracy assessment - já foi feito
#cm_gam <- sits_validate (matogrosso.tb, method = "gam", bands = c("ndvi","evi", "nir"),
                    #times = 100, perc = 0.5, file = "./mt_cm.json")


mt_cm <- sits_relabel (matogrosso.tb, file = system.file("extdata/results/mt_cm.json", package = "sits"))

mt_conv.lst <- tibble::lst("Fallow_Cotton"       = "NonComerc_Cotton",
                           "NonComerc_Cotton"    = "NonComerc_Cotton",
                           "Pasture2"            = "Pasture",
                           "Soybean_Comerc1"     = "Soybean_Comerc",
                           "Soybean_Comerc2"     = "Soybean_Comerc",
                           "Soybean_Cotton"      = "Soybean_Cotton",
                           "Soybean_Fallow1"     = "Soybean_NonComerc",
                           "Soybean_Fallow2"     = "Soybean_NonComerc",
                           "Soybean_NonComerc1"  = "Soybean_NonComerc",
                           "Soybean_NonComerc2"  = "Soybean_Comerc",
                           "Soybean_Pasture"     = "Soybean_Pasture",
                           "Water"               = "Water",
                           "Cerrado"             = "Cerrado",
                           "Pasture"             = "Pasture",
                           "Forest"              = "Forest")

mt_cm <- sits_relabel (matogrosso.tb,
                       file = system.file("extdata/results/mt_cm.json", package = "sits"),
                       conv = mt_conv.lst)

cm_centroids <- sits_validate (matogrosso.tb, method = "centroids", bands = c("ndvi","evi", "nir"),
                                         times = 1, perc = 0.5, file = "./mt_cm_centroids.json")
