
# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# cross-validation for FAO Bolivia Study

library(sits)
fao.tb <- sits_getdata (file = system.file("extdata/samples/fao_all_samples.tb.json", package = "sits"))

# Isto já foi rodado
#fao_cm <- sits_validate (fao.tb, times = 50, file = "./fao_cm2.json")

fao_cm <- sits_relabel (fao.tb, file = system.file("extdata/results/fao_cm.json", package = "sits"))

#Accuracy (PCC): 40.68%

conv_fao.lst <- tibble::lst("PermanentlyFloodedForest" = "Forest",
                           "UrbanArea"  = "BarrenLand",
                           "RegularlyFloodedForest" = "Forest",
                           "ClosedBroadDeciduousForest" = "Forest",
                           "Herbaceous" = "Shrubland",
                           "BarrenLand" =  "BarrenLand",
                           "MosaicGrasslandShrubland" = "Grassland",
                           "SparseVegetation" = "Shrubland",
                           "MosaicShrublandGrassland" = "Shrubland",
                           "RainfedCropland" = "Cropland",
                           "Shrubland" = "Shrubland",
                           "MosaicCropland" = "Cropland",
                           "ClosedBroadEvergreenForest"= "Forest",
                           "Snow" = "BarrenLand",
                           "MosaicVegetation" = "Forest",
                           "Salar" = "BarrenLand")

fao_cm1 <- sits_relabel (fao.tb, file = system.file("extdata/results/fao_cm.json", package = "sits"),
                         conv = conv_fao.lst)
#Accuracy (PCC): 67.50%

# retrieve a set of samples from a JSON file
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))

matogrosso.tb <- sits_prune(matogrosso.tb)
# perform accuracy assessment - já foi feito
<<<<<<< HEAD:inst/extdata/examples/sits_fao_matogrosso_validation.R
cm_dendog <- sits_validate (matogrosso.tb, method = "dendogram", bands = c("ndvi","evi", "nir"),
                     times = 50, perc = 0.5, file = "./mt_dendog.json")
=======
#cm_gam <- sits_validate (matogrosso.tb, method = "gam", bands = c("ndvi","evi", "nir"),
                    #times = 1, perc = 0.5, file = "./mt_cm.json")
>>>>>>> 6d0982be92b0a28c43d014a65bfcdcf40ccf603a:inst/extdata/examples/sits_fao_validation.R


mt_cm <- sits_relabel (matogrosso.tb, file = system.file("extdata/results/mt_cm.json", package = "sits"))

mt_conv.lst <- tibble::lst("Fallow_Cotton"       = "NonComerc_Cotton",
                           "NonComerc_Cotton"    = "NonComerc_Cotton",
                           "Pasture2"            = "Pasture",
                           "Soybean_Comerc1"     = "Soybean_Comerc",
                           "Soybean_Comerc2"     = "Soybean_Comerc",
                           "Soybean_Cotton"      = "Soybean_Comerc",
                           "Soybean_Fallow1"     = "Soybean_NonComerc",
                           "Soybean_Fallow2"     = "Soybean_NonComerc",
                           "Soybean_NonComerc1"  = "Soybean_NonComerc",
                           "Soybean_NonComerc2"  = "Soybean_NonComerc",
                           "Soybean_Pasture"     = "Soybean_NonComerc",
                           "Water"               = "Water",
                           "Cerrado"             = "Cerrado",
                           "Pasture"             = "Pasture",
                           "Forest"              = "Forest")

mt_cm <- sits_relabel (matogrosso.tb,
                       file = system.file("extdata/results/mt_cm.json", package = "sits"),
                       conv = mt_conv.lst)

cm_centroids <- sits_validate (matogrosso.tb, method = "centroids", bands = c("ndvi","evi", "nir"),
                                         times = 100, perc = 0.5, file = "./mt_cm_centroids.json")
