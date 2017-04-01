
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
