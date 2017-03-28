
# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# cross-validation for FAO Bolivia Study

library(sits)
fao.tb <- sits_getdata (file = system.file("extdata/samples/fao_all_samples.tb.json", package = "sits"))

# Isto já foi rodado
#fao_cm <- sits_validate (fao.tb, times = 50, file = "./fao_cm.json")

fao_cm <- sits_relabel (fao.tb, file = system.file("extdata/results/fao_cm.json", package = "sits"))

conv_fao.lst <- tibble:lst("PermanentlyFloodedForest" = "Forest",
                           "UrbanArea"  = "Urban Area",
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
                           "Snow" = "Snow/Salar",
                           "MosaicVegetation" = "Vegetation",
                           "Salar" = "Salar")

# retrieve a set of samples from a JSON file
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits")

# perform accuracy assessment
cm <- sits_validate (matogrosso.tb, method = "gam", bands = c("ndvi","evi", "nir"), times = 50, perc = 0.1)

