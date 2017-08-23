library (sits)

# load data from EMBRAPA
embrapa_orig.tb <- sits_getdata (file = system.file("extdata/samples/embrapa_cerrado_forest.json.gz", package="sits"))
# classes originais da EMBRAPA
labels <- sits_labels (embrapa_orig.tb)
# ajuste inicial - transformar para Ingles e juntar as pastagens
newlabels1.lst <- tibble::lst (
    "Algodao_Pousio"  = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Pasto Limpo"     = "Pasture",
    "Pasto Sujo"      = "Pasture",
    "Reflorestamento" = "Planted_Forest",
    "Soja_Algodao"    = "Soy_Cotton",
    "Soja_Cobertura"  = "Soy_Coverage",
    "Soja_Crotalaria" = "Soy_Crotalaria",
    "Soja_Feijao"     = "Soy_Beans",
    "Soja_Girassol"   = "Soy_Sunflower",
    "Soja_Milheto"    = "Soy_Millet",
    "Soja_Milho"      = "Soy_Corn",
    "Soja_Pasto"      = "Soy_Pasture",
    "Soja_Pousio"     = "Soy_Fallow",
    "Soja_Sorgo"      = "Soy_Sorghum")

# relabel embrapa data
embrapa_orig2.tb <- sits_relabel(embrapa_orig.tb, conv = newlabels1.lst)
# find the bands
bands <- sits_bands (embrapa_orig2.tb) # "ndvi" "evi"  "nir"  "mir"  "blue" "red"

# retrieve data from Damien Arvor
damien.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json.gz", package="sits"))
labels_damien <- sits_labels (damien.tb)
bands_damien <- sits_bands (damien.tb) # "ndvi" "evi"  "nir"  "mir"  "blue" "red"

# extract data from Damien Arvor
soy_fallow_damien.tb <- dplyr::filter(damien.tb, label == "Soybean_Fallow2")
newlabels2.lst <- tibble::lst ("Soybean_Fallow2" =  "Soy_Fallow")
soy_fallow_damien.tb <- sits_relabel (soy_fallow_damien.tb, newlabels2.lst)
water_damien.tb <- dplyr::filter(damien.tb, label == "Water")

# remove classes from EMBRAPA
embrapa_orig2.tb <- dplyr::filter(embrapa_orig2.tb, label != "Soy_Fallow" || label != "Planted_Forest")

# add classes from Damien to EMBRAPA
embrapa_damien_all_agro.tb <- dplyr::bind_rows(embrapa_orig2.tb, soy_fallow_damien.tb)
embrapa_damien_all_agro.tb <- dplyr::bind_rows(embrapa_damien_all_agro.tb, water_damien.tb)

# test cross-validation with DTW distances and all bands

# test accuracy of TWDTW to measure distances
conf_svm_dtw.tb <- sits_kfold_validate(embrapa_damien_all_agro.tb, folds = 2,
                                       pt_method   = sits_gam(),
                                       dist_method = sits_TS_distances(distance = "dtw"),
                                       tr_method   = sits_svm (cost = 100, method = "radial"))
