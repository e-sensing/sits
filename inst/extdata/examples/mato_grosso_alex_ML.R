library (sits)

# load data from EMBRAPA for Mato Grosso (already cleaned)
embrapa_mt.tb <- sits_getdata (file = system.file("extdata/samples/dados_matogrosso_alex.json.gz", package="sits"))

patterns_mt.tb <- sits_patterns(embrapa_mt.tb)
# plot the patterns
sits_plot (patterns_mt.tb, type = "patterns")
# select a subset of the bands

bands <- c("ndvi","evi", "nir", "mir")

# select only the time series for the bands
embrapa_mt.tb <- sits_select(embrapa_mt.tb, bands)

# test accuracy of TWDTW to measure distances
conf_svm_twdtw.tb <- sits_kfold_validate(embrapa_mt.tb, folds = 2,
                                       pt_method   = sits_gam(),
                                       dist_method = sits_TWDTW_distances(),
                                       tr_method   = sits_svm (cost = 1000, method = "radial"))

sits_accuracy(conf_svm_twdtw.tb)

newlabels2.lst <- tibble::lst (
    "Fallow_Cotton"   = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Planted_Forest"  = "Planted_Forest",
    "Soy_Cotton"      = "Soy_Cotton",
    "Soy_Coverage"    = "Soy_Coverage",
    "Soy_Crotalaria"  = "Soy_Coverage",
    "Soy_Beans"       = "Soy_Coverage",
    "Soy_Sunflower"   = "Soy_Coverage",
    "Soy_Millet"      = "Soy_Coverage",
    "Soy_Corn"        = "Soy_Corn",
    "Soy_Pasture"     = "Soy_Coverage",
    "Soy_Fallow"      = "Soy_Fallow",
    "Soy_Sorghum"     = "Soy_Coverage",
    "Water"  = "Water")

sits_accuracy(conf_svm_dtw.tb, conv = newlabels2.lst)
