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

embrapa_mt2.tb <- sits_relabel(embrapa_mt.tb, newlabels2.lst)

sits_accuracy(conf_svm_twdtw.tb, conv = newlabels2.lst)

# Confusion Matrix and Statistics
#
# Reference
# Prediction      Cerrado Fallow_Cotton Forest Pasture Soy_Corn Soy_Cotton Soy_Coverage Soy_Fallow
# Cerrado           353             0     14      34        0          0            2          0
# Fallow_Cotton       0            47      0       3        5         22            4          0
# Forest             15             0    120       2        0          0            1          0
# Pasture            31             4      2    1067       11          6           68          0
# Soy_Corn            0             4      0       6      294         40          134          0
# Soy_Cotton          0            14      0       8       30        318           48          0
# Soy_Coverage        1             6      2      26       60         14          645          8
# Soy_Fallow          0             0      0       0        0          0            4         80
# Water               0             0      0       0        0          0            0          0
# Reference
# Prediction      Water
# Cerrado           0
# Fallow_Cotton     0
# Forest            0
# Pasture           0
# Soy_Corn          0
# Soy_Cotton        0
# Soy_Coverage      0
# Soy_Fallow        0
# Water            15
#
# Overall Statistics
#
# Accuracy : 0.8237
# 95% CI : (0.8108, 0.8361)
#
# Kappa : 0.7784
#
# Statistics by Class:
#
#                           Class: Cerrado Class: Fallow_Cotton Class: Forest Class: Pasture
# Prod Acc (Sensitivity)            0.8825               0.6267        0.8696         0.9311
# Specificity                       0.9842               0.9903        0.9948         0.9496
# User Acc (Pos Pred Value)         0.8759               0.5802        0.8696         0.8974
# Neg Pred Value                    0.9852               0.9920        0.9948         0.9668
#                           Class: Soy_Corn Class: Soy_Cotton Class: Soy_Coverage Class: Soy_Fallow
# Prod Acc (Sensitivity)             0.7350            0.7950              0.7119            0.9091
# Specificity                        0.9419            0.9684              0.9560            0.9989
# User Acc (Pos Pred Value)          0.6151            0.7608              0.8465            0.9524
# Neg Pred Value                     0.9657            0.9740              0.9070            0.9977
# Class: Water
# Prod Acc (Sensitivity)               1
# Specificity                          1
# User Acc (Pos Pred Value)            1
# Neg Pred Value                       1

newlabels3.lst <- tibble::lst (
    "Fallow_Cotton"   = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Soy_Cotton"      = "Soy_Cotton",
    "Soy_Coverage"    = "Soy_Coverage",
    "Soy_Crotalaria"  = "Soy_Coverage",
    "Soy_Beans"       = "Soy_Coverage",
    "Soy_Sunflower"   = "Soy_Coverage",
    "Soy_Millet"      = "Soy_Coverage",
    "Soy_Corn"        = "Soy_Coverage",
    "Soy_Pasture"     = "Soy_Coverage",
    "Soy_Fallow"      = "Soy_Fallow",
    "Soy_Sorghum"     = "Soy_Coverage",
    "Water"  = "Water")

sits_accuracy(conf_svm_twdtw.tb, conv = newlabels3.lst)

# Confusion Matrix and Statistics
#
# Reference
# Prediction      Cerrado Fallow_Cotton Forest Pasture Soy_Cotton Soy_Coverage Soy_Fallow Water
# Cerrado           353             0     14      34          0            2          0     0
# Fallow_Cotton       0            47      0       3         22            9          0     0
# Forest             15             0    120       2          0            1          0     0
# Pasture            31             4      2    1067          6           79          0     0
# Soy_Cotton          0            14      0       8        318           78          0     0
# Soy_Coverage        1            10      2      32         54         1133          8     0
# Soy_Fallow          0             0      0       0          0            4         80     0
# Water               0             0      0       0          0            0          0    15
#
# Overall Statistics
#
# Accuracy : 0.8781
# 95% CI : (0.8669, 0.8886)
#
# Kappa : 0.8347
#
# Statistics by Class:
#
#     Class: Cerrado Class: Fallow_Cotton Class: Forest Class: Pasture
# Prod Acc (Sensitivity)            0.8825               0.6267        0.8696         0.9311
# Specificity                       0.9842               0.9903        0.9948         0.9496
# User Acc (Pos Pred Value)         0.8759               0.5802        0.8696         0.8974
# Neg Pred Value                    0.9852               0.9920        0.9948         0.9668
# Class: Soy_Cotton Class: Soy_Coverage Class: Soy_Fallow Class: Water
# Prod Acc (Sensitivity)               0.7950              0.8675            0.9091            1
# Specificity                          0.9684              0.9527            0.9989            1
# User Acc (Pos Pred Value)            0.7608              0.9137            0.9524            1
# Neg Pred Value                       0.9740              0.9257            0.9977            1


newlabels3.lst <- tibble::lst (
    "Fallow_Cotton"   = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Soy_Cotton"      = "Soy_Cotton",
    "Soy_Coverage"    = "Soy_Coverage",
    "Soy_Crotalaria"  = "Soy_Coverage",
    "Soy_Beans"       = "Soy_Coverage",
    "Soy_Sunflower"   = "Soy_Coverage",
    "Soy_Millet"      = "Soy_Coverage",
    "Soy_Corn"        = "Soy_Coverage",
    "Soy_Pasture"     = "Soy_Coverage",
    "Soy_Fallow"      = "Soy_Fallow",
    "Soy_Sorghum"     = "Soy_Coverage",
    "Water"  = "Water")

embrapa_mt3.tb <- sits_relabel(embrapa_mt.tb, newlabels3.lst)

embrapa_mt3.tb <- dplyr::filter (embrapa_mt3.tb, label != "Water")

# test accuracy of TWDTW to measure distances
conf_lda_twdtw.tb <- sits_kfold_validate(embrapa_mt3.tb, folds = 2,
                                         pt_method   = sits_gam(),
                                         dist_method = sits_TWDTW_distances(),
                                         tr_method   = sits_lda ())
