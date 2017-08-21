library (sits)

# get the data from embrapa
embrapa.tb <- sits_getdata(file = system.file("extdata/samples/embrapa_cerrado_forest.json", package="sits"))
# select the bands
bands <-  c("ndvi", "evi")
embrapa.tb <- sits_select(embrapa.tb, bands)

#get the patterns from Damien
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))
matogrosso.tb <- sits_select(matogrosso.tb, bands)

patterns_embrapa.tb <- sits_patterns(embrapa.tb)
sits_plot (patterns_embrapa.tb, type = "patterns")

patterns_damien.tb <- sits_patterns (matogrosso.tb)
sits_plot (patterns_damien.tb, type = "patterns")

labels_embrapa <- dplyr::distinct(embrapa.tb, label)
labels_damien  <- dplyr::distinct(matogrosso.tb, label)

# Since we consider that

newlabels1.lst <- tibble::lst (
    "Algodao_Pousio"  = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Pasto Limpo"     = "Pasture",
    "Pasto Sujo"      = "Pasture",
    "Reflorestamento" = "Planted_Forest",
    "Soja_Algodao"    = "Soy_Cotton",
    "Soja_Cobertura"  = "Soy_Other",
    "Soja_Crotalaria" = "Soy_Other",
    "Soja_Feijao"     = "Soy_Other",
    "Soja_Girassol"   = "Soy_Other",
    "Soja_Milheto"    = "Soy_Other",
    "Soja_Milho"      = "Soy_Corn",
    "Soja_Pasto"      = "Soy_Other",
    "Soja_Pousio"     = "Soy_Fallow",
    "Soja_Sorgo"      = "Soy_Other")

#relabel the data
embrapa1.tb <- sits_relabel(embrapa.tb, newlabels1.lst)

#remove soy_fallow from embrapa patterns
embrapa2.tb <- dplyr::filter(embrapa1.tb, label != "Soy_Fallow")

soy_fallow.tb <- dplyr::filter(matogrosso.tb, label == "Soybean_Fallow2")

newlabels2.lst <- tibble::lst ("Soybean_Fallow2" = "Soy_Fallow")

soy_fallow.tb <-  sits_relabel (soy_fallow.tb, newlabels2.lst)

embrapa2.tb <- dplyr::bind_rows(embrapa2.tb, soy_fallow.tb)

# do the baseline assessment
patterns2.tb <- sits_patterns(embrapa2.tb)

sits_plot (patterns2.tb, type = "patterns")

matches.tb <- sits_TWDTW_matches(embrapa2.tb, patterns2.tb, bands = bands, keep = TRUE)

class.tb <- sits_TWDTW_classify(matches.tb, patterns2.tb)

class.tb <- dplyr::mutate(class.tb, predicted = as.character(predicted))

class_valid.tb <- dplyr::filter(class.tb, predicted %in% c("Forest", "Pasture", "Soy_Cotton",
                                                           "Cerrado", "Fallow_Cotton", "Soy_Other",
                                                           "Planted_Forest", "Soy_Fallow", "Soy_Corn"))

# build a confusion matrix of the full predicted value
conf_twdtw.tb <- tibble::tibble (Prediction = class_valid.tb$predicted, Reference = class_valid.tb$label)

sits_accuracy(conf_twdtw.tb) # TWDTW accuracy is only 32%

obj.svm <- sits_svm(matches.tb, cost = 1000, kernel = "radial")

predict.tb <- sits_predict(matches.tb, obj.svm)

# build a confusion matrix of the full predicted
conf.tb <- tibble::tibble (Prediction = predict.tb$predicted, Reference = predict.tb$label)

sits_accuracy(conf.tb) # SVM raw accuracy is 91%

conf5.tb <- sits_kfold_validate(embrapa2.tb, tr_method = sits_svm (cost = 1000, kernel = "radial"), folds = 5)

sits_accuracy(conf5.tb) # SVM 5-fold validated accuracy is 75%

# Confusion Matrix and Statistics
#
# Reference
# Prediction       Cerrado Fallow_Cotton Forest Pasture Planted_Forest Soy_Corn Soy_Cotton Soy_Fallow
# Cerrado            287             0      8      58             22        0          1          0
# Fallow_Cotton        0            46      0       1              0        3         15          0
# Forest               6             0    107       3              8        0          0          0
# Pasture             68             3      5     971             44       12          6          0
# Planted_Forest      34             0     15      31             45        4          0          0
# Soy_Corn             0             2      0       5              1      234         39          0
# Soy_Cotton           2            17      0       8              1       24        311          0
# Soy_Fallow           0             0      0       1              0        1          0         76
# Soy_Other            3             7      3      68             14      122         28         12
# Reference
# Prediction       Soy_Other
# Cerrado                3
# Fallow_Cotton          3
# Forest                 2
# Pasture               61
# Planted_Forest        10
# Soy_Corn             122
# Soy_Cotton            36
# Soy_Fallow            10
# Soy_Other            659
#
# Overall Statistics
#
# Accuracy : 0.7419
# 95% CI : (0.7274, 0.7559)
#
# Kappa : 0.6782
#
# Statistics by Class:
#
#                           Class: Cerrado Class: Fallow_Cotton Class: Forest Class: Pasture
# Prod Acc (Sensitivity)            0.7175               0.6133        0.7754         0.8473
# Specificity                       0.9720               0.9939        0.9946         0.9217
# User Acc (Pos Pred Value)         0.7573               0.6765        0.8492         0.8299
# Neg Pred Value                    0.9659               0.9920        0.9913         0.9305
#                           Class: Planted_Forest Class: Soy_Corn Class: Soy_Cotton Class: Soy_Fallow
# Prod Acc (Sensitivity)                   0.3333          0.5850            0.7775            0.8636
# Specificity                              0.9735          0.9486            0.9732            0.9967
# User Acc (Pos Pred Value)                0.3237          0.5806            0.7794            0.8636
# Neg Pred Value                           0.9746          0.9495            0.9729            0.9967
#                            Class: Soy_Other
# Prod Acc (Sensitivity)              0.7274
# Specificity                         0.9076
# User Acc (Pos Pred Value)           0.7194
# Neg Pred Value                      0.9109
