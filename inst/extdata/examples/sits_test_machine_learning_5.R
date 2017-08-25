# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa2.tb <- sits_getdata(file = "inst/extdata/samples/dados_matogrosso_alex.json.gz")

embrapa2.tb <- dplyr::filter (embrapa.tb, label != "Water")

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
    "Soy_Sorghum"     = "Soy_Coverage")

embrapa2.tb <- sits_relabel (embrapa2.tb, newlabels3.lst)

# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_svm(cost = 1000, kernel = "radial"))

sits_accuracy(conf_svm.tb)

# test accuracy of TWDTW to measure distances
conf_lda2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_lda ())

# print the accuracy of the SVM- 94%
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
sits_accuracy(conf_lda2.tb)

# Linear Discriminant Analysis
conf_qda2.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_qda())

# print the accuracy of the Linear Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = LDA =======================")
sits_accuracy(conf_qda2.tb)

# "multinomial log-linear (mlr)
conf_mlr2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
sits_accuracy(conf_mlr2.tb)

# generalized liner model (glm)
conf_glm2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
sits_accuracy(conf_glm2.tb)

# Random Forest (rfor)
conf_rfor.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_rfor())

# Random Forest (rfor)
print("===============================================")
print ("== Confusion Matrix = RFOR  =======================")
sits_accuracy(conf_rfor.tb)


# test accuracy of TWDTW to measure distances
conf_svm_dtw.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TS_distances(distance = "dtw"),
                                   tr_method   = sits_svm (cost = 100, method = "radial"))

sits_accuracy(conf_svm_dtw.tb)
