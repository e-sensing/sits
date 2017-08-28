# A script for testing different ML methods together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 25.08.2017

#load the sits library
library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa.tb <- sits_getdata(file = "./inst/extdata/samples/dados_matogrosso_alex.json.gz")

embrapa.tb <- dplyr::filter (embrapa.tb, label != "Water")

bands <- c ("ndvi", "evi", "nir", "mir")

embrapa.tb <- sits_select (embrapa.tb, bands)

# test accuracy of TWDTW to measure distances
conf_rfor.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(multicores = 1),
                                   tr_method   = sits_rfor ())
print("==================================================")
print ("== Confusion Matrix = RFOR =======================")
rfor.mx <- sits_accuracy(conf_rfor.tb)

sits_save(conf_rfor.tb, file = "./inst/extdata/results/rfor_5folds.json")

# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(multicores = 1),
                                   tr_method   = sits_svm (cost = 1000, kernel = "radial"))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
sits_accuracy(conf_svm.tb)

sits_save (conf_svm.tb, file = "./inst/extdata/results/svm_5folds.json")

# test accuracy of TWDTW to measure distances
conf_lda.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(multicores = 8),
                                   tr_method   = sits_lda ())


# print the accuracy of the SVM- 94%
print("==================================================")
print ("== Confusion Matrix = LDA =======================")
sits_accuracy(conf_lda.tb)

sits_save (conf_lda.tb, file = "./inst/extdata/results/lda_5folds.json")

# Linear Discriminant Analysis
conf_qda.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(multicores = 1),
                                   tr_method   = sits_qda())

# print the accuracy of the quadratic Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = QDA =======================")
sits_accuracy(conf_qda.tb)

sits_save (conf_qda.tb, file = "./inst/extdata/results/qda_5folds.json")

# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(multicores = 1),
                                   tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
sits_accuracy(conf_mlr.tb)
sits_save (conf_mlr.tb, file = "./inst/extdata/results/mlr_5folds.json")

# generalized liner model (glm)
conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(multicores = 1),
                                   tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
sits_accuracy(conf_glm.tb)

sits_save (conf_glm.tb, file = ".inst/extdata/results/glm_5folds.json")


