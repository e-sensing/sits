# A script for testing different ML methods together with the TWDTW distances for classification of time series
# Gilberto Camara, revised 25.08.2017

#load the sits library
library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa_new.tb <- sits_getdata(file = "./inst/extdata/samples/dados_matogrosso_alex_v2.json.gz")

# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 1),
                                   tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                           tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_accuracy(conf_svm.tb)

sits_accuracy_save (conf_svm.mx, "./inst/extdata/results/embrapa_new_svm.csv")


# test accuracy of TWDTW to measure distances
conf_gbm.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 1),
                                   tr_method   = sits_gbm (n.cores = 1))
print("==================================================")
print ("== Confusion Matrix = GBM =======================")
gbm.mx <- sits_accuracy(conf_gbm.tb)

sits_accuracy_save (gbm.mx, file = "./inst/extdata/results/embrapa_new_gbm.csv")


# =============== GLM ==============================

# generalized liner model (glm)
conf_glm.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 1),
                                   tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
conf_glm.mx <- sits_accuracy(conf_glm.tb)

sits_accuracy_save (conf_glm.mx, file = "./inst/extdata/results/embrapa_new_glm.csv")

# =============== RFOR ==============================

# test accuracy of TWDTW to measure distances
conf_rfor.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 1),
                                   tr_method   = sits_rfor ())
print("==================================================")
print ("== Confusion Matrix = RFOR =======================")
rfor.mx <- sits_accuracy(conf_rfor.tb)

sits_accuracy_save(rfor.mx, file = "./inst/extdata/results/embrapa_new_rfor.csv")

# =============== LDA ==============================

# test accuracy of TWDTW to measure distances
conf_lda.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                   tr_method   = sits_lda ())

print("==================================================")
print ("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_accuracy(conf_lda.tb)

sits_accuracy_save (conf_lda.mx, file = "./inst/extdata/results/embrapa_new_lda.csv")

# Linear Discriminant Analysis
conf_qda.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 1),
                                   tr_method   = sits_qda())

# =============== QDA ==============================

# print the accuracy of the quadratic Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = QDA =======================")
conf_qda.mx <- sits_accuracy(conf_qda.tb)

sits_accuracy_save (conf_qda.mx, file = "./inst/extdata/results/embrapa_new_qda.csv")

# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 1),
                                   tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_accuracy(conf_mlr.tb)
sits_accuracy_save (conf_mlr.mx, file = "./inst/extdata/results/embrapa_new_mlr.csv")

# =============== SVM full validate ==============================

# test accuracy of TWDTW to measure distances
conf_svm_full.tb <- sits_kfold_validate(embrapa_new.tb, folds = 5, multicores = 1,
                                        pt_method   = sits_gam(),
                                        dist_method = sits_TWDTW_dist_bands(multicores = 1),
                                        tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                                tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
svm_full.mx <- sits_accuracy(conf_svm_full.tb)

sits_accuracy_save(svm_full.mx, "./inst/extdata/results/embrapa_new_conf_svm_full.csv")

