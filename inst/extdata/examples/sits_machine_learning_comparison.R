# A script for testing different ML methods together with the TWDTW distances for classification of time series
# Gilberto Camara, revised 25.08.2017

#load the sits library
library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa_new.tb <- sits_getdata(file = "./inst/extdata/samples/dados_matogrosso_alex_v2.json.gz")

results <- list()

# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 8,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                   tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                           tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_accuracy(conf_svm.tb)

results[[length(results) + 1]] <- conf_svm.mx


# =============== GLM ==============================

# generalized liner model (glm)
conf_glm.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 8,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                   tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
conf_glm.mx <- sits_accuracy(conf_glm.tb)

results[[length(results) + 1]] <- conf_glm.mx

# =============== RFOR ==============================

# test accuracy of TWDTW to measure distances
conf_rfor.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 8,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                   tr_method   = sits_rfor ())
print("==================================================")
print ("== Confusion Matrix = RFOR =======================")
rfor.mx <- sits_accuracy(conf_rfor.tb)

results[[length(results) + 1]] <- conf_rfor.mx

# =============== LDA ==============================

# test accuracy of TWDTW to measure distances
conf_lda.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 8,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                   tr_method   = sits_lda ())

print("==================================================")
print ("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_accuracy(conf_lda.tb)

results[[length(results) + 1]] <- conf_lda.mx

# Linear Discriminant Analysis
conf_qda.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 8,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                   tr_method   = sits_qda())

# =============== QDA ==============================

# print the accuracy of the quadratic Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = QDA =======================")
conf_qda.mx <- sits_accuracy(conf_qda.tb)

results[[length(results) + 1]] <- conf_qda.mx

# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_fast_validate(embrapa_new.tb, folds = 5, multicores = 8,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                   tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_accuracy(conf_mlr.tb)

results[[length(results) + 1]] <- conf_mlr.mx

# =============== SVM full validate ==============================

# test accuracy of TWDTW to measure distances
conf_svm_full.tb <- sits_kfold_validate(embrapa_new.tb, folds = 5, multicores = 8,
                                        pt_method   = sits_gam(),
                                        dist_method = sits_TWDTW_dist_bands(multicores = 8),
                                        tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                                tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
svm_full.mx <- sits_accuracy(conf_svm_full.tb)

results[[length(results) + 1]] <- conf_svm_full.mx

sits_accuracy_xlsx(results, file = "./inst/extdata/results/accuracy_embrapa_new.xlsx")

