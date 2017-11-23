# A script for testing different ML methods together with the  distances from data method
# Gilberto Camara, revised 03.08.2017

#load the sits library
library (sits)

#load a data set for with samples for EMBRAPA data set
embrapa.tb <- readRDS(system.file ("extdata/time_series/embrapa_mt.rds", package = "sits"))

embrapa1.tb <- sits_select (embrapa.tb, bands = c("ndvi", "evi", "nir", "mir"))

results <- list()

# test accuracy of TWDTW to measure distances
conf_svm1.tb <- sits_kfold_validate(embrapa1.tb, folds = 5, timeline, multicores = 2,
                                   tr_method   = sits_svm (kernel = "radial", cost = 10))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
conf_svm1.mx <- sits_conf_matrix(conf_svm1.tb)

conf_svm1.mx$name <- "svm_10"

results[[length(results) + 1]] <- conf_svm1.mx

embrapa2.tb <- sits_select (embrapa.tb, bands = c("ndvi", "evi", "nir"))

# test accuracy of TWDTW to measure distances
conf_svm2.tb <- sits_kfold_validate(embrapa2.tb, folds = 5, timeline, multicores = 2,
                                         tr_method   = sits_svm (kernel = "radial", cost = 10))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
conf_svm2.mx <- sits_conf_matrix(conf_svm2.tb)

conf_svm2.mx$name <- "svm_10_3bands"

results[[length(results) + 1]] <- conf_svm2.mx



# =============== GLM ==============================

# generalized liner model (glm)
conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 5, timeline, multicores = 2,
                                        tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
conf_glm.mx <- sits_conf_matrix(conf_glm.tb)

conf_glm.mx$name <- "glm"

results[[length(results) + 1]] <- conf_glm.mx

# =============== RFOR ==============================

# test accuracy of TWDTW to measure distances
conf_rfor.tb <- sits_kfold_validate(embrapa.tb, folds = 5, timeline, multicores = 2,
                                         tr_method   = sits_rfor ())
print("==================================================")
print ("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor"

results[[length(results) + 1]] <- conf_rfor.mx

# =============== LDA ==============================

# test accuracy of TWDTW to measure distances
conf_lda.tb <- sits_kfold_validate(embrapa.tb, folds = 5, timeline, multicores = 2,
                                        tr_method   = sits_lda ())

print("==================================================")
print ("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_conf_matrix(conf_lda.tb)
conf_lda.mx$name <- "lda"

results[[length(results) + 1]] <- conf_lda.mx

# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(embrapa.tb, folds = 5, timeline, multicores = 2,
                                        tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_conf_matrix(conf_mlr.tb)
conf_mlr.mx$name <- "mlr"

results[[length(results) + 1]] <- conf_mlr.mx

# =============== GBM ==============================
# Gradient Boosting Machine
conf_gbm.tb <- sits_kfold_validate(embrapa.tb, folds = 5, timeline, multicores = 2,
                                        tr_method   = sits_gbm())

# print the accuracy of the Gradient Boosting Machine
print("===============================================")
print ("== Confusion Matrix = GBM =======================")
conf_gbm.mx <- sits_conf_matrix(conf_gbm.tb)
conf_gbm.mx$name <- "gbm"

results[[length(results) + 1]] <- conf_gbm.mx

# =============== SVM full validate ==============================

# test accuracy of TWDTW to measure distances
conf_svm_full.tb <- sits_kfold_validate(embrapa.tb, folds = 5, timeline, multicores = 2,
                                        tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                                tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
conf_svm_full.mx <- sits_conf_matrix(conf_svm_full.tb)
conf_svm_full.mx$name <- "svm_full"

results[[length(results) + 1]] <- conf_svm_full.mx

WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_embrapa_distance.xlsx"))

