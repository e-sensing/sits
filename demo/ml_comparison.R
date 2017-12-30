# A script for testing different ML methods
#

devAskNewPage(ask = FALSE)

#load the sits library
library (sits)

#load a data set for with samples for land cover classes for the Mato Grosso state of Brazil
data ("samples_MT_9classes")

# This demo shows different machine learning methods for clasification of time series

# select NDVI, EVI, NIR and MIR
samples.tb <- sits_select (samples_MT_9classes, bands = c("ndvi", "evi", "nir", "mir"))

results <- list()

conf_svm1.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                   ml_method   = sits_svm (kernel = "radial", cost = 10))

print ("== Confusion Matrix = SVM =======================")
conf_svm1.mx <- sits_conf_matrix(conf_svm1.tb)

conf_svm1.mx$name <- "svm_10"

results[[length(results) + 1]] <- conf_svm1.mx

# =============== GLM ==============================

# generalized liner model (glm)
conf_glm.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                        ml_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print ("== Confusion Matrix = GLM  =======================")
conf_glm.mx <- sits_conf_matrix(conf_glm.tb)

conf_glm.mx$name <- "glm"

results[[length(results) + 1]] <- conf_glm.mx

# =============== RFOR ==============================

# test accuracy of TWDTW to measure distances
conf_rfor.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                         ml_method   = sits_rfor ())
print ("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor"

results[[length(results) + 1]] <- conf_rfor.mx

# =============== LDA ==============================

# test accuracy of TWDTW to measure distances
conf_lda.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                        ml_method   = sits_lda ())

print ("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_conf_matrix(conf_lda.tb)
conf_lda.mx$name <- "lda"

results[[length(results) + 1]] <- conf_lda.mx

# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                        ml_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print ("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_conf_matrix(conf_mlr.tb)
conf_mlr.mx$name <- "mlr"

results[[length(results) + 1]] <- conf_mlr.mx

# =============== GBM ==============================
# Gradient Boosting Machine
conf_gbm.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                        ml_method   = sits_gbm())

# print the accuracy of the Gradient Boosting Machine
print ("== Confusion Matrix = GBM =======================")
conf_gbm.mx <- sits_conf_matrix(conf_gbm.tb)
conf_gbm.mx$name <- "gbm"

results[[length(results) + 1]] <- conf_gbm.mx

# Save the results in a XLSX spreadsheet

WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_matogrosso.xlsx"))

