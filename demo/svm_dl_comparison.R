devAskNewPage(ask = FALSE)

#load the sits library
library(sits)

#load a data set for with samples for land cover classes for the Mato Grosso state of Brazil
data("cerrado_13classes_modis_col5")

# This demo shows different machine learning methods for clasification of time series

# select NDVI, EVI, NIR and MIR
samples.tb <- sits_select(cerrado_13classes_modis_col5, bands = c("ndvi", "evi", "nir", "mir"))

results <- list()

conf_svm.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                    ml_method   = sits_svm(kernel = "radial", cost = 10))

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_10"

results[[length(results) + 1]] <- conf_svm.mx

# Deep Learning

conf_dl.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                  ml_method   = sits_deeplearning(
                                      units            = c(400, 200),
                                      activation       = 'relu',
                                      dropout_rates    = c(0.4, 0.3),
                                      optimizer        = keras::optimizer_adam(),
                                      epochs           = 150,
                                      batch_size       = 128,
                                      validation_split = 0.2),
                                  adj_fun = function(x) {identity(x)})

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "dl_400_200_relu_040_030_adam"

results[[length(results) + 1]] <- conf_dl.mx

# Deep Learning with LTSM

conf_ltsm.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                      ml_method        = sits_deeplearning_ltsm(
                                      units            = c(400, 200,100),
                                      activation       = 'tanh',
                                      dropout_rates    = c(0.4, 0.3, 0.2),
                                      optimizer        = keras::optimizer_adam(),
                                      epochs           = 150,
                                      batch_size       = 128,
                                      validation_split = 0.2),
                                  adj_fun = function(x) {identity(x)})

print("== Confusion Matrix = DL-LTSM =======================")
conf_ltsm.mx <- sits_conf_matrix(conf_ltsm.tb)

conf_ltsm.mx$name <- "dl_ltsm_400_200_relu_040_030_adam"

results[[length(results) + 1]] <- conf_ltsm.mx

WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_cerrado_1.xlsx"))


