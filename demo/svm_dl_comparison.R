devAskNewPage(ask = FALSE)

#load the sits library
library(sits)

#load a data set for with samples for land cover classes for the Mato Grosso state of Brazil
data("samples_MT_9classes")

# This demo shows different machine learning methods for clasification of time series

# select NDVI, EVI, NIR and MIR
samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi", "nir", "mir"))

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
                                      units            = c(400, 200, 100),
                                      activation       = 'selu',
                                      dropout_rates    = c(0.35, 0.3, 0.2),
                                      optimizer        = keras::optimizer_adam(lr = 0.001),
                                      epochs           = 100,
                                      batch_size       = 128,
                                      validation_split = 0.2),
                                  adj_fun = function(x) {identity(x)})

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "deep_learning"

results[[length(results) + 1]] <- conf_dl.mx

WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_matogrosso.xlsx"))


