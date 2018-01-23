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
                                      units            = c(1024, 1024, 512),
                                      activation       = 'relu',
                                      dropout_rates    = c(0.5, 0.5, 0.3),
                                      optimizer        = keras::optimizer_nadam(),
                                      epochs           = 200,
                                      batch_size       = 128,
                                      validation_split = 0.2),
                                  adj_fun = function(x) {identity(x)})

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "nadam_512_256_128_025_025_025"

results[[length(results) + 1]] <- conf_dl.mx

WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_cerrado_1.xlsx"))


