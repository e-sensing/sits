devAskNewPage(ask = FALSE)

#load the sits library
library(sits)

#' The input data is a tibble with 2115 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series ( list containing a tibble with the values of the time series)..

data("samples_MT_9classes")

print(samples_MT_9classes)

# the tibble 2115 samples of 9 classes of the Cerrado biome in Brazil
sits_labels(samples_MT_9classes)

# This demo shows different machine learning methods for clasification of time series

# select NDVI, EVI, NIR and MIR
samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi", "nir", "mir"))

results <- list()

# Estimate validation with svm with radial kernel

conf_svm.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                    ml_method   = sits_svm(kernel = "radial", cost = 10))

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_radial_10"

results[[length(results) + 1]] <- conf_svm.mx

# Deep Learning

conf_dl.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                  ml_method   = sits_deeplearning(
                                      units            = c(400, 200, 100, 50),
                                      activation       = 'elu',
                                      dropout_rates    = c(0.4, 0.3, 0.2, 0.15),
                                      optimizer        = keras::optimizer_adam(),
                                      epochs           = 500,
                                      batch_size       = 64,
                                      validation_split = 0.2),
                                      adj_fun = function(x) {identity(x)})

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "deep_learning"

results[[length(results) + 1]] <- conf_dl.mx

# =============== RFOR ==============================

# test accuracy of TWDTW to measure distances
conf_rfor.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                    ml_method   = sits_rfor(ntree = 5000))
print("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor"

results[[length(results) + 1]] <- conf_rfor.mx

WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_mato_grosso.xlsx"))


