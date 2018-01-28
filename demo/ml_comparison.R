devAskNewPage(ask = FALSE)

#load the sits library
library(sits)

#  Download a data set with samples of 13 classes for the Cerrado biome in Brazil
#  The time series comes from MOD13Q1 collection 6 images with six bands
#  (ndvi, evi, blue, red, nir, mir)
#  The tibble has 11,744 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#  latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#  end_date (final date of the time series), label (the class label associated to the sample),
#  coverage (the name of the coverage associated with the data),
#  time_series (list containing a tibble with the values of the time series).

samples <- paste0("https://www.dropbox.com/s/addv5lxbpjm85jr/cerrado_13classes_modis_col6.rda?raw=1")

download.file(samples, destfile = "./cerrado_13classes_modis_col6.rda")
load(file = "./cerrado_13classes_modis_col6.rda")

# the loaded file is called "samples.tb"
print(samples.tb)

# the tibble contains 11,743 time series of 13 classes of the Cerrado biome in Brazil
sits_labels(samples.tb)

# This demo shows different machine learning methods for clasification of time series

# select NDVI, EVI, NIR and MIR
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi", "nir", "mir"))

#remove samples with labels

samples.tb <- sits_select(samples.tb, !(label %in% c("Soy_Sunflower", "Corn_Cotton")))

results <- list()

conf_svm.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                    ml_method   = sits_svm(kernel = "radial", cost = 10))

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_10"

results[[length(results) + 1]] <- conf_svm.mx

conf_svm2.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                   ml_method   = sits_svm(kernel = "radial", cost = 100))

print("== Confusion Matrix = SVM =======================")
conf_svm2.mx <- sits_conf_matrix(conf_svm2.tb)

conf_svm2.mx$name <- "svm_100"

results[[length(results) + 1]] <- conf_svm2.mx

# Deep Learning

conf_dl.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                  ml_method   = sits_deeplearning(
                                      units            = c(300, 200, 100),
                                      activation       = 'relu',
                                      dropout_rates    = c(0.4, 0.3, 0.2),
                                      optimizer        = keras::optimizer_adam(),
                                      epochs           = 300,
                                      batch_size       = 128,
                                      validation_split = 0.2),
                                      adj_fun = function(x) {identity(x)})

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "adam_300_200_100_040_020_020"

results[[length(results) + 1]] <- conf_dl.mx

# =============== RFOR ==============================

# test accuracy of TWDTW to measure distances
conf_rfor.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                    ml_method   = sits_rfor(ntree = 2000))
print("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor"

results[[length(results) + 1]] <- conf_rfor.mx



# =============== LDA ==============================

# test accuracy of TWDTW to measure distances
conf_lda.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                   ml_method   = sits_lda())

print ("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_conf_matrix(conf_lda.tb)
conf_lda.mx$name <- "lda"

results[[length(results) + 1]] <- conf_lda.mx

# =============== QDA ==============================

# test accuracy of TWDTW to measure distances
conf_qda.tb <- sits_kfold_validate(samples.tb, folds = 2, multicores = 1,
                                   ml_method   = sits_qda())

print("== Confusion Matrix = QDA =======================")
conf_qda.mx <- sits_conf_matrix(conf_qda.tb)
conf_qda.mx$name <- "qda"

results[[length(results) + 1]] <- conf_qda.mx

# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
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

WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_cerrado_1.xlsx"))


