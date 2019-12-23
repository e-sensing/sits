devAskNewPage(ask = FALSE)


#  This demo shows a k-fold validation of different machine learning methods for clasification of time series
#
# The dataset used is a sits tibble with 33 K time series samples
# from Brazilian Amazonia biome. The samples are from the work of Ana Rorato,
# combined with agricultural data provided by EMBRAPA.
# There are samples of 12 classes ("Fallow_Cotton", "Forest", "Millet_Cotton",
# "Pasture", "Savanna", "Savanna_Roraima", "Soy_Corn", "Soy_Cotton",
# "Soy_Fallow", "Soy_Millet", "Soy_Sunflower" and "Wetlands").
# Each time series covers 12 months (23 data points) from the MOD13Q1 product,
# in 4 bands ("ndvi", "evi", "nir", "mir").

#load the sits library
library(sits)
# load the inSitu library
library(inSitu)
# load the Amazonia data set
download.file("https://www.dropbox.com/s/g8n2yb8xeh80ph4/samples3.rds?dl=1",
              destfile = paste0(tempdir(),"/samples_amazonia.rds"))

samples <- readRDS(paste0(tempdir(),"/samples_amazonia.rds"))
# clean the data
samples <- dplyr::select(samples, -id_sample, -cluster_label )
samples <- dplyr::rename(samples, cube = coverage)

samples <- sits_select_bands(samples, ndvi, evi, nir, mir)
# create a list to store the results
results <- list()

## SVM model
conf_svm.tb <- sits_kfold_validate(samples, folds = 5,
                         ml_method = sits_svm(kernel = "radial", cost = 10))

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_radial_r10"

results[[length(results) + 1]] <- conf_svm.mx

# =============== RFOR ==============================

conf_rfor.tb <- sits_kfold_validate(samples, folds = 5,
                                    ml_method = sits_rfor(num_trees = 5000))
print("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor_5000"

results[[length(results) + 1]] <- conf_rfor.mx

# =============== XGBOOST  ==============================
#
conf_xgb.tb <- sits_kfold_validate(samples, folds = 5,
                                   ml_method = sits_xgboost())

# print the accuracy
print("== Confusion Matrix = XGBOOST  =======================")
conf_xgb.mx <- sits_conf_matrix(conf_xgb.tb)
conf_xgb.mx$name <- "xgboost"

results[[length(results) + 1]] <- conf_xgb.mx

# Deep Learning - MLP
conf_dl.tb <- sits_kfold_validate(samples, folds = 5,
                                  ml_method = sits_deeplearning())

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "mlp_default"

results[[length(results) + 1]] <- conf_dl.mx


# Deep Learning - FCN
conf_fcn.tb <- sits_kfold_validate(samples, folds = 5,
                                   ml_method = sits_FCN(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_fcn.mx <- sits_conf_matrix(conf_fcn.tb)

conf_fcn.mx$name <- "fcn_default"

results[[length(results) + 1]] <- conf_fcn.mx

# Deep Learning - FCN
conf_fcn853.tb <- sits_kfold_validate(samples, folds = 5,
                                   ml_method = sits_FCN(kernels = c(8, 5, 3),verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_fcn853.mx <- sits_conf_matrix(conf_fcn853.tb)

conf_fcn853.mx$name <- "fcn_853"

results[[length(results) + 1]] <- conf_fcn853.mx

# Deep Learning - ResNet
conf_rn.tb <- sits_kfold_validate(samples, folds = 5,
                                      ml_method = sits_ResNet(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_rn.mx <- sits_conf_matrix(conf_rn.tb)

conf_rn.mx$name <- "ResNet"

results[[length(results) + 1]] <- conf_rn.mx

# Deep Learning - TempCNN
conf_tc.tb <- sits_kfold_validate(samples, folds = 5,
                                  ml_method = sits_TempCNN(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_tc.mx <- sits_conf_matrix(conf_tc.tb)

conf_tc.mx$name <- "TempCNN"

results[[length(results) + 1]] <- conf_tc.mx

WD = getwd()

# Deep Learning - LSTM
conf_lc.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
                                  ml_method = sits_LSTM_FCN(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_lc.mx <- sits_conf_matrix(conf_lc.tb)

conf_lc.mx$name <- "LSTM_FCN"

results[[length(results) + 1]] <- conf_lc.mx

WD = getwd()

sits_to_xlsx(results, file = paste0(WD, "/accuracy_amazonia.xlsx"))
