devAskNewPage(ask = FALSE)


# This demo shows different machine learning methods for clasification of time series

#load the sits library
library(sits)

# The dataset contains a sits tibble with 33 K time series samples from Brazilian Amazonia biome.
# The samples are from the work of Ana Rorato, combined with agricultural data provided by EMBRAPA.
# There are samples of 12 classes ("Fallow_Cotton", "Forest", "Millet_Cotton", "Pasture", "Savanna",
# "Savanna_Roraima", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower" and "Wetlands").
# Each time series covers 12 months (23 data points) from the MOD13Q1 product,
# in 4 bands ("ndvi", "evi", "nir", "mir").

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#select the bands for classification
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

#select the bands for classification
samples <- inSitu::amazonia_33K_12classes_4bands

# create a list to store the results
results <- list()

## SVM model
conf_svm.tb <- sits_kfold_validate(samples, folds = 5, multicores = 1,
                                   ml_method = sits_svm(kernel = "radial", cost = 10))

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_radial_r10"

results[[length(results) + 1]] <- conf_svm.mx

# =============== RFOR ==============================

conf_rfor.tb <- sits_kfold_validate(samples, folds = 5, multicores = 1,
                                    ml_method = sits_rfor(num.trees = 5000))
print("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor_5000"

results[[length(results) + 1]] <- conf_rfor.mx

# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
                                   ml_method = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_conf_matrix(conf_mlr.tb)
conf_mlr.mx$name <- "mlr"

results[[length(results) + 1]] <- conf_mlr.mx

# Deep Learning - MLP
conf_dl.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
                                  ml_method = sits_deeplearning(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "mlp_default"

results[[length(results) + 1]] <- conf_dl.mx


# Deep Learning - MLP - 3 layers
conf_dl3.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
                                  ml_method = sits_deeplearning(layers = c(512, 512, 512),
                                                                dropout_rates    = c(0.50, 0.40, 0.30),
                                                                verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_dl3.mx <- sits_conf_matrix(conf_dl3.tb)

conf_dl3.mx$name <- "mlp_3-layers"

results[[length(results) + 1]] <- conf_dl3.mx


# Deep Learning - FCN
conf_fcn.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
                                  ml_method = sits_FCN(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_fcn.mx <- sits_conf_matrix(conf_fcn.tb)

conf_fcn.mx$name <- "fcn_default"

results[[length(results) + 1]] <- conf_fcn.mx

# Deep Learning - FCN
conf_fcn853.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
                                   ml_method = sits_FCN(kernels = c(8, 5, 3),verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_fcn853.mx <- sits_conf_matrix(conf_fcn853.tb)

conf_fcn853.mx$name <- "fcn_853"

results[[length(results) + 1]] <- conf_fcn853.mx

# Deep Learning - ResNet
conf_rn.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
                                      ml_method = sits_ResNet(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_rn.mx <- sits_conf_matrix(conf_rn.tb)

conf_rn.mx$name <- "ResNet"

results[[length(results) + 1]] <- conf_rn.mx

# Deep Learning - TempCNN
conf_tc.tb <- sits_kfold_validate(samples, folds = 5, multicores = 32,
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
