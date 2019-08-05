devAskNewPage(ask = FALSE)


# This demo shows different machine learning methods for clasification of time series

#load the sits library
library(sits)

#  The data contain 1,892 time series samples for the Mato Grosso state in Brasil.
#  The time series come from MOD13Q1 collection 6 images, with 6 bands
#  ("blue", "red", "nir", "mir",  "evi",  and "ndvi")
#  The data set has the following classes:
#  Cerrado(379 samples), Fallow_Cotton (29 samples), Forest (131 samples), Pasture (344 samples),
#  Soy-Corn (364 samples),  Soy-Cotton (352 samples), Soy_Fallow (87 samples),
#  Soy_Millet (186 samples), and Soy_Sunflower (26 samples).
#  The tibble has 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#  latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#  end_date (final date of the time series), label (the class label associated to the sample),
#  cube (the name of the cube associated with the data),
#  time_series (list containing a tibble with the values of the time series).
# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#select the bands for classification
if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

#select the bands for classification
samples_mt <- inSitu::br_mt_1_8K_9classes_6bands

# select NDVI, EVI, NIR and MIR
samples <- sits_select_bands(samples_mt, ndvi, evi, nir, mir)

# create a list to store the results
results <- list()

## SVM model
conf_svm.tb <- sits_kfold_validate(samples, folds = 4, multicores = 2,
                                   ml_method = sits_svm(kernel = "radial", cost = 10))

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_10"

results[[length(results) + 1]] <- conf_svm.mx

# Deep Learning
conf_dl.tb <- sits_kfold_validate(samples, folds = 4, multicores = 2,
                                  ml_method = sits_deeplearning( units = c(512, 512, 512),
                                                                 activation       = 'elu',
                                                                 dropout_rates    = c(0.50, 0.40, 0.30),
                                                                 optimizer        = keras::optimizer_adam(lr = 0.001),
                                                                 epochs           = 150,
                                                                 batch_size       = 128,
                                                                 validation_split = 0.2,
                                                                 binary_classification = FALSE))

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "dl"

results[[length(results) + 1]] <- conf_dl.mx

# =============== RFOR ==============================

conf_rfor.tb <- sits_kfold_validate(samples, folds = 4, multicores = 1,
                                    ml_method = sits_rfor(num.trees = 5000))
print("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor"

results[[length(results) + 1]] <- conf_rfor.mx



# =============== LDA ==============================
conf_lda.tb <- sits_kfold_validate(samples, folds = 4, multicores = 2,
                                   ml_method = sits_lda())

print("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_conf_matrix(conf_lda.tb)
conf_lda.mx$name <- "lda"

results[[length(results) + 1]] <- conf_lda.mx


# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(samples, folds = 4, multicores = 2,
                                   ml_method = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_conf_matrix(conf_mlr.tb)
conf_mlr.mx$name <- "mlr"

results[[length(results) + 1]] <- conf_mlr.mx


WD = getwd()

sits_to_xlsx(results, file = paste0(WD, "/accuracy_cerrado.xlsx"))


