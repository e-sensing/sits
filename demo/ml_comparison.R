devAskNewPage(ask = FALSE)


# This demo shows different machine learning methods for clasification of time series

#load the sits library
library(sits)

#  A dataset containing a tibble with time series samples for the Mato Grosso state in Brasil.
#  The time series come from MOD13Q1 collection 6 images. The data set has the following classes:
#  Cerrado(400 samples), Fallow_Cotton (34 samples), Forest (138 samples), Pasture (370 samples),
#  Soy-Corn (398 samples),  Soy-Cotton (399 samples), Soy_Fallow (88 samples),
#  Soy_Millet (235 samples), and Soy_Sunflower (53 samples).
#  The tibble has 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#  latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#  end_date (final date of the time series), label (the class label associated to the sample),
#  coverage (the name of the coverage associated with the data),
#  time_series (list containing a tibble with the values of the time series).

data("samples_MT_9classes")
# the tibble contains 9 classes of the Cerrado biome in Brazil
sits_labels(samples_MT_9classes)

# remove classes with low number of samples
samples.tb <- sits_select(samples_MT_9classes, !(label %in% c("Fallow_Cotton", "Soy_Sunflower")))

# select NDVI, EVI, NIR and MIR
samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi", "nir", "mir"))

# create a list to store the results
results <- list()

## SVM model
conf_svm.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 2,
                                   ml_method = sits_svm(kernel = "radial", cost = 10))

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_10"

results[[length(results) + 1]] <- conf_svm.mx

# Deep Learning
conf_dl.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                  ml_method = sits_deeplearning( units = c(512, 512, 512, 512, 512),
                                                                 activation       = 'elu',
                                                                 dropout_rates    = c(0.50, 0.40, 0.35, 0.30, 0.20),
                                                                 optimizer        = keras::optimizer_adam(lr = 0.001),
                                                                 epochs           = 500,
                                                                 batch_size       = 128,
                                                                 validation_split = 0.2,
                                                                 binary_classification = FALSE))

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "dl"

results[[length(results) + 1]] <- conf_dl.mx

# =============== RFOR ==============================

conf_rfor.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                    ml_method = sits_train(samples.tb, sits_rfor(ntree = 5000)))
print("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor"

results[[length(results) + 1]] <- conf_rfor.mx



# =============== LDA ==============================
conf_lda.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                   ml_method = sits_lda())

print("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_conf_matrix(conf_lda.tb)
conf_lda.mx$name <- "lda"

results[[length(results) + 1]] <- conf_lda.mx


# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(samples.tb, folds = 5, multicores = 1,
                                   ml_method = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_conf_matrix(conf_mlr.tb)
conf_mlr.mx$name <- "mlr"

results[[length(results) + 1]] <- conf_mlr.mx


WD = getwd()

sits_toXLSX(results, file = paste0(WD, "/accuracy_cerrado.xlsx"))


