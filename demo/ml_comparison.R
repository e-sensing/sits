devAskNewPage(ask = FALSE)

# This demo shows different machine learning methods for
# clasification of time series

# load the sits library
library(sits)

#  The data contain 1,892 time series samples for the Mato Grosso state in Brasil.
#  The time series come from MOD13Q1 collection 6 images, with 6 bands
#  ("blue", "red", "nir", "mir",  "evi",  and "ndvi")
#  The data set has the following classes:
#  Cerrado(379 samples), Fallow_Cotton (29 samples), Forest (131 samples),
#  Pasture (344 samples), Soy-Corn (364 samples),  Soy-Cotton (352 samples),
#  Soy_Fallow (87 samples), Soy_Millet (186 samples),
#  and Soy_Sunflower (26 samples).
#  The tibble has 7 variables:
#  (a) longitude: East-west coordinate of the time series sample (WGS 84);
#  (b) latitude (North-south coordinate of the time series sample in WGS 84),
#  (c) start_date (initial date of the time series),
#  (d) end_date (final date of the time series),
#  (e) label (the class label associated to the sample),
#  (f) cube (the name of the cube associated with the data),
#  (g) time_series (tibble with the values of the time series).


# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
data("samples_mt_4bands")

# create a list to store the results
results <- list()

# adjust the multicores parameters to suit your machine

## SVM model
conf_svm.tb <- sits_kfold_validate(samples_mt_4bands,
    folds = 5,
    multicores = 2,
    ml_method = sits_svm(kernel = "radial", cost = 10)
)

print("== Confusion Matrix = SVM =======================")
conf_svm.mx <- sits_conf_matrix(conf_svm.tb)

conf_svm.mx$name <- "svm_10"

results[[length(results) + 1]] <- conf_svm.mx


# =============== RFOR ==============================

conf_rfor.tb <- sits_kfold_validate(samples_mt_4bands,
    folds = 5,
    multicores = 1,
    ml_method = sits_rfor(num_trees = 500)
)
print("== Confusion Matrix = RFOR =======================")
conf_rfor.mx <- sits_conf_matrix(conf_rfor.tb)
conf_rfor.mx$name <- "rfor"

results[[length(results) + 1]] <- conf_rfor.mx



# =============== LDA ==============================
conf_lda.tb <- sits_kfold_validate(samples_mt_4bands,
    folds = 5,
    multicores = 2,
    ml_method = sits_lda()
)

print("== Confusion Matrix = LDA =======================")
conf_lda.mx <- sits_conf_matrix(conf_lda.tb)
conf_lda.mx$name <- "lda"

results[[length(results) + 1]] <- conf_lda.mx


# =============== MLR ==============================
# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(samples_mt_4bands,
    folds = 5,
    multicores = 2,
    ml_method = sits_mlr()
)

# print the accuracy of the Multinomial log-linear
print("== Confusion Matrix = MLR =======================")
conf_mlr.mx <- sits_conf_matrix(conf_mlr.tb)
conf_mlr.mx$name <- "mlr"

results[[length(results) + 1]] <- conf_mlr.mx


# =============== XGBOOST ==============================
# extreme gradient boosting
conf_xgb.tb <- sits_kfold_validate(samples_mt_4bands,
    folds = 5,
    multicores = 2,
    ml_method = sits_xgboost()
)

# print the accuracy of the Multinomial log-linear
print("== Confusion Matrix = XGB =======================")
conf_xgb.mx <- sits_conf_matrix(conf_xgb.tb)
conf_xgb.mx$name <- "xgboost"

results[[length(results) + 1]] <- conf_xgb.mx


WD <- getwd()

sits_to_xlsx(results, file = "./accuracy_mt_ml.xlsx")
