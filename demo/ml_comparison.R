devAskNewPage(ask = FALSE)

# This demo shows different machine learning methods for
# classification of time series

# load the sits library
library(sits)

# A dataset containing a tibble with time series samples
# for the Mato Grosso state in Brasil.
# The time series come from MOD13Q1 collection 6 images.
# The data set has the following classes:
# Cerrado(379 samples), Forest (131 samples),
# Pasture (344 samples), and Soy_Corn (364 samples).
data("samples_modis_4bands")

# create a list to store the results
results <- list()

# adjust the multicores parameters to suit your machine

## SVM model
print("== Accuracy Assessment = SVM =======================")
acc_svm <- sits_kfold_validate(samples_modis_4bands,
    folds = 5,
    multicores = 3,
    ml_method = sits_svm(kernel = "radial", cost = 10)
)
acc_svm$name <- "svm_10"

results[[length(results) + 1]] <- acc_svm


# =============== RFOR ==============================
print("== Accuracy Assessment = RFOR =======================")
acc_rfor <- sits_kfold_validate(samples_modis_4bands,
    folds = 5,
    multicores = 2,
    ml_method = sits_rfor(num_trees = 2000)
)
acc_rfor$name <- "rfor"

results[[length(results) + 1]] <- acc_rfor

# =============== LDA ==============================
print("== Accuracy Assessment = LDA =======================")
acc_lda <- sits_kfold_validate(samples_modis_4bands,
    folds = 5,
    multicores = 2,
    ml_method = sits_lda()
)
acc_lda$name <- "lda"
results[[length(results) + 1]] <- acc_lda


# =============== MLR ==============================
print("== Accuracy Assessment = MLR =======================")
acc_mlr <- sits_kfold_validate(samples_modis_4bands,
    folds = 5,
    multicores = 2,
    ml_method = sits_mlr()
)
acc_mlr$name <- "MLR"
results[[length(results) + 1]] <- acc_mlr

# =============== XGBOOST ==============================
# extreme gradient boosting
print("== Accuracy Assessment = XGB =======================")
acc_xgb <- sits_kfold_validate(samples_modis_4bands,
    folds = 5,
    ml_method = sits_xgboost()
)
acc_xgb$name <- "xgboost"

results[[length(results) + 1]] <- acc_xgb


sits_to_xlsx(results, file = paste0(tempdir(),"/accuracy_mt_ml.xlsx"))
