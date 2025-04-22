devAskNewPage(ask = FALSE)

# This demo shows different machine learning methods for
# classification of time series

# load the sits library
library(sits)

# load the sits library
library(sits)
if (!requireNamespace("sitsdata", quietly = TRUE)) {
    stop("Please install package sitsdata\n",
         "Please call devtools::install_github('e-sensing/sitsdata')",
         call. = FALSE
    )
}

# load the sitsdata library
library(sitsdata)

# load a dataset of time series samples for the Mato Grosso region
data("samples_matogrosso_mod13q1")


# create a list to store the results
results <- list()

# adjust the multicores parameters to suit your machine

## SVM model
print("== Accuracy Assessment = SVM =======================")
acc_svm <- sits_kfold_validate(
    samples_matogrosso_mod13q1,
    folds = 5,
    multicores = 3,
    ml_method = sits_svm(kernel = "radial", cost = 10)
)
acc_svm[["name"]] <- "svm_10"

results[[length(results) + 1]] <- acc_svm


# =============== RFOR ==============================
print("== Accuracy Assessment = RFOR =======================")
acc_rfor <- sits_kfold_validate(
    samples_matogrosso_mod13q1,
    folds = 5,
    multicores = 2,
    ml_method = sits_rfor()
)
acc_rfor[["name"]] <- "rfor"

results[[length(results) + 1]] <- acc_rfor


# =============== XGBOOST ==============================
# extreme gradient boosting
print("== Accuracy Assessment = XGB =======================")
acc_xgb <- sits_kfold_validate(
    samples_matogrosso_mod13q1,
    folds = 5,
    ml_method = sits_xgboost()
)
acc_xgb[["name"]] <- "xgboost"

results[[length(results) + 1]] <- acc_xgb


sits_to_xlsx(results, file = file.path(tempdir(), "accuracy_mt_ml.xlsx"))
