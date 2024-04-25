devAskNewPage(ask = FALSE)

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
# Deep Learning - MLP

print("== Accuracy Assessment = DL =======================")
acc_ltae <- sits_kfold_validate(
    samples_matogrosso_mod13q1,
    folds = 5,
    ml_method = sits_lighttae()
)
acc_ltae[["name"]] <- "LightTAE"

results[[length(results) + 1]] <- acc_ltae

# Deep Learning - TempCNN
print("== Accuracy Assessment = TempCNN =======================")
acc_tc <- sits_kfold_validate(
    samples_matogrosso_mod13q1,
    folds = 5,
    ml_method = sits_tempcnn()
)
acc_tc[["name"]] <- "TempCNN"

results[[length(results) + 1]] <- acc_tc

# Deep Learning - ResNet
print("== Accuracy Assessment = ResNet =======================")
acc_rn <- sits_kfold_validate(
    samples_matogrosso_mod13q1,
    folds = 5,
    ml_method = sits_resnet()
)
acc_rn[["name"]] <- "ResNet"

results[[length(results) + 1]] <- acc_rn

sits_to_xlsx(results, file = file.path(tempdir(), "/accuracy_mato_grosso_dl.xlsx"))
