devAskNewPage(ask = FALSE)

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


# Deep Learning - MLP
print("== Accuracy Assessment = DL =======================")
acc_dl <- sits_kfold_validate(samples_modis_4bands,
    folds = 5, multicores = 1,
    ml_method = sits_deeplearning(
        layers = c(512, 512, 512),
        activation = "elu",
        dropout_rates = c(0.50, 0.40, 0.30),
        epochs = 300,
        batch_size = 128,
        validation_split = 0.2
    )
)
acc_dl$name <- "mlp_default"

results[[length(results) + 1]] <- acc_dl

# Deep Learning - FCN
print("== Accuracy Assessment = FCN =======================")
acc_fcn853 <- sits_kfold_validate(samples_modis_4bands,
    folds = 5,
    ml_method = sits_FCN(kernels = c(8, 5, 3), verbose = 1)
)
acc_fcn853$name <- "fcn_853"

results[[length(results) + 1]] <- acc_fcn853

# Deep Learning - TempCNN
print("== Accuracy Assessment = TempCNN =======================")
acc_tc <- sits_kfold_validate(samples_modis_4bands,
    folds = 5,
    ml_method = sits_TempCNN(verbose = 0)
)
acc_tc$name <- "TempCNN"

results[[length(results) + 1]] <- acc_tc

# Deep Learning - LSTM
print("== Accuracy Assessment = LSTM =======================")
acc_lstm <- sits_kfold_validate(samples_modis_4bands,
    folds = 5, multicores = 2,
    ml_method = sits_LSTM_FCN(verbose = 0)
)
acc_lstm$name <- "LSTM_FCN"

results[[length(results) + 1]] <- acc_lstm

sits_to_xlsx(results, file = paste0(tempdir(), "/accuracy_mato_grosso_dl.xlsx"))
