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
acc_ltae <- sits_kfold_validate(samples_modis_4bands,
  folds = 5,
  ml_method = sits_LightTAE()
)
acc_ltae$name <- "LightTAE"

results[[length(results) + 1]] <- acc_ltae

# Deep Learning - TempCNN
print("== Accuracy Assessment = TempCNN =======================")
acc_tc <- sits_kfold_validate(samples_modis_4bands,
  folds = 5,
  ml_method = sits_TempCNN()
)
acc_tc$name <- "TempCNN"

results[[length(results) + 1]] <- acc_tc

# Deep Learning - ResNet
print("== Accuracy Assessment = ResNet =======================")
acc_rn <- sits_kfold_validate(samples_modis_4bands,
  folds = 5,
  ml_method = sits_ResNet()
)
acc_rn$name <- "ResNet"

results[[length(results) + 1]] <- acc_rn

sits_to_xlsx(results, file = paste0(tempdir(), "/accuracy_mato_grosso_dl.xlsx"))
