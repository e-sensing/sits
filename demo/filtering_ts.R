# Satellite image time series package (SITS)
# Comparison of time series filtering methods

library(sits)
library(magrittr)

# load the sitsdata library
if (!requireNamespace("sitsdata", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("e-sensing/sitsdata")
}
library(sitsdata)

# Select a data set containing a sits tibble
# with time series samples from Brazilian Mato Grosso State
# (Amazon and Cerrado biomes).

samples <- sitsdata::br_mt_1_8K_9classes_6bands

cat("# =========== UNFILTERED DATA =============\n")
samples %>%
    sits_select(bands = c("NDVI", "EVI", "NIR", "MIR")) %>%
    sits_kfold_validate(folds = 4) %>%
    sits_conf_matrix()

cat("# =========== SAVITKSY - GOLAY FILTER =============\n")

# test savitsky golay filter
# evaluate the accuracy of the classification with 4 classes
samples %>%
    sits_sgolay(order = 3, length = 5, scaling = 1) %>%
    sits_kfold_validate(folds = 4) %>%
    sits_conf_matrix()

cat("# =========== WHITAKKER SMOOTHER =============\n")

# test whitakker filter
# evaluate the accuracy of the classification with 4 classes
samples %>%
  sits_whittaker(lambda = 2.0) %>%
  sits_kfold_validate(folds = 4) %>%
  sits_conf_matrix()


