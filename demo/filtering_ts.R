# Satellite image time series package (SITS)
# Comparison of time series filtering methods

library(sits)
library(magrittr)

# Select a data set containing a sits tibble
# with time series samples from Brazilian Mato Grosso State
# (Amazon and Cerrado biomes).

samples <- samples_modis_4bands

cat("# =========== UNFILTERED DATA =============\n")
samples %>%
    sits_kfold_validate(folds = 4)

cat("# =========== SAVITKSY - GOLAY FILTER =============\n")

# test savitsky golay filter
# evaluate the accuracy of the classification with 4 classes
sg_filter <- sits_sgolay(order = 3, length = 5, scaling = 1)
samples %>%
    sits_apply(NDVI = sg_filter(NDVI), EVI = sg_filter(EVI),
               NIR = sg_filter(NIR), MIR = sg_filter(MIR)) %>%
    sits_kfold_validate(folds = 4)

cat("# =========== WHITAKKER SMOOTHER =============\n")

# test whitakker filter
# evaluate the accuracy of the classification with 4 classes
wt_filter <- sits_whittaker(lambda = 2.0)
samples %>%
  sits_apply(NDVI = wt_filter(NDVI), EVI = wt_filter(EVI),
             NIR = wt_filter(NIR), MIR = wt_filter(MIR)) %>%
  sits_kfold_validate(folds = 4)


