# Satellite image time series package (SITS)
# Comparison of time series filtering methods

library(sits)
library(magrittr)

if (!requireNamespace("inSitu", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
      }
    devtools::install_github("e-sensing/inSitu")
}
library(inSitu)

# Select a data set containing a sits tibble
# with time series samples from Brazilian Mato Grosso State
# (Amazon and Cerrado biomes).

samples <- inSitu::br_mt_1_8K_9classes_6bands

cat("# =========== UNFILTERED DATA =============\n")
samples %>%
    sits_select(bands = c("NDVI", "EVI", "NIR", "MIR")) %>%
    sits_kfold_validate(folds = 4) %>%
    sits_conf_matrix()

cat("# =========== SAVITKSY - GOLAY FILTER =============\n")

# test savitsky golay filter
samples %>%
    sits_select(bands = c("NDVI", "EVI", "NIR", "MIR")) %>%
    sits_sgolay(order = 3, length = 5, scaling = 1) %>%
    sits_kfold_validate(folds = 4) %>%
    sits_conf_matrix()

prodes_sg.tb <- sits_sgolay(prodes_226_064, order = 3, length = 5, scaling = 1)

# compare the raw data with the Savistky Golay filter
sg1 <- sits_sgolay(prodes_226_064[1, ], order = 3, length = 5, scaling = 1)

sg1 %>%
    sits_merge(prodes_226_064[1, ]) %>%
    sits_select(bands = "EVI", "EVI.sg") %>%
    plot()

conf_sg.tb <- sits_kfold_validate(prodes_sg.tb, folds = 2)

# evaluate the accuracy of the classification with 4 classes
sits_conf_matrix(conf_sg.tb)

cat("# =========== WHITAKKER SMOOTHER =============\n")

# test whitakker filter
prodes_whit.tb <- sits_whittaker(prodes_226_064, lambda = 2.0)

# compare the raw data with the Whitakker filter
w1 <- sits_whittaker(prodes_226_064[1, ], lambda = 2.0)

w1 %>%
    sits_merge(prodes_226_064[1, ]) %>%
    sits_select(bands = c("NDVI", "NDVI.wf")) %>%
    plot()

conf_whit.tb <- sits_kfold_validate(prodes_whit.tb, folds = 2)

# evaluate the accuracy of the classification
sits_conf_matrix(conf_whit.tb)

# relabel and see assessment
sits_conf_matrix(conf_whit.tb)

cat("# =========== ENVELOPE FILTER =============\n")

# The envelope filter works with the NDVI band

prodes_ndvi.tb <- sits_select(prodes_226_064, bands = c("NDVI"))

# test envelope filter
prodes_env.tb <- sits_envelope(prodes_ndvi.tb)

# compare the raw data with the envelope filter
w1 <- sits_envelope(prodes_ndvi.tb[1, ])

w1 %>%
    sits_merge(prodes_ndvi.tb[1, ]) %>%
    sits_select(bands = c("NDVI", "NDVI.env")) %>%
    plot()

conf_env.tb <- sits_kfold_validate(prodes_env.tb, folds = 2)

# evaluate the accuracy of the classification
sits_conf_matrix(conf_env.tb)


cat("# =========== CLOUD REMOVAL FILTER =============\n")

# The could removal filter works with the NDVI band

# test whitakker filter
prodes_cf.tb <- sits_cloud_removal(prodes_ndvi.tb)

# compare the raw data with the envelope filter
w1 <- sits_cloud_removal(prodes_ndvi.tb[1, ])

w1 %>%
    sits_merge(prodes_ndvi.tb[1, ]) %>%
    sits_select(bands = c("NDVI", "NDVI.cf")) %>%
    plot()

conf_cf.tb <- sits_kfold_validate(prodes_cf.tb, folds = 2)

# evaluate the accuracy of the classification
sits_conf_matrix(conf_cf.tb)
