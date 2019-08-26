# Satellite image time series package (SITS)
# Comparison of time series filtering methods

library(sits)
library(magrittr)

# Select a data set containing a sits tibble
# with time series samples from Brazilian Mato Grosso State
# (Amazon and Cerrado biomes).

samples <- inSitu::br_mt_1_8K_9classes_6bands

cat("# =========== UNFILTERED DATA =============\n")
samples %>%
    sits_select_bands(ndvi, evi, nir, mir) %>%
    sits_kfold_validate(folds = 4) %>%
    sits_conf_matrix()

cat ("# =========== SAVITKSY - GOLAY FILTER =============\n")

# test savitsky golay filter
samples %>%
    sits_select_bands(ndvi, evi, nir, mir) %>%
    sits_sgolay(order = 3, length = 5, scaling = 1) %>%
    sits_kfold_validate(folds = 4) %>%
    sits_conf_matrix()

prodes_sg.tb <- sits_sgolay(prodes_226_064, order = 3, length = 5, scaling = 1)

# compare the raw data with the Savistky Golay filter
sg1 <- sits_sgolay(prodes_226_064[1,], order = 3, length = 5, scaling = 1)

sg1 %>%
     sits_merge(prodes_226_064[1,]) %>%
     sits_select_bands(evi, evi.sg) %>%
     sits_plot()

conf_sg.tb <- sits_kfold_validate (prodes_sg.tb, folds = 2)

#evaluate the accuracy of the classification with 4 classes
sits_conf_matrix(conf_sg.tb)

#evaluate the accuracy of the classification with 2 classes
sits_conf_matrix(conf_sg.tb, prodes_relabel.lst)

cat ("# =========== WHITAKKER SMOOTHER =============\n")

# test whitakker filter
prodes_whit.tb <- sits_whittaker(prodes_226_064, lambda = 2.0)

# compare the raw data with the Whitakker filter
w1 <- sits_whittaker(prodes_226_064[1,], lambda = 2.0)

w1 %>%
    sits_merge(prodes_226_064[1,]) %>%
    sits_select_bands(ndvi, ndvi.whit) %>%
    sits_plot()

conf_whit.tb <- sits_kfold_validate(prodes_whit.tb, folds = 2)

#evaluate the accuracy of the classification
sits_conf_matrix(conf_whit.tb)

# relabel and see assessment
sits_conf_matrix(conf_whit.tb, prodes_relabel.lst)

cat ("# =========== ENVELOPE FILTER =============\n")

# The envelope filter works with the NDVI band

prodes_ndvi.tb <- sits_select_bands(prodes_226_064, ndvi)

# test envelope filter
prodes_env.tb <- sits_envelope(prodes_ndvi.tb)

# compare the raw data with the envelope filter
w1 <- sits_envelope(prodes_ndvi.tb[1,])

w1 %>%
    sits_merge(prodes_ndvi.tb[1,]) %>%
    sits_select_bands(ndvi, ndvi.env) %>%
    sits_plot()

conf_env.tb <- sits_kfold_validate(prodes_env.tb, folds = 2)

#evaluate the accuracy of the classification
sits_conf_matrix(conf_env.tb)

# relabel and see assessment
sits_conf_matrix(conf_env.tb, prodes_relabel.lst)


cat ("# =========== CLOUD REMOVAL FILTER =============\n")

# The could removal filter works with the NDVI band

# test whitakker filter
prodes_cf.tb <- sits_cloud_filter(prodes_ndvi.tb)

# compare the raw data with the envelope filter
w1 <- sits_cloud_filter(prodes_ndvi.tb[1,])

w1 %>%
    sits_merge(prodes_ndvi.tb[1,]) %>%
    sits_select_bands(ndvi, ndvi.cf) %>%
    sits_plot()

conf_cf.tb <- sits_kfold_validate(prodes_cf.tb, folds = 2)

#evaluate the accuracy of the classification
sits_conf_matrix(conf_cf.tb)

# relabel and see assessment
sits_conf_matrix(conf_cf.tb, prodes_relabel.lst)
