# Satellite image time series package (SITS)
# Comparison of time series filtering methods

library(sits)
library(magrittr)

# Get data from a mixed LANDSAT 8/MODIS coverage over a cloudy area in Amazonia
# This data comes from areas classified by the PRODES project (manual interpretation)
# This is a cloudy area which represents a good test for filtering methods

data(prodes_226_064)
# Plot the NDVI band distribution for each label
sits_plot(sits_select_bands(prodes_226_064, ndvi))

#cross_validate raw series
conf.tb <- sits_kfold_validate (prodes_226_064, folds = 2)

#evaluate the accuracy of the classification
sits_conf_matrix(conf.tb)

# relabel and see new assessment with two classes only
prodes_relabel.lst <-  tibble::lst("Forest" = "Forest",
                                   "Deforestation_2014"  = "NonForest",
                                   "Deforestation_2015" = "NonForest",
                                   "Pasture"  = "NonForest")

sits_conf_matrix(conf.tb, prodes_relabel.lst)

cat ("# =========== SAVITKSY - GOLAY FILTER =============\n")

# test savitsky golay filter
prodes_sg.tb <- sits_sgolay(prodes_226_064, order = 2, scale = 1)

# compare the raw data with the Savistky Golay filter
sg1 <- sits_sgolay(prodes_226_064[1,], order = 2, scale = 1)

sg1 %>%
     sits_merge (prodes_226_064[1,]) %>%
     sits_select_bands(ndvi, ndvi.sg) %>%
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

prodes_ndvi.tb <- sits_select(prodes_226_064, bands = c("ndvi"))

# test whitakker filter
prodes_env.tb <- sits_envelope(prodes_ndvi.tb)

# compare the raw data with the Savistky Golay filter
w1 <- sits_envelope(prodes_ndvi.tb[1,])

w1 %>%
    sits_merge(prodes_ndvi.tb[1,]) %>%
    sits_select_bands(ndvi, ndvi.env) %>%
    sits_plot()

conf_env.tb <- sits_kfold_validate (prodes_env.tb, folds = 2)

#evaluate the accuracy of the classification
sits_conf_matrix(conf_env.tb)

# relabel and see assessment
sits_conf_matrix(conf_env.tb, prodes_relabel.lst)


cat ("# =========== CLOUD REMOVAL FILTER =============\n")

# The could removal filter works with the NDVI band

# test whitakker filter
prodes_cf.tb <- sits_cloud_filter(prodes_ndvi.tb)

# compare the raw data with the Savistky Golay filter
w1 <- sits_cloud_filter(prodes_ndvi.tb[1,])

w1 %>%
    sits_merge (prodes_ndvi.tb[1,]) %>%
    sits_select_bands(ndvi, ndvi.cf.whit) %>%
    sits_plot()

conf_cf.tb <- sits_kfold_validate (prodes_cf.tb, folds = 2)

#evaluate the accuracy of the classification
sits_conf_matrix(conf_cf.tb)

# relabel and see assessment
sits_conf_matrix(conf_cf.tb, prodes_relabel.lst)
