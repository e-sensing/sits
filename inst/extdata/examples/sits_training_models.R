# This example describes different options for training models for classifications

library(sits)


# This is a set of traning data that has been validated by field work
embrapa_mt.tb <- readRDS(system.file ("extdata/time_series/embrapa_mt.rds", package = "sits"))

# find out which bands exist in the data
sits_bands(embrapa_mt.tb)

# select four bands for training
embrapa_mt2.tb <- sits_select(embrapa_mt.tb, bands = c("ndvi", "evi", "nir", "mir") )


## Option 1 - Derive patterns from GAM method, then calculate distances using TWDTW

# obtain a set of patterns for these samples using GAM
patterns.tb <- sits_gam(embrapa_mt2.tb)
# plot the patterns
sits_plot (patterns.tb, type = "patterns")

# find the matches between the patterns and the time series using the TWDTW algorithm
distances_twdtw.tb <- sits_TWDTW_distances (embrapa_mt2.tb, patterns.tb)
saveRDS (distances_twdtw.tb, file = "./inst/extdata/models/embrapa_mt_distances_twdtw.rds")

# find a model on the training data set
model.ml <- sits_svm (distances_twdtw.tb, cost = 10, kernel = "radial",tolerance = 0.001, epsilon = 0.1)
saveRDS (model.ml, file = "./inst/extdata/models/svm_embrapa_mt_twdtw_radial_cost_10.rds")

# find the matches between the patterns and the time series using the DTW algorithm
distances_dtw.tb <- sits_TS_distances (embrapa_mt2.tb, patterns.tb)
saveRDS(distances_dtw.tb, file ="./inst/extdata/models/embrapa_mt_distances_dtw.rds")

# find a model on the training data set
model_dtw.ml <- sits_svm (distances_dtw.tb, cost = 10, kernel = "radial",tolerance = 0.001, epsilon = 0.1)
saveRDS (model_dtw.ml, file = "./inst/extdata/models/svm_embrapa_mt_dtw_radial_cost_10.rds")

# find the matches between the patterns and the time series using the spread function
embrapa_plus.tb <- sits_apply(embrapa_mt2.tb, fun = function (band) band + 3.0)
distances_spread.tb <- sits_spread_time_series (embrapa_plus.tb)
saveRDS(distances_spread.tb, file ="./inst/extdata/models/embrapa_mt_distances_spread.rds")

# find a model for the training data set

model_spread.ml <- sits_svm (distances_spread.tb, cost = 10, kernel = "radial",tolerance = 0.001, epsilon = 0.1)
saveRDS (model_spread.ml, file = "./inst/extdata/models/svm_embrapa_mt_spread_radial_cost_10.rds")
