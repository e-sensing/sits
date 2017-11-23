# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

# select the bands for classification
embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi"))

# CLASSIFICATION USING TS DISTANCES

# estimate distances
distances_dtw.tb <- sits_TS_distances (embrapa.tb)

# estimate an SVM model for this training data
model_svm.ml <- sits_svm(distances_dtw.tb, kernel = "radial", cost = 10)

# obtain indepedent samples
data.tb <- readRDS("./inst/extdata/time_series/cerrado_agriculture.rds")

data.tb <- sits_select(data.tb, bands = c("ndvi", "evi"))

# classify the samples
class.tb <- sits_classify(data.tb[1,], embrapa.tb, model_svm.ml,
                          dist_method = sits_TS_distances(distance = "dtw"))

# get confusion matrix
conf.mx <- sits_conf_matrix(class.tb)
