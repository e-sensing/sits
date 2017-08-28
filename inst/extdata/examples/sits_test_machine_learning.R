# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for classes "Cerrado" and "Pasture"
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json.gz", package="sits"))

patterns.tb <- sits_gam(cerrado.tb)

distances.tb <- sits_TWDTW_distances(cerrado.tb, patterns.tb)

gbm_model <- sits_gbm (distances.tb)

cerrado.tb$predicted <- as.character(gbm_model(distances.tb))

# test accuracy of TWDTW to measure distances
conf_gbm.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_gbm ())

# print the accuracy of the TWDTW - 94%
sits_accuracy(conf_twdtw.tb)

# test accuracy of DTW to measure distances
conf_rfor.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_rfor ())

sits_accuracy(conf_rfor.tb)
