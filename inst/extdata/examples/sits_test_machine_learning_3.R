# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for classes "Cerrado" and "Pasture"
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json.gz", package="sits"))

patterns.tb <- sits_gam (cerrado.tb)
# perform a k-fold validation for ERP
conf_erp.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TSdistances(distance = "erp", g = 1),
                                   tr_method   = sits_svm (cost = 1000, method = "radial"))

# print the accuracy of the ERP - 88%
sits_accuracy(conf_erp.tb)

# test accuracy of TWDTW to measure distances
conf_twdtw.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_svm (cost = 1000, method = "radial"))

# print the accuracy of the TWDTW - 94%
sits_accuracy(conf_twdtw.tb)

# test accuracy of LCSS to measure distances
conf_lcss.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TSdistances(distance = "lcss", epsilon = 0.1),
                                     tr_method   = sits_svm (cost = 1000, method = "radial"))

# print the accuracy of the Longest Common Subsequence Matching - 78%
sits_accuracy(conf_lcss.tb)

# "ccor": Distance based on the cross-correlation.
conf_eucl.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TSdistances(distance = "euclidean"),
                                    tr_method   = sits_svm (cost = 1000, method = "radial"))

# print the accuracy of the partial (PACF) autocorrelation - 78%
sits_accuracy(conf_eucl.tb)
