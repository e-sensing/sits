# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for classes "Cerrado" and "Pasture"
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json.gz", package="sits"))

# perform a k-fold validation
conf_k.tb <- sits_kfold_validate(cerrado.tb, folds = 2, multicores = 1,
                                 pt_method = sits_gam(),
                                 dist_method = sits_TWDTW_distances(),
                                 tr_method = sits_rfor())

# print the accuracy of the 5-fold validation
sits_accuracy(conf_k.tb)
