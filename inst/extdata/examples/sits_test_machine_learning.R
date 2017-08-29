# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for classes "Cerrado" and "Pasture"
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json.gz", package="sits"))

patterns.tb <- sits_patterns(cerrado.tb)

sits_plot(patterns.tb, type = "patterns")

distances.tb <- sits_TWDTW_distances(cerrado.tb, patterns.tb)

h2o::h2o.init(nthreads = -1)

h2o_model <- sits_deep_learning(distances.tb)

categorias.h2odpl.pred <- h2o.predict(categorias.h2odpl, newdata = h2o_dadosTest)$predict;

result <- h2o_model(distances.tb)

distances.tb$predicted <- result

sits_accuracy(distances.tb)

# test accuracy of TWDTW to measure distances
conf_dl.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_deep_learning ())

# print the accuracy of the TWDTW - 94%
sits_accuracy(conf_dl.tb)

h2o.shutdown()

# test accuracy of DTW to measure distances
conf_rfor.tb <- sits_kfold_validate(cerrado.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_rfor ())

sits_accuracy(conf_rfor.tb)
