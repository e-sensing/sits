# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for classes "Cerrado" and "Pasture"
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits"))

#plot one time series
sits_plot (cerrado.tb[1,])

#plot all time series together to see their dispersion
# the plit highlights the median and the 25% and 75% quantiles
sits_plot (cerrado.tb, type = "together")

# extract the patterns for the cerrado (uses the default gam method)
patterns.tb <- sits_patterns(cerrado.tb)

# plot the resulting patterns
sits_plot (patterns.tb, type = "patterns")

#find the alignments of the series with the patterns with the TWDTW algorithm
matches.tb <- sits_TWDTW_matches(cerrado.tb, patterns.tb, bands = c("ndvi", "evi"), keep = TRUE)

# plot one aligment
sits_plot(matches.tb[1,], patterns.tb, type = "alignments")

# Spread TWDTW matches (not required - used to show how training works)
spread.tb <- sits_spread_matches(matches.tb)
# show how the attributes for the training are obtained
spread.tb

# use the alignments to train a support vector machine (default method for machine learning)
obj.svm <- sits_train(matches.tb, tr_method = sits_svm (cost = 1000, kernel = "radial"))
# show the resulting training object
obj.svm

# predict the values using the trained SVM
predict.tb <- sits_predict(matches.tb, obj.svm)

# build a confusion matrix of the full predicted
conf.tb <- tibble::tibble (Prediction = predict.tb$predicted, Reference = predict.tb$label)

# print the accuracy (just for verification)
sits_accuracy(conf.tb)

# perform a k-fold validation
conf_k.tb <- sits_kfold_validate(cerrado.tb, folds = 5)

# print the accuracy of the 5-fold validation
sits_accuracy(conf_k.tb)
