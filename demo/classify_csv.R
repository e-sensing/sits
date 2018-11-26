# Example of classification of a series of samples defined by a CSV file

coverage_wtss <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")

# obtain a time series from the WTSS server for a set of 30 samples
data.tb <- sits_get_data(file = system.file("extdata/samples/samples_matogrosso.csv", package = "sits"),
                           coverage = coverage_wtss,
                           bands = c("ndvi", "evi"),
                           .n_save = 0)

# plot the data
sits_plot(data.tb[1,])

# get the samples used for classification
data("cerrado_2classes")

svm_model <- sits_train(cerrado_2classes, ml_method = sits_svm())

# classify the test data using an SVM model
class.tb <- sits_classify(data.tb, svm_model)

# plot the classification of the time series by yearly intervals
sits_plot(class.tb[1:5,], band = "ndvi")

# estimate the accuracy of the result and the confusion matrix
conf.mx <- sits_conf_matrix(class.tb)
