# Example of classification of a series of samples defined by a CSV file

# obtain a time series from the WTSS server for a set of 30 samples
samples.tb <- sits_getdata(file = system.file("extdata/samples/samples_matogrosso.csv", package = "sits"),
                           service = "WTSS", product = "MOD13Q1", coverage = "mod13q1_512",
                           bands = c("ndvi", "evi"),  .n_save = 0)

# plot the data
sits_plot(samples.tb[1,])

# get the samples used for classifation
data("cerrado_2classes")

# classify the test data using an SVM model
class.tb <- sits_classify(samples.tb, cerrado_2classes, ml_method = sits_svm())

# plot the classification of the time series by yearly intervals
sits_plot(class.tb[1:5,], band = "ndvi")

# estimate the accuracy of the result and the confusion matrix
conf.mx <- sits_conf_matrix(class.tb)
