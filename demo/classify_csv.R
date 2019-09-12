# Example of classification of a series of samples defined by a CSV file
library(sits)
cube_wtss <- sits_cube(service = "WTSS", name = "MOD13Q1")

# obtain a time series from the WTSS server for a set of 5 samples defined in a CSV file
data <- sits_get_data(cube_wtss, file = system.file("extdata/samples/samples_matogrosso.csv", package = "sits"),
                        bands = c("ndvi", "evi"), .n_save = 0)

# plot the data
sits_plot(data[1,])

# get the samples used for classification
data("cerrado_2classes")

svm_model <- sits_train(cerrado_2classes, ml_method = sits_svm())

# classify the test data using an SVM model
class.tb <- sits_classify(data, svm_model)

# plot the classification of the time series by yearly intervals
sits_plot(class.tb[1:5,], band = "ndvi")

# estimate the accuracy of the result and the confusion matrix
conf.mx <- sits_conf_matrix(class.tb)
