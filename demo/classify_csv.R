# Example of classification of a series of samples defined by a CSV file
library(sits)
cube_wtss <- sits_cube(type  = "WTSS",
                       URL   = "http://www.esensing.dpi.inpe.br/wtss/",
                       name  = "MOD13Q1")

if (purrr::is_null(cube_wtss))
    stop("WTSS server not responding")

# obtain a time series from the WTSS server for a set of 5 samples defined in a CSV file
data <- sits_get_data(cube_wtss,
            file = system.file("extdata/samples/samples_matogrosso.csv",
                               package = "sits"),
            bands = c("ndvi", "evi"), .n_save = 0)

# plot the data
plot(data[1,])

# get the samples used for classification
data("cerrado_2classes")

svm_model <- sits_train(cerrado_2classes, ml_method = sits_svm())

# classify the test data using an SVM model
class.tb <- sits_classify(data, svm_model)

# plot the classification of the time series by yearly intervals
plot(class.tb[1:5,], bands = c("ndvi", "evi"))

# estimate the accuracy of the result and the confusion matrix
conf.mx <- sits_conf_matrix(class.tb)
