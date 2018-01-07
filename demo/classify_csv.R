# Example of classification of a series of samples defined by a CSV file

# Set the URL of the WTSS service
URL <- "http://www.dpi.inpe.br/tws/wtss"

# choose a coverage
coverage <- "mod13q1_512"
bands <-  c("ndvi", "evi", "nir", "mir")

# obtain a time series from the WTSS server for these samples
samples.tb <- sits_getdata (file = system.file ("extdata/samples/samples_matogrosso.csv", package = "sits"),
                            URL = URL, bands = bands, coverage = coverage)

# plot the data
sits_plot(samples.tb[1,])

# get the samples used for classifation
data("samples_MT_9classes")

samples_MT_9classes <- sits_select (samples_MT_9classes, bands = c("ndvi", "evi", "nir", "mir"))

# classify the test data using a LASSO model
class.tb <- sits_classify(samples.tb, samples_MT_9classes, ml_method = sits_glm())

# plot the classification of the time series by yearly intervals
sits_plot(class.tb[1:5,], band = "ndvi")

# estimate the accuracy of the result and the confusion matrix
conf.mx <- sits_conf_matrix (class.tb)
