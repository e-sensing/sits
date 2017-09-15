# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
sits_coverageWTSS(URL,"mod13q1_512")

# choose a coverage
coverage <- "mod13q1_512"
# recover the NDVI, EVI, MIR and NIR bands
bands <- c("ndvi", "evi", "nir", "mir")

# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
series.tb <- sits_getdata(system.file ("extdata/samples/samples_matogrosso.csv", package = "sits"),
                          URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the series
sits_plot (series.tb[1,])

# retrieve a set of pre-defined patterns from an RDS file
patterns_matogrosso.tb <- readRDS(system.file("extdata/patterns/patterns_matogrosso_9classes.rds", package = "sits"))

# retrieve the distances file derived from the Mato Grosso training data set
training_data.tb <- readRDS(system.file ("extdata/models/distances_training_data_matogrosso.rds", package = "sits"))

# estimate an SVM model for this training data
model.ml <- sits_svm (training_data.tb, cost = 1000, kernel = "radial",tolerance = 0.001, epsilon = 0.1)

# classify the test data
class.tb <- sits_classify(series.tb[8:13,], patterns_matogrosso.tb, model.ml)

#classify the time series matches using yearly intervals

# plot the classification of the time series by yearly intervals
sits_plot(class.tb, patterns_matogrosso.tb, band = "ndvi")


