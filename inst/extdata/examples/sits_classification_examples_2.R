# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
coverage.tb <- sits_coverageWTSS(URL,"mod13q1_512")

# choose a coverage
coverage <- "mod13q1_512"

# recover the NDVI, EVI, MIR and NIR bands
bands <- c("ndvi", "evi", "nir", "mir")

# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
samples.tb <- sits_getdata (file = system.file ("extdata/samples/samples_TO_GO_Cerrado.csv", package = "sits"),
                            URL = URL, bands = bands, coverage = coverage)


# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi", "nir", "mir"))

# CLASSIFICATION USING DISTANCES FROM DATA

# estimate distances
distances_data.tb <- sits_distances(embrapa.tb)

# estimate an SVM model for this training data
model_svm.ml <- sits_svm(distances_data.tb, kernel = "radial", cost = 10)

# classify the samples
class.tb <- sits_classify(samples.tb, embrapa.tb, model_svm.ml)

# get confusion matrix
conf.mx <- sits_conf_matrix(class.tb)
