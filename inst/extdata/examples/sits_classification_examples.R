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

# a point in the transition forest pasture in Northern MT
long <- -55.57320
lat <- -11.50566

# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
series.tb <- sits_getdata(system.file ("extdata/samples/samples_matogrosso.csv", package = "sits"),
                          URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the series
sits_plot (series.tb[1:3,])

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi", "nir", "mir"))

# We now compare two methods of classification, given the
# (a) Classification using the TWDTW distances
# (b) Classification using the distances from data method

# (A) CLASSIFICATION USING THE TWDTW DISTANCES

# This function generates the patterns for the MatoGrosso data using the GAM model
patterns_gam.tb <- sits_gam (embrapa.tb)

# Plot the patterns using GAM
sits_plot(patterns_gam.tb, type = "patterns")

# This function obtains a traning data set of attributes for the MatoGrosso data using the TWTDW algorithm
# we are not running this function as it takes a lot of time, so we
# retrieve a set of pre-defined patterns from an RDS file
if (FALSE) {
    distances_twdtw.tb <- sits_TWDTW_distances(embrapa.tb, patterns_gam.tb, multicores = 2)
    saveRDS(distances_twdtw.tb, "./inst/extdata/models/embrapa_mt_distances_twdtw.rds")
}

if (TRUE) distances_twdtw.tb <- readRDS(system.file("extdata/models/embrapa_mt_distances_twdtw.rds", package = "sits"))

# estimate an SVM model for this training data
model_svm1.ml <- sits_svm(distances_twdtw.tb, kernel = "radial", cost = 10)

# classify the test data
class.tb <- sits_classify(series.tb, patterns_gam.tb, model_svm1.ml, dist_method = sits_TWDTW_distances())

#classify the time series matches using yearly intervals

# plot the classification of the time series by yearly intervals
sits_plot_classification(class.tb, patterns_gam.tb, band = "ndvi")

# (b) CLASSIFICATION USING THE DISTANCES FROM DATA
#create patterns
patterns_data.tb <- sits_patterns_from_data(embrapa.tb)

# estimate distances
distances_data.tb <- sits_distances_from_data(embrapa.tb, patterns_data.tb)

# estimate an SVM model for this training data
model_svm2.ml <- sits_svm(distances_data.tb, kernel = "radial", cost = 10)

# classify the test data
class.tb <- sits_classify(series.tb, patterns_data.tb, model_svm2.ml, dist_method = sits_distances_from_data())

# plot the classification of the time series by yearly intervals
sits_plot_classification(class.tb, patterns_data.tb, band = "ndvi")
