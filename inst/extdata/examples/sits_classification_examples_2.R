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
timeline <- sits_timeline(coverage.tb)

# choose a coverage
coverage <- "mod13q1_512"

# recover the NDVI, EVI, MIR and NIR bands
bands <- c("ndvi", "evi", "nir", "mir")

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi", "nir", "mir"))

# CLASSIFICATION USING THE DISTANCES FROM DATA
#create patterns
patterns_data.tb <- sits_patterns_from_data(embrapa.tb, timeline)

# estimate distances
distances_data.tb <- sits_distances_from_data(embrapa.tb, patterns_data.tb)

# estimate an SVM model for this training data
model_svm.ml <- sits_svm(distances_data.tb, kernel = "radial", cost = 10)


# a point in the Para sta
long <- -53.67105
lat <- -6.06105

point.tb <- sits_getdata(latitude = lat, longitude = long, URL = URL, coverage = coverage, bands = bands)

sits_plot (point.tb)

# classify the test data
class.tb <- sits_classify(point.tb, patterns_data.tb, model_svm.ml, dist_method = sits_distances_from_data())

# plot the classification of the time series by yearly intervals
sits_plot_classification(class.tb, patterns_data.tb, band = "ndvi")
