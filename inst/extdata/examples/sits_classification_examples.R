# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# In this example, we are going to train a ML model and then will classify a point retrieved
# from the WTSS server and then a set of samples retrieved from the server
# we will show how to set the classification info

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

# print the bands available
sits_bands (embrapa.tb)

# select the bands "ndvi", "evi", "nir", and "mir"
embrapa.tb <- sits_select (embrapa.tb, bands = c("ndvi","evi","nir","mir"))

# Show the patterns for the MatoGrosso data using the GAM model
patterns.tb <- sits_gam (embrapa.tb)

# Plot the patterns
sits_plot(patterns.tb)

# estimate distances from the data
distances.tb <- sits_distances_from_data(embrapa.tb)

# estimate an SVM model for this training data
model_svm.ml <- sits_svm(distances.tb, kernel = "radial", cost = 10)

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

# select a point in the transition from forest to pasture in Northern MT
longitude <- -55.31657
latitude  <- -11.66789

# retrieve the time series associated with the point from the WTSS server
point.tb <- sits_getdata(latitude = latitude, longitude = longitude,
                          URL = URL, coverage = "mod13q1_512", bands = bands,
                         start_date = "2000-02-18", end_date = "2016-12-18")

# plot the series (only the ndvi and evi bands)
sits_plot (sits_select (point.tb, bands = c("ndvi", "evi")))

# classify the test data
class.tb <- sits_classify(point.tb, embrapa.tb, model_svm.ml)

# plot the classification of the time series by yearly intervals
sits_plot_classification(class.tb, band = "ndvi")

# retrieve a series of samples defined by a CSV file
# obtain a time series from the WTSS server for these samples
data.tb <- sits_getdata (file = system.file ("extdata/samples/samples_matogrosso.csv", package = "sits"),
                         URL = URL, bands = bands, coverage = coverage)

# plot the data
sits_plot(data.tb[1,])

# classify the test data
class2.tb <- sits_classify(data.tb, embrapa.tb, model_svm.ml)

# plot the classification of the time series by yearly intervals
sits_plot_classification(class2.tb, band = "ndvi")
