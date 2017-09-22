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
sits_plot (series.tb[1,])

# retrieve a set of pre-defined patterns from an RDS file
patterns_matogrosso.tb <- readRDS(system.file("extdata/patterns/patterns_matogrosso_9classes.rds", package = "sits"))

# test the distances function

distances.tb <- sits_distances (series.tb, patterns_matogrosso.tb)

# retrieve the distances file derived from the Mato Grosso training data set
distances_twdtw.tb <- readRDS(system.file ("extdata/models/embrapa_mt_distances_twdtw.rds", package = "sits"))

# estimate an SVM model for this training data
model.ml <- sits_train(distances_twdtw.tb)

# classify the test data
class.tb <- sits_classify(series.tb, patterns_matogrosso.tb, model1.ml, start_date = "2008-09-14",
                          end_date = "2015-08-28")

#classify the time series matches using yearly intervals

# plot the classification of the time series by yearly intervals
sits_plot_classification(class.tb, patterns_matogrosso.tb, band = "ndvi")


