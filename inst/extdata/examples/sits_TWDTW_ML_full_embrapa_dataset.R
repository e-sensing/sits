# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

WD <- getwd()

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

# get information about a specific coverage
sits_coverageWTSS(URL,"mod13q1_512")

# choose a coverage
coverage <- "mod13q1_512"
# recover all bands
bands <- c("ndvi", "evi", "nir", "mir")

# a point in the transition forest pasture in Northern MT
long <- -55.57320
lat <- -11.50566

# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = long, latitude = lat, URL = URL, coverage = "mod13q1_512", bands = bands)

# plot the series
sits_plot (series.tb)

# retrieve a set of samples from a JSON file
embrapa_mt.tb <- readRDS(system.file ("extdata/time_series/embrapa_mt.rds", package = "sits"))

sits_bands(embrapa_mt.tb)

embrapa_mt2.tb <- sits_select(embrapa_mt.tb, bands = c("ndvi", "evi", "nir", "mir") )

# obtain a set of patterns for these samples
patterns.tb <- sits_patterns(embrapa_mt2.tb)

sits_plot (patterns.tb, type = "patterns")

# find the matches between the patterns and the time series using the TWDTW algorithm
# (uses the dtwSat R package)
distances_train.tb <- sits_distances (embrapa_mt2.tb, patterns.tb)

# save the distances for reusing the distance file later
saveRDS (distances_train.tb, file = paste0(WD,"/embrapa_mt_distances_train.rds"))

# find a model on the training data set
model.ml <- sits_svm (distances_train.tb, cost = 10, kernel = "radial",tolerance = 0.001, epsilon = 0.1)

# classify the test data
class.tb <- sits_classify(series.tb, patterns.tb, model.ml, start_date = "2000-09-14", end_date = "2016-08-29")

# plot the classification of the time series by yearly intervals
sits_plot(class.tb, patterns.tb, band = "ndvi")
