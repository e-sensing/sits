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

# shapefile
shp_file <- system.file("extdata/shapefiles/anhanguera/anhanguera.shp", package = "sits")

munic.tb <- sits_fromSHP(shp_file, URL, coverage)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

# select the bands for classification
embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi", "nir", "mir"))

# train a model
distances.tb <- sits_distances(embrapa.tb)

model_svm.ml <- sits_svm (distances.tb)

# classify the data
class.tb <- sits_classify(munic.tb, embrapa.tb, model_svm.ml, multicores = 2)

