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
shp_file <- system.file("extdata/shapefiles/sao_pedro_da_cipa/sao_pedro_da_cipa.shp", package = "sits")

sao_pedro.tb <- sits_fromSHP(shp_file, URL, coverage.tb, bands)

# select the ndvi and evi bands
sao_pedro.tb <- sits_select(sao_pedro.tb, bands = c("ndvi", "evi"))

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
embrapa.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))

# select the bands for classification
embrapa.tb <- sits_select(embrapa.tb, bands = c("ndvi", "evi"))

# train a model
distances.tb <- sits_distances(embrapa.tb)

model_svm.ml <- sits_svm (distances.tb)

# classify the data
class.tb <- sits_classify(sao_pedro.tb[1:10,], embrapa.tb, model_svm.ml, multicores = 2)

# obtain indepedent samples
data.tb <- readRDS("./inst/extdata/time_series/cerrado_agriculture.rds")

data.tb <- sits_select(data.tb, bands = c("ndvi", "evi"))

# classify the samples
class.tb <- sits_classify(data.tb, embrapa.tb, model_svm.ml)

# get confusion matrix
conf.mx <- sits_conf_matrix(class.tb)
