#load the sits library
library (sits)

# load the pre-stored data samples provided by EMBRAPA
embrapa.tb <- readRDS (system.file ("extdata/time_series/embrapa_mt.rds", package = "sits"))

# print the bands available in the EMBRAPA data set
sits_bands(embrapa.tb)

# print the summary of the data set
sits_summary(embrapa.tb)

# select the bands for classification
embrapa.tb <- sits_select (embrapa.tb, bands = c("ndvi", "evi", "nir", "mir"))

# calculate the patterns used for distance matching (this is just to align the input data)
patterns.tb <- sits_patterns_from_data(embrapa.tb)

# calculate the distances using the original data set
distances.tb <-  sits_spread_time_series(embrapa.tb, patterns.tb)

model.ml <- sits_svm(distances.tb)

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

point.tb <- sits_getdata (longitude = long, latitude = lat, bands = bands, coverage = coverage, URL = URL)

class.tb <- sits_classify (point.tb, patterns.tb, model.ml, dist_method = sits_spread_time_series(),
                           start_date = "2000-09-13", end_date = "2016-08-31")
<<<<<<< HEAD


=======
>>>>>>> a2becc73b4a48c7238c9b3e527414ac0230031db
