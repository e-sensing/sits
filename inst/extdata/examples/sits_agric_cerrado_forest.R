# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

coverage <- "mod13q1_512"
bands <-  c("ndvi", "evi", "blue", "red", "nir", "mir")

# get samples from agriculture (Ieda and Damien)
agric.tb <- sits_getdata(file = system.file("extdata/samples/samples_Damien_Ieda_12classes_6bands_Water.json", package="sits") )

# get samples from cerrado and pasture (Rodrigo)
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado_6bands.json", package="sits"))

cerrado6.tb <- sits_getdata(table = cerrado.tb, URL = URL, coverage = coverage, bands = bands)
# get samples from forest
forest.tb <- sits_getdata(file = system.file("extdata/samples/forest_6bands.json", package="sits"))

matogrosso.tb <- dplyr::bind_rows(agric.tb, cerrado6.tb)
matogrosso.tb <- dplyr::bind_rows(matogrosso.tb, forest.tb)

sits_save(matogrosso.tb, "./inst/extdata/samples/matogrosso.json")

results.tb <-
