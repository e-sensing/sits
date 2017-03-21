# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")
sits_infoWTSS(URL = "http://www.dpi.inpe.br/tws/wtss")

# then, configure the WTSS service
inpe <- sits_configWTSS (URL = "http://www.dpi.inpe.br/tws/wtss",
                 coverage = "chronos:modis:mod13q1_512",
                 bands = c("ndvi", "evi", "nir"))

# select samples for pasture and savanna
cerrado.tb <- sits_getdata (system.file("extdata/samples/cerrado.json", package="sits"))

patterns.tb <- sits_patterns(cerrado.tb)

sits_plot (patterns.tb, type = "patterns")

