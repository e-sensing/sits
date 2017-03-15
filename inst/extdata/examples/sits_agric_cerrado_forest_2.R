# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

coverage <- "mod13q1_512"
bands <-  c("ndvi", "evi", "nir")

# get samples from agriculture (Ieda and Damien)
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits") )

patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_MatoGrosso.json", package="sits") )

sits_plot(patterns.tb, type = "patterns")
results.tb <- sits_TWDTW (matogrosso.tb[1050:1075,], patterns.tb, bands)
