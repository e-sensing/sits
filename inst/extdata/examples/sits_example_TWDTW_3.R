# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

coverage <- "mod13q1_512"
bands <-  c("ndvi", "evi", "nir")

sits_coverageWTSS(URL, coverage)


#load patterns from examples file
examples.tb <- sits_getdata(file = "./inst/extdata/samples/MatoGrosso-examples.csv", URL = URL, coverage = coverage, bands = bands)

examples.tb %>%
     sits_select(c("evi")) %>%
     sits_plot()

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_Damien_Ieda_Rodrigo_14classes_3bands_Original_Labels_Sep.json", package="sits"))

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
results.tb  <- sits_TWDTW (examples.tb[9:12,], patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

sits_plot(results.tb, type = "classification")



