# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)


#load patterns from examples file
cerrado.tb <- sits_getdata(file = "./inst/extdata/Samples/cerrado.json")

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Rodrigo_7classes_6bands.json")

# classify samples using TWDTW
bands <- c("ndvi", "evi")
results.tb  <- sits_TWDTW (cerrado.tb[1:30,], patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

sits_plot(results.tb, type = "classification")



