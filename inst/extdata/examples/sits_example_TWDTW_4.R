# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)


#load patterns from examples file
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits"))

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_Rodrigo_7classes_6bands.json", package="sits"))

# classify samples using TWDTW
bands <- c("ndvi", "evi")
results.tb  <- sits_TWDTW (cerrado.tb[1:30,], patterns.tb, bands, alpha= -0.1, beta = 100, theta = 0.5)

sits_plot(results.tb, type = "classification")



