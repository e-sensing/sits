# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)
# get samples from agriculture (Ieda and Damien)
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits") )

patterns.tb <- sits_getdata(file = system.file("extdata/patterns/patterns_MatoGrosso.json", package="sits") )

sits_plot(patterns.tb, type = "patterns")
results.tb <- sits_TWDTW (matogrosso.tb, patterns.tb, bands)
