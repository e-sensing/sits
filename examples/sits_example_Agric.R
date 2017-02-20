# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

# then, configure the WTSS service
sits_configWTSS (URL,
                 coverage = "mod13q1_512",
                 bands = c("ndvi", "evi", "nir"))

# read a pattern table from a JSON file
patterns.tb <- sits_getdata("./data/patterns/patterns_Rodrigo_8classes_6bands.json")

# plot patterns
sits_plot (patterns.tb, type = "patterns")

#load patterns from examples file
examples.tb <- sits_getdata("./data/Samples/MatoGrosso-examples.csv")

examples.tb %>%
     sits_select(c("evi")) %>%
     sits_plot()

cerrado.tb <- sits_getdata("./data/Samples/cerrado.json")

long <- -55.51810
lat <-  -11.63884
