# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

long <-  -55.23354
lat  <- -11.51652

sits_infoWTSS(URL)

# then, configure the WTSS service
inpe <- sits_configWTSS (URL,
                         coverage = "mod13q1_512",
                         bands = c("ndvi", "evi", "nir"))

# pick one point as an example
point.tb <- sits_getdata(longitude = long, latitude = lat, wtss = inpe)
# select the evi and plot it
point.tb %>%
     sits_select (c("evi")) %>%
     sits_plot()

#smooth the data and put into a new table
point2.tb <- sits_smooth (point.tb, lambda = 1.0)

# plot the smoothed time series
point2.tb %>%
     sits_select ("ndvi") %>%
     sits_plot()

# rename the smoothed time series
point3.tb <- sits_rename (point2.tb, c("ndvi_smooth", "evi_smooth", "nir_smooth"))

# merge the raw and smoothed time series and plot the ndvi
point3.tb %>%
     sits_merge (point.tb) %>%
     sits_select (c("ndvi", "ndvi_smooth")) %>%
     sits_plot()

samples1.tb <- sits_getdata(file = "./inst/extdata/samples/MatoGrosso-examples.csv", wtss = inpe )

samples1.tb %>%
     sits_plot(type = "one_by_one")

# read a pattern table from a JSON file
patterns.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_Rodrigo_8classes_6bands.json")

# plot patterns
sits_plot (patterns.tb, type = "patterns")

