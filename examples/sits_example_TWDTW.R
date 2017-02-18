# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")
sits_infoWTSS(URL = "http://www.dpi.inpe.br/tws/wtss")

# then, configure the WTSS service
sits_configWTSS (URL = "http://www.dpi.inpe.br/tws/wtss",
                 coverage = "mod13q1_512",
                 bands = c("ndvi", "evi", "nir"))

# pick one point as an example
point.tb <- sits_fromWTSS(longitude = -55.23354, latitude = -11.51652)

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

# read a pattern table from a JSON file
patterns.tb <- sits_fromJSON ("./data/patterns/patterns_Rodrigo_8classes_6bands.json")

# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_classify(point.tb, patterns.tb, bands)

# # plot the classification
plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
plot(x = matches, type = "alignments")

#load patterns from examples file
examples.tb <- sits_fromCSV("./data/Samples/MatoGrosso-examples.csv")

examples.tb %>%
     sits_select(c("evi")) %>%
     sits_plot()

# read a pattern table from a JSON file
patterns.tb <- sits_fromJSON ("./data/patterns/patterns_Rodrigo_8classes_6bands.json")

# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_classify(examples.tb[5,], patterns.tb, bands)

# # plot the classification
plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
plot(x = matches, type = "alignments")


# load patterns from savanna and pasture (expert = Rodrigo)
cerrado.tb <- sits_fromJSON("./data/Samples/cerrado.json")

#plot the first 50 points, same lat/long points are plotted in the same graph
cerrado.tb[1:50,] %>%
     sits_plot()

# plot all time series, divided into classes
sits_plot (cerrado.tb, type = "together")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_classify(point.tb, patterns.tb, bands)

# # plot the classification
plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
plot(x = matches, type = "alignments")

# select samples for pasture and savanna
cerrado.tb <- sits_fromJSON ("./data/Samples/cerrado.json")

#select only savanna samples
savanna.tb <- filter (cerrado.tb, label == "Savanna")

sits_plot(savanna.tb, type = "together")

# cluster the savanna samples
sits_dendogram(savanna.tb, n_clusters = 4)

damien4.tb <- sits_fromCSV("./data/samples/damien.csv", n_max = 20)

damien4.tb %>%
     rowwise() %>%
     print(.$start_date)

# classify point using TWDTW

# read a pattern table from a JSON file
patterns.tb <- sits_fromJSON ("./data/patterns/patterns_8classes_6bands.json")
# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_classify(point.tb, patterns.tb, bands)

# # plot the classification
plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
plot(x = matches, type = "alignments")

# read sample information from a CSV archive,
# recover the time series from the WTSS service
# put data and metadata on a SITS table
embrapa.tb <- sits_fromCSV ("./data/Samples/damien.csv", n_max = 5)


