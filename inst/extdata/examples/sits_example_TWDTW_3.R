# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"

sits_infoWTSS(URL)

# then, configure the WTSS service
inpe <- sits_configWTSS (URL,
                 coverage = "mod13q1_512",
                 bands = c("ndvi", "evi", "nir"))


#load patterns from examples file
examples.tb <- sits_getdata(file = "./inst/extdata/Samples/MatoGrosso-examples.csv", wtss = inpe)

examples.tb %>%
     sits_select(c("evi")) %>%
     sits_plot()

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")

examples.tb %>%
     by_row (function (r) {
          matches <- sits_classify(r, patterns.tb, bands)
          # # plot the classification
          plot(x = matches, type = "classification", overlap = 0.5)
          # # plot the alignments
          plot(x = matches, type = "alignments")
     })

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
patterns.tb <- sits_getdata ("./data/patterns/patterns_8classes_6bands.json")
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
embrapa.tb <- sits_getdata ("./data/Samples/damien.csv", n_max = 5)


