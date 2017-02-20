library (sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")
sits_infoWTSS(URL = "http://www.dpi.inpe.br/tws/wtss")

# then, configure the WTSS service
sits_configWTSS (URL = "http://www.dpi.inpe.br/tws/wtss",
                 coverage = "mod13q1_512",
                 bands = c("ndvi", "evi", "nir", "mir", "red", "blue"))

# select samples for pasture and savanna
cerrado.tb <- sits_getdata ("./data/Samples/cerrado.json")

savanna.tb <- filter (cerrado.tb, label == "Savanna")

sits_plot (savanna.tb, type = "together")

clusters_sav.tb <- sits_cluster (savanna.tb, type = "dendogram", n_clusters = 4)
