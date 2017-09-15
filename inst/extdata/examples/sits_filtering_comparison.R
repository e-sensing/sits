# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

#select a coverage
coverage <- "mixl8mod"

# get information about a specific coverage
sits_coverageWTSS(URL,coverage)
#select the bands used for classification
bands <- c("ndvi", "evi")

#get the prodes samples
prodes.tb <- sits_getdata(system.file("extdata/prodes/prodes_new_226_64.csv", package = "sits"),
                                  coverage = coverage, bands = bands)
sits_plot (prodes.tb, type = "together")

#generate patterns with raw data and plot them
prodes_patterns.tb <- sits_patterns(prodes.tb)
sits_plot(prodes_patterns.tb, type = "patterns")

#cross_validate raw series
conf.tb <- sits_kfold_fast_validate (prodes.tb, folds = 2)

#evaluate the accuracy of the classification
sits_accuracy(conf.tb)

# relabel and see new assessment
prodes_relabel.lst <-  tibble::lst("Forest" = "Forest",
                                   "ClearCut"  = "NonForest",
                                   "ClearCut" = "NonForest",
                                   "Pasture"  = "NonForest")

sits_accuracy(conf.tb, prodes_relabel.lst)

# test savitsky golay filter
prodes_sg.tb <- sits_sgolay(prodes.tb, order = 2, scale = 1)

# =========== SAVITKSY - GOLAY FILTER =============

#plot the result of SG filter
sits_plot (prodes_sg.tb, type = "together")

# compare the raw data with the Savistky Golay filter
sg1 <- sits_sgolay(prodes.tb[1,], order = 2, scale = 1)

sg1 %>%
     sits_merge (samples_prodes.tb[1,]) %>%
     sits_select (bands = c("ndvi", "ndvi.sg")) %>%
     sits_plot()

conf_sg.tb <- sits_kfold_fast_validate (prodes_sg.tb, folds = 2)

#evaluate the accuracy of the classification
sits_accuracy(conf_sg.tb)

sits_accuracy(conf_sg.tb, prodes_relabel.lst)

# =========== WHITAKKER SMOOTHER =============

# test whitakker filter
prodes_whit.tb <- sits_whittaker(prodes.tb, lambda = 2.0)

# compare the raw data with the Savistky Golay filter
w1 <- sits_whittaker(prodes.tb[1,], lambda = 2.0)

w1 %>%
    sits_merge (prodes.tb[1,]) %>%
    sits_select (bands = c("ndvi", "ndvi.with")) %>%
    sits_plot()

conf_whit.tb <- sits_kfold_fast_validate (prodes_whit.tb, folds = 2)

#evaluate the accuracy of the classification
sits_accuracy(conf_whit.tb)

# relabel and see assessment
sits_accuracy(conf_whit.tb, prodes_relabel.lst)
