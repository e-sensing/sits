# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

#select a coverage
coverage <- "mod13q1_512"

# get information about a specific coverage
sits_coverageWTSS(URL,coverage)
#select the bands used for classification
bands <- c("ndvi", "evi", "nir")

#get the prodes samples
prodes_mod13.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.csv",
                                coverage = coverage, bands = bands)

#relabel the series
# relabel and see assessment
prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "ClearCut",
                                   "clear_cut2016"  = "ClearCut",
                                   "pasture"        = "Pasture")

prodes_mod13.tb <- sits_relabel(prodes_mod13.tb, prodes_relabel.lst)

sits_plot(prodes_mod13.tb, type = "together")

#plot the first element of the series
#sits_plot (series_prodes.tb[606,])

#generate patterns with raw data and plot them
prodes_mod13_patterns.tb <- sits_patterns(prodes_mod13.tb, method = "gam", bands = bands)
sits_plot(prodes_mod13_patterns.tb, type = "patterns")

#cross_validate raw series
cv_mod13 <- sits_cross_validate (prodes_mod13.tb, method = "gam", bands = bands,
     times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_mod13.json")

# test envelope filter
prodes_mod13_env.tb <- prodes_mod13.tb %>%
     sits_envelope(window_size = 3) %>%
     sits_envelope(window_size = 3) %>%
     sits_whittaker(lambda = 1.0 ) %>%
     sits_select (bands = c("ndvi.upper.lower.whit", "ndvi.lower.upper.whit"))

cv_mod13_env <- sits_cross_validate (prodes_mod13_env.tb, method = "gam", bands = c("ndvi.upper.lower.whit", "ndvi.lower.upper.whit"),
                                 times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_mod13_env.json")

# test whitakker filter
prodes_mod13_whit.tb <- prodes_mod13.tb %>%
     sits_whittaker(lambda = 2.0 ) %>%

cv_mod13_whit <- sits_cross_validate (prodes_mod13_whit.tb, method = "gam", bands = c("ndvi.whit", "evi.whit", "nir.whit"),
                                     times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_mod13_whit.json")



