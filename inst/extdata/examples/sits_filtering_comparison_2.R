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

samples_prodes.tb <-  sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.json")
series_prodes.tb  <-  sits_getdata(file = "./inst/extdata/samples/prodes_series_226_64.json")

series_forest <- dplyr::filter (series_prodes.tb, label == "primary_forest")

sits_plot (series_forest, type = "together")
#plot the first element of the series
#sits_plot (series_prodes.tb[606,])

series_forest %>%
     sits_envelope (window_size = 3) %>%
     sits_envelope (window_size = 3) %>%
     sits_whittaker(lambda = 1.0) %>%
     sits_select (bands = c("ndvi.upper.lower.whit")) %>%
     sits_plot (type = "together")

series_cut <- dplyr::filter (series_prodes.tb, label == "clear_cut2015")
series_cut %>%
     sits_select (bands = c("ndvi")) %>%
     sits_plot (type = "together")

series_cut %>%
     sits_envelope (window_size = 2) %>%
     sits_envelope (window_size = 2) %>%
     sits_whittaker(lambda = 1.0) %>%
     sits_select (bands = c("ndvi.upper.lower.whit")) %>%
     sits_plot (type = "together")

#generate patterns with raw data and plot them
prodes_patterns.tb <- sits_patterns(samples_prodes.tb, method = "gam", bands = bands)
sits_plot(prodes_patterns.tb, type = "patterns")

sample_cut <- dplyr::filter (samples_prodes.tb, label == "clear_cut2015")

sample_cut %>%
     sits_select (bands = c("ndvi")) %>%
     sits_plot (type = "together")
#cross_validate raw series
cv_raw <- sits_reassess (file = "./inst/extdata/validation/cv_raw.json")

# relabel and see assessment
prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "NonForest",
                                   "clear_cut2016"  = "NonForest",
                                   "pasture"        = "NonForest")
cv_raw2 <- sits_reassess (file = "./inst/extdata/validation/cv_raw.json", conv = prodes_relabel.lst )

# test savitsky golay filter
seriew_sg.tb <- sits_sgolay(series_prodes.tb)
bands_sg = c("ndvi.sg", "evi.sg")
sg_patterns_f <- sits_patterns(samples_sg_f.tb, method = "gam", bands = bands_sg)
cv_sg <- sits_cross_validate (samples_sg_f.tb, method = "gam", bands = bands_sg,
                               times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_sg_f.json")


# test whitakker filter
samples_whit_f.tb <- sits_sgolay(samples_prodes_f.tb, lambda = 2.0)
bands_whit = c("ndvi.whit", "evi.whit")
whit_patterns_f <- sits_patterns(samples_whit_f.tb, method = "gam", bands = bands_whit)
cv_whit_f <- sits_cross_validate (samples_whit_f.tb, method = "gam", bands = bands_whit,
                              times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_whit_f.json")



# test envelope filter
bands_env <- c("ndvi.upper.whit", "evi.lower.whit")
samples_env_f.tb <- sits_envelope(samples_prodes_f.tb) %>%
     sits_whittaker(lambda = 2.0) %>%
     sits_select(bands = bands_env)

env_patterns_f <- sits_patterns(samples_env_f.tb, method = "gam", bands = bands_env)
cv_env_f <- sits_cross_validate (samples_env_f.tb, method = "gam", bands = bands_env,
                                times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_env_f.json")

