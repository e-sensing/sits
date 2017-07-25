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
samples_prodes.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.json")
#get the full series for each point
series_prodes.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.json")

#relabel the series
# relabel and see assessment
prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "ClearCut2015",
                                   "clear_cut2016"  = "ClearCut2016",
                                   "pasture"        = "Pasture")


samples_prodes.tb <- sits_relabel(samples_prodes.tb, prodes_relabel.lst)
#plot the all samples for each class together
sits_plot (samples_prodes.tb, type = "together")

#generate patterns with raw data and plot them
prodes_patterns.tb <- sits_patterns(samples_prodes.tb, method = "gam", bands = bands)
sits_plot(prodes_patterns.tb, type = "patterns")

samples_prodes_2016.tb <- dplyr::filter (samples_prodes.tb, label != 'ClearCut2015')

#cross_validate raw series
cv_raw_2016 <- sits_cross_validate (samples_prodes_2016.tb, method = "gam", bands = bands,
     times = 30, perc = 0.5, file = "./inst/extdata/validation/cv_raw.json")

# relabel and see assessment
prodes_relabel2.lst <-  tibble::lst("Forest" = "Forest",
                                   "ClearCut2015"  = "NonForest",
                                   "ClearCut2016" = "NonForest",
                                   "Pasture"      = "NonForest")

cv_raw_2016_2 <- sits_reassess(file = "./inst/extdata/validation/cv_raw.json",
                               conv = prodes_relabel2.lst)
# test savitsky golay filter
samples_sg.tb <- sits_sgolay(samples_prodes.tb, order = 2, scale = 1)

#plot the result of SG filter
sits_plot (samples_sg.tb, type = "together")
bands_sg = c("ndvi.sg", "evi.sg")

sg1 <- sits_sgolay(samples_prodes.tb[1,], order = 2, scale = 1)

sg1 %>%
     sits_merge (samples_prodes.tb[1,]) %>%
     sits_select (bands = c("ndvi", "ndvi.sg")) %>%
     sits_plot()

sg_patterns <- sits_patterns(samples_sg.tb, method = "gam", bands = bands_sg)

cv_sg <- sits_cross_validate (samples_sg.tb, method = "gam", bands = bands_sg,
                               times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_sg.json")


# test whitakker filter
samples_whit.tb <- sits_whittaker(samples_prodes.tb, lambda = 2.0)
bands_whit = c("ndvi.whit", "evi.whit")
whit_patterns <- sits_patterns(samples_whit.tb, method = "gam", bands = bands_whit)
cv_whit <- sits_cross_validate (samples_whit.tb, method = "gam", bands = bands_whit,
                              times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_whit.json")


# relabel and see assessment
prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "NonForest",
                                   "clear_cut2016"  = "NonForest",
                                   "pasture"        = "NonForest")
cv_whit_2016 <- sits_reassess(file = "./inst/extdata/validation/cv_whit.json",
                               conv = prodes_relabel.lst)


#relabel the series
# relabel and see assessment
prodes_relabel_2.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "ClearCut2015",
                                   "clear_cut2016"  = "ClearCut2016",
                                   "pasture"        = "Pasture")

samples_prodes_2016.tb <- samples_prodes.tb %>%
     sits_relabel (conv = prodes_relabel_2.lst ) %>%
     dplyr::filter (label != "ClearCut2015")

# test envelope filter
bands_env <- c("ndvi.upper.lower")

samples_env.tb <- sits_envelope(samples_prodes_2016.tb, window_size = 2) %>%
     sits_envelope(window_size = 2)%>%
     sits_select(bands = bands_env)

samples_evi_whit.tb <- sits_whittaker(samples_prodes_2016.tb, lambda = 2.0) %>%
     sits_select (bands = c("evi.whit"))

samples_env_whit <- sits_merge (samples_env.tb, samples_evi_whit.tb)

bands_new = c("ndvi.upper.lower", "evi.whit")
env_patterns <- sits_patterns(samples_env_whit, method = "gam", bands = bands_new)
sits_plot(env_patterns, type = "patterns")

cv_env <- sits_cross_validate (samples_env.tb, method = "gam", bands = bands_env,
                                times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_env.json")

# relabel and see assessment
prodes_relabel.lst <-  tibble::lst("Forest" = "Forest",
                                   "ClearCut2015"  = "NonForest",
                                   "ClearCut2016"  = "NonForest",
                                   "Pasture"        = "NonForest")

cv_env4_r <- sits_reassess (file = "./inst/extdata/validation/cv_env4.json",
                           conv = prodes_relabel.lst )

# adjust patterns
#get MT patterns
sits_patterns_mt <- sits_getdata (file = "./inst/extdata/patterns/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")
sits_forest_pattern.tb <- dplyr::filter(sits_patterns_mt, label == "Forest") %>%
     sits_select(bands = c("ndvi", "evi"))

sits_nonforest.tb <- dplyr::filter(prodes_patterns.tb, label != "Forest")

patterns_mix.tb <- dplyr::bind_rows(sits_forest_pattern.tb, sits_nonforest.tb)

cv_mix <- sits_test_patterns (samples_prodes.tb, patterns_mix.tb, bands = c("ndvi", "evi"))
