# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# recover all bands
bands <- c("ndvi", "evi")

# retrieve a set of samples from a JSON file
series_all.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_series_226_64.json")

samples_all.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.json")

patterns_raw.tb <- sits_patterns (samples_all.tb)
sits_plot(patterns_raw.tb, type = "patterns")

matches.tb <- sits_TWDTW_matches(series.tb[1,], patterns_raw.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, span = 0)
sits_plot (matches.tb, type = "alignments")

class.tb <- sits_TWDTW_classify (matches.tb, start_date = "2013-08-01", end_date = "2017-07-31", interval = "12 month")
sits_plot(matches.tb, type = "classification", start_date = "2013-08-01", end_date = "2017-07-31", interval = "12 month")

bands = c("ndvi.lower.upper.whit", "evi.lower.upper.whit")

seriesf_all.tb <- series_all.tb %>% 
     sits_envelope(window_size = 3) %>% 
     sits_envelope (window_size = 3) %>% 
     sits_select (bands = c("ndvi.lower.upper", "evi.lower.upper")) %>% 
     sits_whittaker(lambda = 2.0)

patterns_f.tb <- sits_patterns ()
#assessment <- sits_accuracy(results.tb)
matches1.tb <- sits_TWDTW_matches(seriesf.tb, patterns_raw.tb, bands, alpha= -0.1, beta = 100, theta = 0.5, span = 0)
sits_plot (matches.tb, type = "alignments")

class1.tb <- sits_TWDTW_classify (matches1.tb, start_date = "2013-08-01", end_date = "2017-07-31", interval = "12 month")
sits_plot(matches.tb, type = "classification", start_date = "2013-08-01", end_date = "2017-07-31", interval = "12 month")
>>>>>>> d864f58c1aba6d813a3413858509369a47000989
