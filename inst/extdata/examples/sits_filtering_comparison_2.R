# satellite image time series package (SITS)
# example of the classification of a time series
#devtools::install_github("gilbertocamara/sits")
library(sits)

# Get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

#select a coverage
coverage <- "mixl8mod_f"

# get information about a specific coverage
sits_coverageWTSS(URL,coverage)
#select the bands used for classification
bands <- c("ndvi", "evi")

#get the prodes samples
samples_prodes_f.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.csv", URL = URL,
                          coverage = coverage, bands = bands)
#get the full series for each point
series_prodes_f.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.csv", URL = URL,
                                  coverage = coverage, bands = bands, ignore_dates = TRUE)

sits_save (samples_prodes_f.tb, json_file = "./inst/extdata/samples/prodes_samples_226_64_f.json")
sits_save (series_prodes_f.tb, json_file = "./inst/extdata/samples/prodes_series_226_64_f.json")

#plot the first element of the series
#sits_plot (series_prodes.tb[606,])

#generate patterns with raw data and plot them
prodes_patterns_f.tb <- sits_patterns(samples_prodes_f.tb, method = "gam", bands = bands)
sits_plot(prodes_patterns_f.tb, type = "patterns")

#cross_validate raw series
cv_raw_f <- sits_cross_validate (samples_prodes_f.tb, method = "gam", bands = bands,
     times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_raw_f.json")

# test savitsky golay filter
samples_sg_f.tb <- sits_sgolay(samples_prodes_f.tb)
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

