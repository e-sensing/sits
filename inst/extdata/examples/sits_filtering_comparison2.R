samples_env4.tb <- sits_envelope(samples_prodes_2016.tb, window_size = 4) %>%
     sits_envelope(window_size = 4)%>%
     sits_select(bands = bands_env)

samples_evi_whit.tb <- sits_whittaker(samples_prodes_2016.tb, lambda = 2.0) %>%
     sits_select (bands = c("evi.whit"))

samples_env4_whit <- sits_merge (samples_env4.tb, samples_evi_whit.tb)

bands_new = c("ndvi.upper.lower", "evi.whit")
env4_patterns <- sits_patterns(samples_env4_whit, method = "gam", bands = bands_new)
sits_plot(env4_patterns, type = "patterns")

cv_env <- sits_cross_validate (samples_env.tb, method = "gam", bands = bands_env,
                               times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_env.json")
