

samples_env4.tb <- sits_envelope(samples_prodes_2016.tb, window_size = 3) %>%
     sits_envelope(window_size = 3)

bands_new <- c("ndvi.upper.lower", "evi.upper.lower")
env4_patterns <- sits_patterns(samples_env4.tb, method = "gam", bands = bands_new)
sits_plot(env4_patterns, type = "patterns")

cv_env4 <- sits_cross_validate (samples_env4.tb, method = "gam", bands = bands_new,
                               times = 50, perc = 0.8, file = "./inst/extdata/validation/cv_env4.json",
                               .multicores=10)

prodes_labels.lst = tibble::lst("ClearCut2015" = "NonForest",
                                   "ClearCut2016" = "NonForest",
                                   "Pasture" = "NonForest",
                                   "Forest"  = "Forest")
cv_env_r = sits_reassess(file = "./inst/extdata/validation/cv_env4.json", conv = prodes_labels.lst)

