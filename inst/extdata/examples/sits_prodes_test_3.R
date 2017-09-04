# satellite image time series package (SITS)
# example of the classification of PRODES data
library(sits)

# recover all bands

prodes.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.json.gz")

patterns_raw.tb <- sits_patterns (samples_all.tb)
sits_plot(patterns_raw.tb, type = "patterns")

conf1.tb <- sits_kfold_fast_validate(prodes.tb, folds = 2, multicores = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(multicores = 2),
                                     tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                             tolerance = 0.001, epsilon = 0.1))

conf1_svm.mx <- sits_accuracy(conf1.mx)

prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                     "clear_cut2015"  = "ClearCut",
                                     "clear_cut2016"  = "ClearCut",
                                     "pasture"        = "Pasture")

prodes2.tb <- sits_relabel(prodes.tb, prodes_relabel.lst)

conf2.tb <- sits_kfold_fast_validate(prodes2.tb, folds = 2, multicores = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(multicores = 2),
                                    tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                            tolerance = 0.001, epsilon = 0.1))

conf2_svm.mx <- sits_accuracy(conf2.tb)


prodesf.tb <- prodes2.tb %>%
     sits_envelope (window_size = 3) %>%
     sits_envelope (window_size = 3) %>%
     sits_select (bands = c("ndvi.lower.upper", "evi.lower.upper")) %>%
     sits_whittaker(lambda = 2.0)

conf3.tb <- sits_kfold_fast_validate(prodesf.tb, folds = 2, multicores = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(multicores = 2),
                                     tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                             tolerance = 0.001, epsilon = 0.1))

conf3_svm.mx <- sits_accuracy(conf3.tb)

# retrieve a set of samples from a JSON file
series_all.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_series_226_64.json")
