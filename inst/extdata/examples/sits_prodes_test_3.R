# satellite image time series package (SITS)
# example of the classification of PRODES data
library(sits)

# recover all bands

prodes.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.json.gz")

sits_plot(prodes.tb, type = "together")

prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "ClearCut",
                                   "clear_cut2016"  = "ClearCut",
                                   "pasture"        = "Pasture")

prodes2.tb <- sits_relabel(prodes.tb, prodes_relabel.lst)

conf1.tb <- sits_kfold_fast_validate(prodes2.tb, folds = 2, multicores = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(multicores = 2),
                                     tr_method   = sits_svm (cost = 10, kernel = "radial",
                                                             tolerance = 0.001, epsilon = 0.1))


