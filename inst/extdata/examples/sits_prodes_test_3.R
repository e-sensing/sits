# satellite image time series package (SITS)
# example of the classification of PRODES data
library(sits)

# recover all bands

prodes.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_samples_226_64.json.gz")

sits_plot(prodes.tb, type = "together")

prodes_cf.tb <- sits_cloud_filter(prodes.tb)

prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "ClearCut",
                                   "clear_cut2016"  = "ClearCut",
                                   "pasture"        = "Pasture")

prodes_cf.tb <- sits_relabel(prodes_cf.tb, prodes_relabel.lst)


prodes_clds.tb <- sits_cloud_filter (prodes2.tb[1:9,], cutoff = -0.25, order = 3)



