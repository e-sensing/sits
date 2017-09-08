# satellite image time series package (SITS)
# example of the classification of PRODES data
library(sits)
library (dtwclust)

# recover all bands

URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

#select a coverage
coverage <- "mixl8mod"

# get information about a specific coverage
sits_coverageWTSS(URL,coverage)

bands <- c("nir", "ndvi", "evi")

prodes.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_new_226_64.csv", URL = URL, bands = bands, coverage = coverage)

sits_plot(prodes.tb)

prodes_cf.tb <- sits_cloud_filter(prodes.tb)

prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "ClearCut",
                                   "clear_cut2016"  = "ClearCut",
                                   "pasture"        = "Pasture")

prodes_cf.tb <- sits_relabel(prodes_cf.tb, prodes_relabel.lst)

dend.obj <- sits_dendrogram(prodes_cf.tb)

sits_plot_dendrogram(prodes_cf.tb, dend.obj)

clusters.tb <- sits_cluster (prodes_cf.tb, dend.obj, k = 12)

sits_cluster_frequency(clusters.tb)





