## ---- include = FALSE----------------------------------------------------
library(sits)
library(tibble)
library(dtwclust)

## ------------------------------------------------------------------------
# retrieve a set of samples from an RDS file
samples.tb <- samples_MT_9classes
samples.tb

## ------------------------------------------------------------------------
# print the first time series
samples.tb$time_series[[1]]

## ------------------------------------------------------------------------
sits_bands(samples.tb)

## ------------------------------------------------------------------------
sits_labels(samples.tb)

## ------------------------------------------------------------------------
# a list for relabelling the samples
new_labels <- list("Cerrado"       = "Savanna", 
                   "Pasture"       = "Grasslands", 
                   "Soy_Corn"      = "Double_Cropping",
                   "Soy_Cotton"    = "Double_Cropping",
                   "Soy_Sunflower" = "Double_Cropping",
                   "Soy_Fallow"    = "Single_Cropping",
                   "Soy_Millet"    = "Single_Cropping",
                   "Fallow_Cotton" = "Single_Cropping")
# apply the sits_relabel function
samples2.tb <- sits_relabel(samples_MT_9classes, new_labels)
# view the result
sits_labels(samples2.tb)

## ----fig.align="center", fig.height=3.1, fig.width=5---------------------
# select the "ndvi" bands
samples_ndvi.tb <- sits_select(samples_MT_9classes, bands = c("ndvi"))
# select only the samples with the cerrado label
samples_cerrado.tb <- dplyr::filter(samples_ndvi.tb, label == "Cerrado")
# plot the first 15 samples (different dates for the same points)
sits_plot(samples_cerrado.tb[1:15,])

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# plot all cerrado samples together (shows the distribution)
sits_plot(samples_cerrado.tb)

## ------------------------------------------------------------------------
sits_infoWTSS()

## ------------------------------------------------------------------------
# get information about a specific coverage
coverage.tb <- sits_coverageWTSS("mod13q1_512")
coverage.tb

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# a point in the transition forest pasture in Northern MT
long <- -55.57320
lat <- -11.50566
# obtain a time series from the WTSS server for this point
series.tb <- 
    sits_getdata(longitude = long, latitude = lat, service = "WTSS",
                 coverage = "mod13q1_512", bands = c("ndvi", "evi"),
                 start_date = "2001-01-01", end_date = "2016-12-31")
# plot the series
sits_plot(series.tb)

## ------------------------------------------------------------------------
# open CSV file with trusted samples
read.csv(system.file("extdata/samples/samples_import.csv",
                     package = "sits"))

## ------------------------------------------------------------------------
# obtain two years NDVI time series from the WTSS server for the last used coordinates
series.tb <- 
    sits_getdata(longitude = long, latitude = lat, service = "WTSS", 
                 coverage = "mod13q1_512", bands = c("ndvi"),
                 start_date = "2001-01-01", end_date = "2002-12-31")

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# apply Savitzky–Golay filter
series_sg.tb <- sits_sgolay(series.tb)
# plot the series
sits_plot(sits_merge(series_sg.tb, series.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# apply Whitakker filter
series_whit.tb <- sits_whittaker(series.tb)
# plot the series
sits_plot(sits_merge(series_whit.tb, series.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# apply envelope filter
series_env.tb <- sits_envelope(series.tb)
# plot the series
sits_plot(sits_merge(series_env.tb, series.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# apply ARIMA filter
series_cf.tb <- sits_cloud_filter(series.tb)
# plot the series
sits_plot(sits_merge(series_cf.tb, series.tb))

## ------------------------------------------------------------------------
# create a dendrogram object with default clustering parameters
dendro <- sits_dendrogram(samples2.tb)

## ---- fig.align="center", fig.height=4.5, fig.width=5.5------------------
# plot the resulting dendrogram
sits_plot_dendrogram(samples2.tb, dendro, cutree_height = 40)

## ------------------------------------------------------------------------
# create clusters by cutting the dendrogram at the linkage distance 300
clusters.tb <- sits_cluster(samples2.tb, dendro, height = 300)
# show clusters samples frequency
sits_cluster_frequency(clusters.tb)

## ------------------------------------------------------------------------
# clear sample outliers relative to clusters (those with less than 1% in a cluster)
clusters2.tb <- sits_cluster_cleaner(clusters.tb, min_clu_perc = 0.01)
# show clusters samples frequency
sits_cluster_frequency(clusters2.tb)

## ------------------------------------------------------------------------
# proceed relabeling procedure
clusters2.tb <- 
    sits_cluster_relabel(clusters2.tb,
                         c("Non-Cropping_Land", "Cropping_Land"))
# show clusters samples frequency
sits_cluster_frequency(clusters2.tb)

