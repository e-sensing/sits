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
coverage.tb <- sits_coverage(service = "WTSS", product = "MOD13Q1", coverage = "mod13q1_512")
coverage.tb

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# a point in the transition forest pasture in Northern MT
long <- -55.57320
lat <- -11.50566
# obtain a time series from the WTSS server for this point
series.tb <- 
    sits_getdata(longitude = long, latitude = lat, service = "WTSS", product = "MOD13Q1",
                 coverage = "mod13q1_512", bands = c("ndvi", "evi"),
                 start_date = "2001-01-01", end_date = "2016-12-31")
# plot the series
sits_plot(series.tb)

## ------------------------------------------------------------------------
prodes_226_064[1:3,]

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply Savitzky–Golay filter
point_sg.tb <- sits_sgolay(point.tb)
# plot the series
sits_plot(sits_merge(point_sg.tb, point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply Whitakker filter
point_whit.tb <- sits_whittaker(point.tb)
# plot the series
sits_plot(sits_merge(point_whit.tb, point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply envelope filter
point_env.tb <- sits_envelope(point.tb)
# plot the series
sits_plot(sits_merge(point_env.tb, point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply ARIMA filter
point_cf.tb <- sits_cloud_filter(point.tb)
# plot the series
sits_plot(sits_merge(point_cf.tb, point.tb))

## ----dendrogram, cache=TRUE, fig.align="center", fig.height=4.5, fig.width=5.5----
# create a dendrogram object with default clustering parameters
dendro <- sits_dendrogram(samples2.tb)
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
# Retrieve the set of samples for the Mato Grosso region 
# (provided by EMBRAPA) (samples_MT_ndvi) and 
# get a point to be classified (point_ndvi)
class.tb <- sits_classify(point_ndvi,
                          samples_MT_ndvi,
                          ml_method = sits_svm(kernel = "radial", 
                                               cost = 10))
sits_plot(class.tb)

## ------------------------------------------------------------------------
# Retrieve the set of samples for the Mato Grosso region 
# (provided by EMBRAPA) (samples_MT_ndvi) and 
# get a point to be classified (point_ndvi)
class.tb <- sits_classify(point_ndvi,
                          samples_MT_ndvi,
                          ml_method = sits_rfor())
sits_plot(class.tb)

