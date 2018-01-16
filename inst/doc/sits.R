## ---- include = FALSE----------------------------------------------------
library(sits)
library(tibble)
library(dtwclust)

## ------------------------------------------------------------------------
# data set of samples
# print the first three samples
samples_MT_9classes[1:3,]

## ------------------------------------------------------------------------
# print the first 10 time series records of the first sample
samples_MT_9classes$time_series[[1]][1:3,]

## ------------------------------------------------------------------------
sits_labels(samples_MT_9classes)

## ----cerrado-15, fig.align="center", fig.height=3.1, fig.width=5, fig.cap="Plot of the first 15 'Cerrado' samples from data set \\texttt{samples_MT_9classes} (different dates for the same point location)."----
# select the "ndvi" bands
samples_ndvi.tb <- 
    sits_select(samples_MT_9classes, 
                bands = c("ndvi"))
# select only the samples with the cerrado label
samples_cerrado.tb <- 
    dplyr::filter(samples_ndvi.tb, 
                  label == "Cerrado")
# plot the first 15 samples (different dates for the same points)
sits_plot(samples_cerrado.tb[1:15,])

## ----cerrado-all, fig.align="center", fig.height=3.1, fig.width=5, fig.cap="Plot of all 'Cerrado' samples from data set \\texttt{samples_MT_9classes}."----
# plot all cerrado samples together (shows the distribution)
sits_plot(samples_cerrado.tb)

## ------------------------------------------------------------------------
# get information about a specific coverage from WTSS
coverage.tb <- 
    sits_coverage(service  = "WTSS", 
                  product  = "MOD13Q1", 
                  coverage = "mod13q1_512")
coverage.tb[, c("xmin","xmax","ymin","ymax",
                "start_date", "end_date")]

## ---- fig.align="center", fig.height=3.1, fig.width=5, fig.cap="NDVI and EVI time series fetched from WTSS service."----
# a point in the transition forest pasture in Northern MT
# obtain a time series from the WTSS server for this point
series.tb <- 
    sits_getdata(longitude  = -55.57320, 
                 latitude   = -11.50566, 
                 service    = "WTSS", 
                 product    = "MOD13Q1",
                 coverage   = "mod13q1_512", 
                 bands      = c("ndvi", "evi"),
                 start_date = "2001-01-01", 
                 end_date   = "2016-12-31")
# plot the series
sits_plot(series.tb)

## ----dendrogram, cache=TRUE, fig.align="center", fig.height=4.1, fig.width=5----
# take a set of patterns for 2 classes
# create a dendrogram object with default clustering parameters
dendro <- sits_dendrogram(cerrado_2classes)
# plot the resulting dendrogram
sits_plot_dendrogram(cerrado_2classes, 
                     dendro)

## ------------------------------------------------------------------------
# search for the best height to cut the dendrogram
sits_dendro_bestcut(cerrado_2classes, 
                    dendro)

## ------------------------------------------------------------------------
# create clusters by cutting the dendrogram at the linkage distance 300
clusters.tb <- sits_cluster(cerrado_2classes, 
                            dendro, 
                            height = 22)
# show clusters samples frequency
sits_cluster_frequency(clusters.tb)

## ------------------------------------------------------------------------
# clear sample outliers relative to clusters (those with less than 5% in a cluster)
clusters2.tb <- sits_cluster_cleaner(clusters.tb)
# show clusters samples frequency
sits_cluster_frequency(clusters2.tb)

## ------------------------------------------------------------------------
prodes_226_064[1:3,]

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply Savitzky–Golay filter
point_sg.tb <- sits_sgolay(point.tb)
# plot the series
sits_plot(sits_merge(point_sg.tb, point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5, fig.cap="Whittaker filter applied on a one-year sample NDVI time series."----
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply Whitaker filter
point_whit.tb <- sits_whittaker(point.tb)
# plot the series
sits_plot(sits_merge(point_whit.tb, point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply envelope filter (remove short downward noises)
point_env.tb <- sits_envelope(point.tb, "UL")
# plot the series
sits_plot(sits_merge(point_env.tb, point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply ARIMA filter
point_cf.tb <- sits_cloud_filter(point.tb, apply_whit = FALSE)
# plot the series
sits_plot(sits_merge(point_cf.tb, point.tb))

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

