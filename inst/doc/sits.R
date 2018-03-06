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
coverage_wtss <- 
    sits_coverage(service  = "WTSS-INPE-1",
                  name     = "mod13q1_512")
coverage_wtss[, c("xmin","xmax","ymin","ymax",
                "start_date", "end_date")]

## ---- fig.align="center", fig.height=3.1, fig.width=5, fig.cap="NDVI and EVI time series fetched from WTSS service."----
# a point in the transition forest pasture in Northern MT
# obtain a time series from the WTSS server for this point
series.tb <- 
    sits_getdata(longitude  = -55.57320, 
                 latitude   = -11.50566, 
                 coverage   = coverage_wtss, 
                 bands      = c("ndvi", "evi"))
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
# create 6 clusters by cutting the dendrogram at 
# the linkage distance 20.39655
clusters.tb <- 
    sits_cluster(cerrado_2classes, 
                 dendro, 
                 k = 6)
# show clusters samples frequency
sits_cluster_frequency(clusters.tb)

## ------------------------------------------------------------------------
# clear those samples with a high confusion rate in a cluster 
# (those clusters which majority class does not reach 90% of 
#  samples in that cluster)
cleaned.tb <- 
    sits_cluster_remove(clusters.tb, 
                        min_perc = 0.9)
# show clusters samples frequency
sits_cluster_frequency(cleaned.tb)

## ---- fig.align="center", fig.height=3.1, fig.width=5, fig.cap="Savitzky-Golay filter applied on a one-year NDVI time series."----
# Take the NDVI band of the first sample data set
point.tb <- 
    sits_select(prodes_226_064[1,], 
                bands = c("ndvi"))
# apply Savitzky–Golay filter
point_sg.tb <- sits_sgolay(point.tb)
# plot the series
sits_plot(sits_merge(point_sg.tb, point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5, fig.cap="Whittaker smoother filter applied on one-year NDVI time series. The example uses default $\\lambda=1$ parameter."----
# Take the NDVI band of the first sample data set
point.tb <- 
    sits_select(prodes_226_064[1,], 
                bands = c("evi"))
# apply Whitaker filter
point_whit.tb <- sits_whittaker(point.tb)
# plot the series
sits_plot(sits_merge(point_whit.tb, 
                     point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5, fig.cap="Envelope filter applied on one-year NDVI time series. The examples uses two morfological filters: opening filtration (~.OF) and closing filtration (~.CF)."----
# Take the NDVI band of the first sample data set
point.tb <- 
    sits_select(prodes_226_064[1,], 
                bands = c("ndvi"))
# apply envelope filter (remove downward and upward noises)
point_env1.tb <- 
    sits_envelope(point.tb,
                  "ULLULUUL",
                  bands_suffix = "OF")
point_env2.tb <- 
    sits_envelope(point.tb,
                  "LUULULLU",
                  bands_suffix = "CF")
# plot the series
sits_plot(
    sits_merge(
        sits_merge(point_env1.tb, 
                   point_env2.tb),
        point.tb))

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# Take the NDVI band of the first sample data set
point.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
# apply ARIMA filter
point_cf.tb <- sits_cloud_filter(point.tb, apply_whit = FALSE)
# plot the series
sits_plot(sits_merge(point_cf.tb, point.tb))

## ---- fig.align="center", fig.height=3.4, fig.width=5.5, fig.cap="SVM classification of a $16$ years time series. The location (latitude, longitude) shown at the top of the graph is in geographic coordinate system (WGS84 {\\it datum})."----
# Retrieve the set of samples (provided by EMBRAPA) from the 
# Mato Grosso region for train the SVM model
data(samples_MT_ndvi)
# get a point to be classified
data(point_ndvi)
# Classify using SVM model
class.tb <- 
    sits_classify(point_ndvi,
                  samples_MT_ndvi,
                  ml_method = sits_svm(kernel = "radial",
                                       cost = 10))
sits_plot(class.tb)

## ---- fig.align="center", fig.height=3.4, fig.width=5.5, fig.cap="Random forest classification of a $16$ years time series. The location (latitude, longitude) shown at the top of the graph are in geographic coordinate system  (WGS84 {\\it datum})."----
# Retrieve the set of samples (provided by EMBRAPA) from the 
# Mato Grosso region for train the Random Forest model.
data(samples_MT_ndvi)
# get a point to be classified
data(point_ndvi)
# Classify using Random Forest model
class.tb <- 
    sits_classify(point_ndvi,
                  samples_MT_ndvi,
                  ml_method = sits_rfor())
sits_plot(class.tb)

## ---- fig.align="center", fig.height=3.4, fig.width=5.5, fig.cap="Deep learning classification of a $16$ year time series. The location (latitude, longitude) shown at the top of the graph are in geographic coordinate system  (WGS84 {\\it datum})."----
# Retrieve the set of samples (provided by EMBRAPA) from the 
# Mato Grosso region for train the deep learning model.
data(samples_MT_ndvi)
# get a point to be classified
data(point_ndvi)
# Classify using Random Forest model
class.tb <- 
    sits_classify(point_ndvi,
                  samples_MT_ndvi,
                  ml_method = sits_deeplearning(),
                  adj_fun = function (x) {identity(x)})
sits_plot(class.tb)

## ------------------------------------------------------------------------
# read a set of samples
data(cerrado_2classes)

# perform a five fold validation with the 
# SVM machine learning method using default parameters
prediction.mx <- 
    sits_kfold_validate(cerrado_2classes, 
                        folds = 5)
# prints the output confusion matrix and statistics 
sits_conf_matrix(prediction.mx)

## ---- fig.align="center", fig.height=3.4, fig.width=4.1, fig.cap="Image (${11\\times14}$ pixels) classified using SVM. The image coordinates ({\\it meters}) shown at vertical and horizontal axis are in MODIS sinusoidal projection."----
# Retrieve the set of samples for the Mato Grosso region 
data(samples_MT_ndvi)

# read a raster file and put it into a vector
files  <- 
    system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", 
                package = "sits")

# define the timeline
data("timeline_modis_392")

# create a raster metadata file based on the 
# information about the files
raster.tb <- 
    sits_coverage(service  = "RASTER",
                  name     = "Sinop-crop",
                  timeline = timeline_modis_392,
                  bands    = c("ndvi"),
                  files    = files)

# classify the raster file
raster_class.tb <- 
    sits_classify_raster(file = "./raster-class", 
                         raster.tb, 
                         samples_MT_ndvi,
                         ml_method  = sits_svm(), 
                         blocksize = 250, 
                         multicores = 1)
# plot classified image
plot(sits_get_raster(raster_class.tb, 1))

## ---- include=FALSE------------------------------------------------------
# remove all files
file.remove(unlist(raster_class.tb$files))

