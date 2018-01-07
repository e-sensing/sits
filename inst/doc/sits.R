## ----include = FALSE-----------------------------------------------------
library(sits)

## ------------------------------------------------------------------------
# retrieve a set of samples from an RDS file
samples.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds", package = "sits"))
samples.tb

## ------------------------------------------------------------------------
#print the first time series
samples.tb[1,]$time_series

## ------------------------------------------------------------------------
sits_bands (samples.tb)

## ------------------------------------------------------------------------
sits_labels (samples.tb)

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
samples2.tb <- sits_relabel(samples.tb, new_labels)
# view the result
sits_labels(samples2.tb)

## ------------------------------------------------------------------------
# select the "ndvi" bands
samples_ndvi.tb <- sits_select(samples.tb, bands = c("ndvi"))
# select only the samples with the cerrado label
samples_cerrado.tb <- dplyr::filter (samples_ndvi.tb, label == "Cerrado")

## ---- fig.height=3.1, fig.width=5----------------------------------------
# plot the first 15 samples (different dates for the same points)
sits_plot (samples_cerrado.tb[1:15,])

## ---- fig.align="center", fig.height=3.1, fig.width=5--------------------
# plot all cerrado samples together (shows the distribution)
sits_plot (samples_cerrado.tb)

## ---- eval = TRUE--------------------------------------------------------
URL <- "http://www.dpi.inpe.br/tws/wtss"
wtss_inpe <- sits_infoWTSS(URL)

## ---- eval = TRUE--------------------------------------------------------
# get information about a specific coverage
coverage.tb <- sits_coverageWTSS(URL,"mod13q1_512")

## ---- eval = TRUE, echo = TRUE-------------------------------------------
# a point in the transition forest pasture in Northern MT
# obtain a time series from the WTSS server for this point
series.tb <- sits_getdata(longitude = -55.57320, latitude = -11.50566, URL = URL,
coverage = "mod13q1_512", bands = c("ndvi", "evi"),
start_date = "2001-01-01", end_date = "2016-12-31")
# plot the series
sits_plot (series.tb)

