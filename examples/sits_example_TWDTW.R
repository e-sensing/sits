# this is an example of how to use SITS

# these are the sits dependencies
# --------------------------------
# Required libraries
# --------------------------------
# devtools
library (devtools)
# input/output - JSON
library(jsonlite)
# Documentation
library (roxygen2)
# raster data
library (raster)
# lubridate for date-time
library(lubridate)
# Zoo time series
library(zoo)
# Signal processing
library(signal)
# Parametric Time Warping
library(ptw)
# Time Series Clustering with DTW
library(dtwclust)
# Magrittr library for pipes
library(magrittr)
# Tidyverse
library (tidyverse)
# stringr
library (stringr)
# Web Series Time Service
library(wtss.R)
# Data mining with TWDTW
# load from github
# dtwSat package needs gfortran
# please install gfortran before installing dtwSat
# install_github("vwmaus/dtwSat")
library(dtwSat)
# reshape
library(reshape2)
# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")
sits_infoWTSS(URL = "http://www.dpi.inpe.br/tws/wtss")

# then, configure the WTSS service
sits_configWTSS (URL = "http://www.dpi.inpe.br/tws/wtss",
                 coverage = "mod13q1_512",
                 bands = c("ndvi", "evi", "nir"))

# pick one point as an example
point.tb <- sits_fromWTSS()

point.tb %>%
     sits_select ("ndvi") %>%
     sits_plot()

point2.tb <- sits_smooth (point.tb, lambda = 1.0)

point2.tb %>%
     sits_select ("ndvi") %>%
     sits_plot()

point3.tb <- sits_rename (point2.tb, c("ndvi_smooth", "evi_smooth", "nir_smooth"))

point3.tb %>%
     sits_merge (point.tb) %>%
     sits_select (c("ndvi", "ndvi_smooth")) %>%
     sits_plot()

# read a pattern table from a JSON file
patterns.tb <- sits_fromJSON ("./data/patterns/patterns_8classes_6bands.json")
# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_classTWDTW(point2.tb, patterns.tb, bands)

# # plot the classification
plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
plot(x = matches, type = "alignments")


damien4.tb <- sits_fromCSV("./data/samples/damien.csv", n_max = 4)

damien4.tb %>%
     rowwise() %>%
     print(.$start_date)

# classify point using TWDTW

# read a pattern table from a JSON file
patterns.tb <- sits_fromJSON ("./data/patterns/patterns_8classes_6bands.json")
# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_classTWDTW(point.tb, patterns.tb, bands)

# # plot the classification
plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
plot(x = matches, type = "alignments")

# read sample information from a CSV archive,
# recover the time series from the WTSS service
# put data and metadata on a SITS table
embrapa.tb <- sits_fromCSV ("./data/Samples/damien.csv", n_max = 5)





# plot some of the time series (only the evi and ndvi bands)
embrapa.tb %>%
     sits_plot()

     sits_smooth (lambda = 0.5) %>%
     sits_merge(embrapa.tb[1:10,], .) %>%
     sits_select (c("evi", "evi_smooth")) %>%
     sits_plot ()



# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
matches <- sits_twdtw_classify(embrapa.tb[1,], patterns.tb, bands)

dtwSat::plot(x = matches, type = "classification", overlap = 0.5)

dtwSat::plot(x = matches, type = "alignments")

matches.tb <- matches[]

