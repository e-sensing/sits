# satellite image time series
# example of clustering
# Takes a set of 736 time series with two labels ("Cerrado" and "Pasture")
# and find a set of clusters that group the data
#devtools::install_github("gilbertocamara/sits")
library(sits)

# this data set as tow bands and is stored in a JSON file
bands <- c("ndvi", "evi")

# retrieve a set of samples from a JSON file
# Plot all the series for each label and band together
# Shows the dispersion and the noise of the data
series.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits")) %>%
     sits_plot(type = "together")

#Smooth the data and plot the result to see how smoothing reduces noise (and also signal)
series_s.tb <- series.tb %>%
     sits_whittaker(lambda = 2.0) %>%
     sits_plot(type = "together")

# select the series with label "Cerrado"
cerrado.tb <- dplyr::filter (series.tb, label == "Cerrado")

# plot the samples separately
sits_plot (cerrado.tb[1:10,], type = "one_by_one")
# join series from the same place and different years - plot them together
sits_plot (cerrado.tb[1:20,], type = "allyears")

# create patterns using the gam method (default)
patt_mt.tb <- sits_patterns(series.tb) %>%
     sits_plot (type = "patterns")

# cluster the raw series using dendogram
series_dend.tb <- sits_cluster(series.tb, bands = bands, method = "dendogram",  n_clusters = 4) %>%
     sits_plot(type = "one_by_one")

# cluster the raw series using centroids
series_dend.tb <- sits_cluster(series.tb, method = "centroids", bands = bands, n_clusters = 4) %>%
     sits_plot(type = "one_by_one")

# cluster the smoothed series using dendogram
series_dend_s.tb <- series.tb %>%
     sits_whittaker(lambda = 2.0) %>%
     sits_cluster(bands = c("ndvi.whit", "evi.whit"), method = "dendogram",  n_clusters = 4) %>%
     sits_plot(type = "one_by_one")

# create patterns using the dendogram method (using the smoothed series) and plot the result
patt_mt_d.tb <- sits_patterns(series_s.tb, bands = c("ndvi.whit", "evi.whit"), method = "dendogram") %>%
     sits_plot ( type = "patterns")

#compare the results of the dendogram-based patterns with those of the GAM-based one
# GAM is done with the unsmoothed series
patt_mt_g.tb <- sits_patterns(series.tb, bands = c("ndvi", "evi"), method = "gam") %>%
     sits_plot ( type = "patterns")

# We can also combine dendogram and GAM
patt_mt_dg.tb <- sits_patterns(series_s.tb, bands = c("ndvi.whit", "evi.whit"), method = "dendogram", apply_gam = TRUE) %>%
     sits_plot ( type = "patterns")

series2.tb <- dplyr::bind_rows(series.tb[1:10,], series.tb[691:700,])
# The results of each patterns can be used for cross validation to determine which
# methods has the better performance
# in the example we use 5 rounds for convenience - in real cases 100 times would be better
cv_gam <- sits_cross_validate (series2.tb, method = "gam", bands = c("ndvi", "evi"), times = 2, perc = 0.5)
print (cv_gam)
