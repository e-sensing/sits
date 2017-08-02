#' @title Example of retrieval, plotting, filtering, and classification of
#' Cerrado and Pasture temporal patterns
#' @name sits_cerrado_pasture
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Retrieves a set of 736 time series of cerrado and pasture, plots the daata
#' and shows various processing functions of sits_plot.
#'
#' @param cerrado.tb      a SITS tibble retrieved from a JSON file

#install library
library (sits)
# get a local data set
cerrado.tb <- sits_getdata("./inst/extdata/samples/cerrado.json")

# pick the first 10 time series
cerrado1.tb <- cerrado.tb[1:20,]

#plots the first 10 time series (default: put the same places in a single plot)
sits_plot (cerrado1.tb)

#plots the time series one by one
sits_plot (savanna.tb[1:10,], type = "one_by_one")


#plots all time series of the same label and band together (shows dispersion)
sits_plot (cerrado.tb, type = "together")

# interpolate the series using spline
cerrado1s.tb <- sits_interp(cerrado1.tb, stats::spline, n = 50)
sits_plot(cerrado1s.tb)

# smooth the time series using the Whittaker smoother
# Merge with original and plot
cerrado2.tb <- sits_interp(cerrado1.tb, stats::spline, n = 50) %>%
     sits_whittaker(lambda = 5.0) %>%
     sits_merge(cerrado1s.tb)  %>%
     sits_select(c("evi.whit", "evi")) %>%
     sits_plot()

# find a set of patterns to find the samples using the GAM
# Plot the result
pat_cerrado.tb <- sits_patterns(cerrado.tb, method = "gam")
sits_plot(pat_cerrado.tb, type = "patterns")

# assess the quality of the classification using TWDTW 
# perform a cross-validation
cm_gam <- sits_cross_validate (cerrado.tb, method = "gam", bands = c("ndvi","evi"), times = 20, perc = 0.5)

# cross validation results:
# Accuracy (PCC): 92.2654155495978% 
# Cohen's Kappa: 0.8435 
# Users accuracy:  Cerrado = 96.7% Pasture = 87.1% 
# Producers accuracy: Cerrado = 89.7% Pasture = 95.8%


