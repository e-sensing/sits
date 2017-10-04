#' @title Example of retrieval, plotting, filtering (Kalman filter), and classification of
#' Cerrado and Pasture temporal patterns
#' @name sits_cerrado_pasture
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sánchez, \email{alber.sanchez@@inpe.br}
#'
#' @description Retrieves a set of 736 time series of cerrado and pasture, plots the data
#' and shows various processing functions of sits_plot.
#'
#' @param cerrado.tb      a SITS tibble retrieved from a JSON file

#install library
library (sits)
# get a local data set
cerrado.tb <- readRDS(system.file("extdata/time_series/cerrado.rds", package = "sits"))

# pick the first 10 time series
cerrado1.tb <- cerrado.tb[1:20,]

#plots the first 10 time series (default: put the same places in a single plot)
sits_plot (cerrado1.tb)

#plots all time series of the same label and band together (shows dispersion)
sits_plot (cerrado.tb, type = "together")

# smooth the time series using the Kalman filter
# then rename the result, merge with original and plot

cerrado1s.tb <- sits_interp(cerrado1.tb, stats::approx, n = 50)

cerrado2.tb <- sits_interp(cerrado1.tb, stats::approx, n = 50) %>%
     sits_kf()

cerrado2.tb %>%
     sits_merge(cerrado1s.tb)  %>%
     sits_select(bands = c("evi.kf.estimation", "evi")) %>%
     sits_plot()

cerrado3.tb <- cerrado1.tb %>%
     sits_interp (stats::approx, n = 50) %>%
     sits_envelope() %>%
     sits_kf()

cerrado3.tb %>%
     sits_merge(cerrado1s.tb)  %>%
     sits_select(bands = c("evi.env.kf.estimation", "evi")) %>%
     sits_plot()

