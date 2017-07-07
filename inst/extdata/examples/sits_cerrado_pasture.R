
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
cerrado_1.tb <- cerrado.tb[1:20,]

#plots the first 10 time series (default: put the same places in a single plot)
sits_plot (cerrado_1.tb)

#plots all time series of the same label and band together (shows dispersion)
sits_plot (cerrado.tb, type = "together")

# smooth the time series using the Whittaker smoother
# then rename the result, merge with original and plot

cerrado_1.tb %>%
     sits_filter (type = "whittaker", lambda = 5.0) %>%
     sits_rename (c("ndvi_smooth", "evi_smooth")) %>%
     sits_merge(cerrado1.tb)  %>%
     sits_select(c("evi_smooth", "evi")) %>%
     sits_plot()


train.tb <- dplyr::bind_rows(cerrado_t1.tb, cerrado_t2.tb)
# test samples
test.tb <- cerrado.tb[21:720,]
test1.tb <- cerrado.tb[21:22,]
# get the patterns using gam
patterns.tb <- sits_patterns(train.tb)
sits_plot (patterns.tb, type = "patterns")

bands <- c("ndvi", "evi")

result.tb <- sits_TWDTW (test1.tb, patterns.tb, bands)
