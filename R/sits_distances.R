
#' @title Use time series values from a sits tibble as distances for training patterns
#' @name sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. Instead of first estimating a set
#' of idealised patterns and then computing distances from these patterns,
#' the attributes used to train the model are the series themselves.
#' This function then extracts the time series from a SITS tibble and
#' "spreads" them in time to produce a tibble with distances.
#'
#' @param  data.tb        a SITS tibble with original data
#' @param  shift          Adjustment value to avoid negative pixel vales
#' @return distances.tb  a tibble where columns have the reference label and the time series values as distances
#' @export
sits_distances <- function(data.tb = NULL, shift = 3.0){
    # create a list with the time series transposed from columns to rows
    ts.lst <- data.tb$time_series %>%
        purrr::map (function (ts){
            as.data.frame(t(unlist(ts[-1])))
        })
    # bind the lists of time series together
    dist.tb <- data.table::rbindlist(ts.lst)
    # shift the values of the time series to avoid negative numbers
    dist.tb <- dist.tb + shift
    # create a data frame with the first two columns for training
    distances.tb <- data.frame("original_row" = 1:nrow(data.tb), "reference" = data.tb$label)
    # join the two references columns with the data values
    distances.tb <- cbind(distances.tb, dist.tb)

    return(distances.tb)
}
#' @title Create an empty distance table to store the results of distance metrics
#' @name sits_distance_table
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty distance data.table to store the results of distance metrics
#'
#' @param patterns.tb     a tibble with a set of patterns
#' @return distances.tb   a data.table to store the distances between a time series and a set of patterns
#' @export
#'
sits_distance_table <- function (patterns.tb) {

    distances.tb <- data.table::data.table(
        original_row = integer(),
        reference    = character())

    labels <- sits_labels(patterns.tb)$label
    bands  <- sits_bands (patterns.tb)

    for (b in 1:length(bands)) {
        for (l in 1:length(labels)) {
            measure <- paste0 (labels[l], ".", bands[b])
            distances.tb [measure] = double()
        }
    }
    return (distances.tb)
}
