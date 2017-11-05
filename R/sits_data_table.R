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
