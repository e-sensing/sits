#' Finds the names of the bands of a time series
#'
#' \code{sits_bands} finds the names of the bands of time series in a sits table
#' @param sits.tb     a valid sits table
#' @return names      a string vector with the names of the bands
#' @export
#'
sits_bands <- function (sits.tb) {
     names <- sits.tb[1,]$time_series %>%
          data.frame() %>%
          colnames() %>%
          . [2:length(.)]
     return (names)
}
