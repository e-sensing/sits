#' Finds the names of the bands of a time series
#'
#' \code{sits_band_names} tests if the sits table exists and has data
#' @param ts     a time series extracted from a sits table
#' @return names a string vector with the names of the bands
#' @export
#'
sits_band_names <- function (ts) {
     mynames <- ts %>%
          data.frame() %>%
          colnames() %>%
          . [2:length(.)]
     return (mynames)
}
