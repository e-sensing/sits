#' Return the values of one band of a SITS table
#'
#' \code{sits_value} returns a sits table with values only
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @param  band    string - a band whose values are to be extracted
#' @return table   a tibble in SITS format with values
#' @export

sits_values <- function (data, band) {
     values <- data$time_series %>%
          data.frame() %>%
          as_tibble() %>%
          dplyr::select (dplyr::starts_with (band)) %>%
          t()
     return (values)
}
