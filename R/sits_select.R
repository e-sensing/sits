#' Filter bands on a SITS table
#'
#' \code{sits_select} returns a sits table with the selected bands
#'
#' @param data.tb - a sits table with the time series of the selected bands
#' @param bands   - a vector of bands
#' @return table  a tibble in SITS format with the selected bands
#' @export

sits_select <- function (data.tb, bands) {
     # create a new table to store the result
     new.tb <- sits_table()
     # select the metadata attributes from the input table
     new.tb <- dplyr::select (data.tb, longitude, latitude, start_date, end_date, label, coverage)
     # select the chosen bands for the time series
     new.tb$time_series <- data.tb$time_series %>%
          map (function (ts) ts <- ts [,c("Index", bands)])
     # return the result
     return (new.tb)
}
