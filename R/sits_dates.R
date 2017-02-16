#' Return the dates of a sits table
#'
#' \code{sits_dates} returns a sits table with dates only (colwise organised)
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @return table   a tibble in SITS format with values of time indexes
#' @export

sits_dates <- function (data) {
     values <- data$time_series %>%
          data.frame() %>%
          as_tibble() %>%
          dplyr::select (dplyr::starts_with ("Index"))
     return (values)
}
#'
#' Aligns dates of time series to a reference date
#'
#' \code{sits_align} convert the index of a sits time to a reference year
#' The reference year is taken from the date of the start of the time series
#' available in the coverage.
#'
#'
#' @param    data.tb    tibble - input SITS table (useful for chaining functions)
#' @param    ref_date   date   - a reference date where all series will start
#' @return   data1.tb   tibble - the converted SITS table (useful for chaining functions)
#' @keywords STIS
#' @family   STIS main functions
#' @examples data1.tb <- sits_align (data = "mydata.tb", ref_date = "2000-09-02")
#' @export
#'
sits_align <- function (data, ref_date) {
     ts <- data$time_series
     # convert the time index to a reference year
     ts1 <- ts %>%
          map (function (t) {
               df <- as.data.frame(t)
               start_date <- as.Date(df[1,"Index"])
               if (abs (yday(start_date) - yday(ref_date)) > 2) {
                    print (start_date, ref_date)
                    message (paste ("sits_align: time series do not start at the same date"))
               }
               dplyr::mutate (t, Index = Index - ymd(start_date) + ymd (ref_date))
          })
     data$time_series <- ts1
     return (data)
}
