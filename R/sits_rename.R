#' Finds the names of the bands of a time series
#'
#' \code{sits_renames} renames the bands of a sits table
#' @param in.tb    a SITS table with a list of SITS time series
#' @param bands    a list of new band names
#' @return out.tb  a SITS table with a list of renamed bands for the time series
#' @export
#'
sits_rename <-  function (in.tb, bands_new) {

     if (is.null(bands_new)) message (paste, "New band names should be provided")

     # rename the time series
     out.ts <- in.tb$time_series %>%
          purrr::map (function (ts) {
               ts_out <- ts
               colnames (ts_out) <- c("Index", bands_new)
               return (ts_out)
     })
     out.tb <- dplyr::select (in.tb, latitude, longitude, start_date, end_date, label, coverage)
     out.tb$time_series <- out.ts

     return (out.tb)
}
