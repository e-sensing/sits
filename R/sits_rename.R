#' Finds the names of the bands of a time series
#'
#' \code{sits_bands} finds the names of the bands of time series in a sits table
#' @param in.ts    a list of SITS time series
#' @param bands    a list of new band names
#' @return out.ts  a list of renamed SITS time series
#' @export
#'
sits_rename <-  function (in.ts, bands_new) {

     # rename the time series
     out.ts <- in.ts %>%
          purrr::map (function (ts) {
               ts_out <- ts
               colnames (ts_out) <- c("Index", bands_new)
               return (ts_out)
     })
}
