# -----------------------------------------------------------
#' Merge two satellite image time series
#'
#' \code{sits_merge} merges two STIS tables with the same spatio-temporal references
#'
#' #' To merge two series, we consider that they contain different
#' attributes but refer to the same coverage, and spatio-temporal location.
#'
#' This function is useful to merge different bands of the same spatio-temporal locations.
#' For example, one may want to put the raw and smoothed bands for the same set of locations
#' in the same table.
#'
#' @param ts1  the first SITS table to be merged
#' @param ts2  the second SITS table to be merged
#' @return new.tb    a merged SITS tibble with a nested set of time series
#' @keywords SITS
#' @family   SITS main functions
#' @export
#' @examples merged.tb <- merge_WTSS (sits1.tb, sits2.tb)
#'
sits_merge <-  function(sits1.tb, sits2.tb) {

     # merge the time series
     merge_one <-  function (ts1, ts2) {
          ts3 <- left_join (ts1, ts2, by = "Index")
     }
     # first, select the metadata columns
     merged.tb <- sits1.tb %>%
          select (latitude, longitude, from, to, label, coverage)

     # then merge the data sets
     sits1.data <- sits1.tb$time_series
     sits2.data <- sits2.tb$time_series
     # join the attributes and values using zoo merge
     merged.tb$time_series <- map2 (sits1.data, sits2.data, merge_one)

     return (merged.tb)
}
