#'
#' Transform patterns from TWDTW format
#' \code{sits_fromTWDTW} reads a set of TWDTW patterns
#' transforms them into a SITS table
#'
#' @param patterns - a TWDTW object containing a set of patterns to be used for classification
#' @return sits.tb  - a SITS table containing the patterns
#' @export
#'
sits_fromTWDTW <- function (patterns){
     # get the time series from the patterns
     tb.lst <- map2 (patterns@timeseries, patterns@labels, function (ts, lab) {
          # tranform the time series into a row of a sits table
          ts.tb <- fortify.zoo(ts)
          # store the sits table in a list
          mylist        <- list()
          mylist [[1]]  <- as_tibble (ts.tb)
          # add the row to the sits table
          row   <- tibble(longitude    = 0.00,
                                            latitude     = 0.00,
                                            start_date   = ts.tb[1,"Index"],
                                            end_date     = ts.tb[nrow(ts.tb),"Index"],
                                            label        = as.character (lab),
                                            coverage     = cov_name.gl,
                                            time_series  = mylist)
          return (row)
          })
     # create a sits table to store the result
     patterns.tb <- sits_table()
     patterns.tb <- tb.lst %>%
          map_df (function (row) {
               bind_rows (patterns.tb, row)
          })
     return (patterns.tb)
}



