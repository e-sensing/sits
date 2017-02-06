#'
#' Transform patterns saved in .RData to a SITS table
#' \code{sits_fromRData} reads a set of TWDTW patterns saved as RData and
#' transforms them into a SITS table
#'
#' @param patterns - a TWDTW object containing a set of patterns to be used for classification
#' @return sits.tb  - a SITS table containing the patterns
#' @export
#'
sits_fromRData <- function (patterns){
     sits_assertWTSS()
     patterns.tb <- sits_table()
     for (i in (1:length (patterns@timeseries))){
          series        <- unname(patterns@timeseries[i])[[1]]
          time_series   <- fortify.zoo(series)
          mylist        <- list()
          mylist [[1]]  <- as_tibble (time_series)
          patterns.tb   <- tibble::add_row (patterns.tb,
                                  longitude    = 0.00,
                                  latitude     = 0.00,
                                  start_date   = time_series[1,"Index"],
                                  end_date     = time_series[nrow(time_series),"Index"],
                                  label        = as.character (patterns@labels[i]),
                                  coverage     = coverage,
                                  time_series  = mylist)
     }
     return (patterns.tb)
}



