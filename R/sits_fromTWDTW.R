#
#' Transform patterns from .RData to JSON
#'
#'
sits_fromTWDTW <- function (patterns, coverage = cov_name.gl){
     sits_assertWTSS()
     patterns.tb <- sits_table()
     for (i in (1:length (patterns@timeseries))){
          series <- unname(patterns@timeseries[i])[[1]]
          time_series <- fortify.zoo(series)
          mylist <- list()
          mylist [[1]]  <- as_tibble (time_series)
          patterns.tb <- add_row (patterns.tb,
                                  longitude    = 0.00,
                                  latitude     = 0.00,
                                  from         = time_series[1,"Index"],
                                  to           = time_series[nrow(time_series),"Index"],
                                  label        = as.character (patterns@labels[i]),
                                  coverage     = coverage,
                                  time_series  = mylist)
     }
     return (patterns.tb)
}



