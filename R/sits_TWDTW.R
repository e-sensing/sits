#' Export data to be used by the dtwSat package
#'
#' \code{sits_toTWDTW} returns a twdtwTimeSeries object (S4)
#'
#' Converts data from a SITS table to an instance of a TWDTW time series class
#'
#' Reference: Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping. IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data.tb       a table in SITS format with time series to be converted to TWTDW time series
#' @return ts.tw         a time series in TWDTW format (an object of the twdtwTimeSeries class)
#' @export
#'
#'
sits_toTWDTW <- function (data.tb){
     zoo.ls <- data.tb$time_series %>%
          purrr::map (function (ts) {
               df <- data.frame (ts)
               return (zoo (df[,2:ncol(df)], df[,1]))
          })
     labels.fc <-  as.factor (data.frame (dplyr::select (data.tb, label))[,1])

     ts.tw <-  new("twdtwTimeSeries", timeseries = zoo.ls,
               labels = labels.fc)
     return (ts.tw)
}

#'
#' Transform patterns from TWDTW format to SITS format
#' \code{sits_fromTWDTW} reads a set of TWDTW patterns
#' transforms them into a SITS table
#'
#' @param patterns - a TWDTW object containing a set of patterns to be used for classification
#' @return sits.tb  - a SITS table containing the patterns
#' @export
#'
sits_fromTWDTW <- function (patterns, coverage){
     # get the time series from the patterns
     tb.lst <- purrr::map2 (patterns@timeseries, patterns@labels, function (ts, lab) {
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
                          coverage     = coverage,
                          time_series  = mylist)
          return (row)
     })
     # create a sits table to store the result
     patterns.tb <- sits_table()
     patterns.tb <- tb.lst %>%
          purrr::map_df (function (row) {
               bind_rows (patterns.tb, row)
          })
     return (patterns.tb)
}
