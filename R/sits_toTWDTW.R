#' Classify a sits tibble using TWDTW (using the dtwSat package)
#'
#' \code{sits_toTWDTW} returns a sits table with values only
#'
#' Converts data from a SITS table to an instance of a TWDTW time series class
#'
#' Reference: Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' “A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping.” IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data.tb       a table in SITS format with time series to be converted to TWTDW time series
#' @return ts.tw         a time series in TWDTW format
#' @export
#'
#'
sits_toTWDTW <- function (data.tb){
     zoo.ls <- data.tb$time_series %>%
          map (function (ts) {
               df <- data.frame (ts)
               return (zoo (df[,2:ncol(df)], df[,1]))
          })
     labels.fc <-  as.factor (data.frame (select (data.tb, label))[,1])

     ts.tw <-  new("twdtwTimeSeries", timeseries = zoo.ls,
               labels = labels.fc)
     return (ts.tw)
}

