#' Convert a SITS table to TWDTW format
#'
#' \code{sits_toTWDTW} returns a TWDTW time series for classification
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, label, coverage, time_series>
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @return twdtw.ts   a list of time series in TWDTW format
#' @export

sits_toTWDTW <- function (data.tb){
     zoo.ls <- data.tb$time_series %>%
          map (function (ts) {
               df <- data.frame (ts)
               return (zoo (df[,2:ncol(df)], df[,1]))
          })
     labels.fc <-  as.factor (data.frame (select (data.tb, label))[,1])

     ptt = new("twdtwTimeSeries", timeseries = zoo.ls,
               labels = labels.fc)
     return (ptt)
}
