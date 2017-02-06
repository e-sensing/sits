#' Filter bands on a SITS table
#'
#' \code{sits_select} returns a sits table with the selected bands
#'
#' @param data.tb - a sits table with the time series of the selected bands
#' @param bands   - a vector of bands
#' @return table  a tibble in SITS format with the selected bands
#' @export

sits_select <- function (data.tb, bands) {
     new.tb <- sits_table()
     new.tb <- dplyr::select (data.tb, longitude, latitude, start_date, end_date, label, coverage)
     time_series.ls <- data.tb$time_series
     ts <- list()
     # for (i in 1:length (time_series.ls)) {
     #      ts[[i]] <- time_series.ls[[i]] %>%
     #           data.frame() %>%
     #           .[,c("Index", bands)]
     # }
     for (i in 1:length (time_series.ls)) {
          tb <- time_series.ls[[i]]
          print (tb)
          ts[[i]] <- tb [,c('Index', bands)]
     }
     new.tb$time_series <- ts
     return (new.tb)
}
