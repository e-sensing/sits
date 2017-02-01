#'
#' Obtain one timeSeries from WTSS server and load it on a sits table
#'
#' \code{sits_fromWTSS} returns one set of time series provided by a WTSS server
#'
#' Given a location (lat/long), and from/to period, and the WTSS server information
#' retrieve a time series and include it on a stis table
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, lable, time_series>
#'
#' @param longitude double - the longitude of the chosen location
#' @param latitude  double - the latitude of the chosen location
#' @param from      date - the start of the period
#' @param to        date - the end of the period
#' @param label     string - the label to attach to the time series (optional)
#' @return table    the table with the data
#' @keywords SITS
#' @family   SITS main functions
#' @examples sits_fromWTSS (-53.30, -14.24, "2000-02-18", "2016-09-24")
#' @export
sits_fromWTSS <- function  (longitude =      -54.2313,
                            latitude  =      -14.0482,
                            from      =       from.gl,
                            to        =         to.gl,
                            label     =     "NoClass") {
     # is the WTSS service running?
     sits_assert_WTSS()
     # get a time series from the WTSS server
     ts <- timeSeries (ts_server.global,
                       coverages  = cov_name.gl,
                       attributes = bands.gl,
                       longitude  = longitude,
                       latitude   = latitude,
                       start      = from,
                       end        = to
     )

     # retrieve the time series information
     time_series <- ts[[cov_name.gl]]$attributes
     # scale the time series
     time_series[,bands.gl] <-  time_series[,bands.gl]*0.0001

     # create a list to store the zoo time series coming from the WTSS service
     ts.lst <- list()
     # transform the zoo list into a tibble to store in memory
     ts.lst[[1]] <- as_tibble (fortify.zoo (time_series))

     # create a table to store the WTSS data
     table <- sits_table()
     # add one row to the table
     table <- add_row (table,
                       longitude    = longitude,
                       latitude     = latitude,
                       from         = as.Date(from),
                       to           = as.Date(to),
                       label        = label,
                       coverage     = cov_name.gl,
                       time_series  = ts.lst
     )

     # return the table with the time series
     return (table)
}
