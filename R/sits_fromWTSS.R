#'
#' Obtain one timeSeries from WTSS server and load it on a sits table
#'
#' \code{sits_fromWTSS} returns one set of time series provided by a WTSS server
#'
#' Given a location (lat/long), and start/end period, and the WTSS server information
#' retrieve a time series and include it on a stis table
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, lable, time_series>
#'
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series (optional)
#' @return table          the table with the data
#' @keywords SITS
#' @family   SITS main functions
#' @examples sits_fromWTSS (-53.30, -14.24, "2000-02-18", "2016-09-24")
#' @export
#'
sits_fromWTSS <- function  (longitude  =          -55.51810,
                            latitude   =          -11.63884,
                            start_date =     start_date.gl,
                            end_date   =       end_date.gl,
                            label      =         "NoClass") {

     # internal function to remove the missing values
     clean_ts <- function (tb){

          for (b in bands.gl) {
               # what is the missing value for this band?
               mv <- select ((filter (cov_desc.gl, band == b)), missing_value)*0.0001
               # is the first value a missing_value?
               if (tb[1,b] == mv) tb[1,b] <- tb[2,b]
               # is the last value a missing_value?
               if (tb[nrow(tb),b] == mv) tb[nrow(tb)] <-  tb[nrow(tb) - 1,b]
               # all other missing values are replaced by an average of their neighbors
               for (i in 2:(nrow(tb) -1) ) {
                    if (tb[i,b] == mv) tb[i,b] <- 0.5*tb[i-1,b] + 0.5*tb[i+1,b]
               }
          }
          return (tb)
     }

     # is the WTSS service running?
     sits_testWTSS()
     # get a time series from the WTSS server
     ts <- timeSeries (ts_server.gl,
                       coverages  = cov_name.gl,
                       attributes = bands.gl,
                       longitude  = longitude,
                       latitude   = latitude,
                       start      = start_date,
                       end        = end_date
     )

     # retrieve the time series information
     time_series <- ts[[cov_name.gl]]$attributes

     # scale the time series
     time_series[,bands.gl] <-  time_series[,bands.gl]*0.0001

     # convert the series to a tibble
     row.tb <- as_tibble (fortify.zoo (time_series))
     # clean the time series
     #row.tb <- clean_ts (row.tb)
     # create a list to store the zoo time series coming from the WTSS service
     ts.lst <- list()
     # transform the zoo list into a tibble to store in memory
     ts.lst[[1]] <- row.tb

     # create a table to store the WTSS data
     table <- sits_table()
     # add one row to the table
     table <- add_row (table,
                       longitude    = longitude,
                       latitude     = latitude,
                       start_date   = as.Date(start_date),
                       end_date     = as.Date(end_date),
                       label        = label,
                       coverage     = cov_name.gl,
                       time_series  = ts.lst
     )

     # return the table with the time series
     return (table)
}



