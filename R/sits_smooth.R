# -----------------------------------------------------------
#' Smooth the time series using Whittaker smoother
#'
#' \code{sits_smooth} returns a database with raw smoothed sits time series
#'
#' The input and the output are two databases. The input database must exist.
#'
#' The degree of smoothing depends on smoothing factor lambda (usually from 0.5 to 10.0)
#' Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong smoothing
#'
#' @param table_in   a string - the name of the database with original time series  (must exist)
#' @param table_out  a string - the name of the database with original time series  (is created)
#' @param bands_in   a vector - the names of the raw bands to be smoothed
#' @param bands_out  a vector - the names of the output bands
#' @param lambda     double   - the smoothing factor to be applied
#' @return           a tibble with smoothed sits time series
#' @keywords sits
#' @family   sits auxiliary functions
#' @examples sits_smooth ("raw.tb", "smoothed.tb", c("ndvi", "evi"), c("ndvi_smooth", "evi_smooth"), 5.0)
#' @export
sits_smooth <- function (table_in,
                         bands_in  = bands.gl,
                         bands_out = bands_s.gl,
                         lambda    = 5.0) {

     # function to smooth the bands of a time series
     smooth_Whittaker <- function (ts) {
          for (b in bands_in) ts[[b]]  <- whit1(ts[[b]], lambda = lambda)
          return (ts)
     }
     # does the database exist?
     sits_assert_table(table_in)
     # test if the input database has at least one row of data
     #sits_is_empty (table_in)
     # extract the time series data from the sits table
     data1.tb <- table_in$time_series
     # smooth the time series
     smoothed.tb <- data1.tb %>%
          map(smooth_Whittaker)
     # rename the time series
     data2.tb <- smoothed.tb %>%
          map (function (ts) {
               ts_out <- ts
               colnames (ts_out) <- c("Index", bands_out)
               return (ts_out)
          })

     # create a new database by copying metadata information from the input sits database
     out.tb <- select (table_in, latitude, longitude, from, to, label, coverage, time_series)
     # insert the new time series
     out.tb$time_series <-  data2.tb
     # return the result
     return (out.tb)
}
