# -----------------------------------------------------------
#' Smooth the time series using Whittaker smoother (based on PTW package)
#'
#' \code{sits_smooth} returns a database with raw smoothed sits time series
#'
#' The input and the output are two databases. The input database must exist.
#'
#' The algorithm searches for an optimal polynomial describing the warping.
#' It is possible to align one sample to a reference,
#' several samples to the same reference, or several samples to several references.
#'
#' The degree of smoothing depends on smoothing factor lambda (usually from 0.5 to 10.0)
#' Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong smoothing
#'
#' @param table_in   a string - the name of the database with original time series  (must exist)
#' @param lambda     double   - the smoothing factor to be applied
#' @return           a tibble with smoothed sits time series
#' @keywords sits
#' @family   sits auxiliary functions
#' @examples sits_smooth ("raw.tb", lambda = 0.5)
#' @export
sits_smooth <- function (table_in, lambda    = 0.5) {

     # does the database exist?
     sits_assert(table_in)
     # extract the time series data from the sits table
     data1.ts <- table_in$time_series
     # what are the input bands?
     bands_in  <- sits_bands (table_in)
     # make the names of the output bands
     bands_out <- as.character(map (bands_in, function (name)
                                   {new_name <- paste(name,"_smooth", sep="")
                                    return (new_name) }))
     # smooth the time series using Whittaker smoother
     smoothed.ts <- data1.ts %>%
          purrr::map(function (ts) {
                    for (b in bands_in) ts[[b]]  <- ptw::whit2(ts[[b]], lambda = lambda)
                    return (ts) })
     # rename the time series
     data2.ts <- sits_rename (smoothed.ts, bands_out)

     # create a new database by copying metadata information from the input sits database
     out.tb <- dplyr::select (table_in, latitude, longitude, start_date, end_date, label, coverage, time_series)
     # insert the new time series
     out.tb$time_series <-  data2.ts
     # return the result
     return (out.tb)
}
