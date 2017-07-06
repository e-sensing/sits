# -----------------------------------------------------------
#' Filter a set of satellite image time series
#'
#' \code{sits_filter} filters one or more time series from a SITS table
#'
#' This function provides access to different types of filters
#'
#' @param    data.tb    tibble - a SITS table with the list of time series to be plotted
#' @param    type       string - the type of plot to be generated
#' @param lambda     double   - the smoothing factor to be applied
#' @param    bands         a vector of strings with band's names.
#' @param    window_size   an integer informing the window size for envelop calculation. See compute_envelop details.
#' @return   data.tb    tibble - the input SITS table (useful for chaining functions)
#' @export
#'
sits_filter <- function (data.tb = NULL, type = "whittaker", bands = NULL, lambda = 0.5, window_size = 1) {
     # check the input exists
     ensurer::ensure_that(data.tb, !purrr::is_null(.), err_desc = "sits_plot: input data not provided")

     switch(type,
            "whittaker"      =
                 { output.tb <- sits_whittaker  (data.tb, lambda = lambda)},
            "upper_bound"    =
                 { output.tb <- sits_upper_bound (data.tb, window_size = window_size) }
            )

     # return the result
     return (output.tb)
}



# -----------------------------------------------------------
#' Smooth the time series using Whittaker smoother (based on PTW package)
#'
#' \code{sits_whittaker} returns a SITS table with a smoothed sits time series
#'
#' The input and the output are two SITS tables.
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
#' @export
sits_whittaker <- function (table_in, lambda    = 0.5) {
     # extract the time series data from the sits table
     data1.ts <- table_in$time_series
     # what are the input bands?
     bands_in  <- sits_bands (table_in)
     # smooth the time series using Whittaker smoother
     smoothed.ts <- data1.ts %>%
          purrr::map(function (ts) {
               for (b in bands_in) ts[[b]]  <- ptw::whit2(ts[[b]], lambda = lambda)
               return (ts) })

     # create a new SITS table by copying metadata information from the input sits database
     out.tb <- dplyr::select (table_in, latitude, longitude, start_date, end_date, label, coverage, time_series)
     # insert the new time series
     out.tb$time_series <-  smoothed.ts
     # return the result
     return (out.tb)
}

#' @title Filter function of a SITS band.
#' @name sits_upper_bound
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function computes the envelope of a time series using the
#' streaming algorithm proposed by Lemire (2009). This functions calls `dtwclust::compute_envelop` function.
#' @param data.tb       a valid sits table
#' @param bands         a vector of strings with band's names.
#' @param window_size   an integer informing the window size for envelop calculation. See compute_envelop details.
#' @param .original     a boolean informing if the original bands must be returned.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_upper_bound <- function(data.tb, bands = NULL, window_size = 1, .original = TRUE){
     if (is.null(bands))
          bands <- sits_bands(data.tb, return_index = FALSE)

     ensurer::ensure_that(bands, length(.) > 0, err_desc = "sits_upper_bound_ts: at least one band should be provided.")

     result.tb <- data.tb

     # compute envelopes
     result.tb <- sits_apply(data.tb,
                                bands = bands,
                                fun = function(band) dtwclust::compute_envelop(band, window.size = window_size, error.check = FALSE),
                                fun_index = function(band) band, .original = .original)

     return(result.tb)

}
