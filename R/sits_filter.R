# -----------------------------------------------------------
#' Filter a set of satellite image time series
#' @name sits_filter
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This function provides access to different types of filters
#'
#' @param    data.tb       tibble - a SITS table with the list of time series to be plotted
#' @param    type          string - the type of filter to be used
#' @param    lambda        double   - the smoothing factor to be applied for Whittaker smoothing
#' @param    window_size   an integer informing the window size for envelop calculation. See compute_envelop details.
#' @return   output.tb     a SITS table with the filtered time series
#' @export
#'
sits_filter <- function (data.tb = NULL, type = "whittaker", lambda = 0.5, window_size = 1) {
     # check the input exists
     ensurer::ensure_that(data.tb, !purrr::is_null(.), err_desc = "sits_filter: input data not provided")
     # check valid methods
     ensurer::ensure_that(method, (. == "whittaker" || . == "upper_bound"),
                          err_desc = "sits_filter: valid methods are 'whittaker' and 'upper_bound'.")
     switch(type,
            "whittaker"      =
                 { output.tb <- sits_whittaker  (data.tb, lambda = lambda)},
            "upper_bound"    =
                 { output.tb <- sits_upper_bound (data.tb, window_size = window_size) }
            )
     # return the result
     return (output.tb)
}

#' Smooth the time series using Whittaker smoother (based on PTW package)
#' @name sits_whittaker
#' @description  The algorithm searches for an optimal polynomial describing the warping.
#' The degree of smoothing depends on smoothing factor lambda (usually from 0.5 to 10.0)
#' Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong smoothing
#'
#' @param data.tb    The SITS tibble containing the original time series
#' @param lambda     double   - the smoothing factor to be applied
#' @return output.tb a tibble with smoothed sits time series
#' @export
sits_whittaker <- function (data.tb, lambda    = 0.5) {
     # extract the time series data from the sits table
     data.ts <- data.tb$time_series
     # what are the input bands?
     bands  <- sits_bands (data.tb)
     # smooth the time series using Whittaker smoother
     smoothed.ts <- data.ts %>%
          purrr::map(function (ts) {
               for (b in bands) ts[[b]]  <- ptw::whit2(ts[[b]], lambda = lambda)
               return (ts) })

     # create a new SITS table by copying metadata information from the input sits database
     output.tb <- dplyr::select (data.tb, latitude, longitude, start_date, end_date, label, coverage, time_series)
     # insert the new time series
     output.tb$time_series <-  smoothed.ts
     # return the result
     return (output.tb)
}

#' @title Upper bound filter
#' @name sits_upper_bound
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function computes the envelope of a time series using the
#' streaming algorithm proposed by Lemire (2009). This functions calls `dtwclust::compute_envelop` function.
#' @param data.tb       a valid sits table
#' @param window_size   an integer informing the window size for envelop calculation. See compute_envelop details.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_upper_bound <- function(data.tb, window_size = 1){
     # compute envelopes
     result.tb <- sits_apply(data.tb,
                                fun = function(band) dtwclust::compute_envelop(band, window.size = window_size, error.check = FALSE),
                                fun_index = function(band) band)

     return(result.tb)

}
