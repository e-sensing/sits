# ---------------------------------------------------------------
#
#  This file contain a list of time series filters
#  As a rule, filters are functions that apply a 1D function to a
#  time series and produce new values as a result
#
#  The package provides the generic method sits_apply to apply a
#  1D generic function to a time series and specific methods for
#  common tasks such as missing values removal and smoothing
#  
#  The following filters are supported: Savitsky-Golay, Whittaker and envelope
#
# ---------------------------------------------------------------

#' @title Apply a function over SITS bands.
#' @name sits_apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  `sits_apply` returns a sits_table with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_ts_apply` for each band,
#' whose vector values is passed as the function argument.
#'
#' `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' The boolean parameter `original` controls whether the original bands should be also part of
#' the out SITS table.
#'
#' @param data.tb       a valid sits table
#' @param fun           a function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     a function with one parameter as input and a Date vector as output.
#' @param bands_suffix a string informing the resulting bands name's suffix.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_apply <- function(data.tb, fun, fun_index = NULL, bands_suffix = "") {
  # get the bands in the data
  bands <- sits_bands(data.tb)
  ensurer::ensure_that(bands, length(.) > 0, err_desc = "sits_apply: at least one band should be provided.")
  
  data.tb$time_series <- data.tb$time_series %>%
    purrr::map(function(ts.tb) {
      ts_computed.lst <- dplyr::select(ts.tb, dplyr::one_of(bands)) %>%
        purrr::map(function(band) {
          result <- fun(band)
          return(result)
        })
      
      # append bands names' suffixes
      if (nchar(bands_suffix) != 0)
        names(ts_computed.lst) <- ifelse(bands == "Index", "Index", paste0(bands, ".", bands_suffix))
      
      # unlist if there are more than one result from `fun`
      if (is.recursive(ts_computed.lst[[1]]))
        ts_computed.lst <- unlist(ts_computed.lst, recursive = FALSE)
      
      # convert to tibble
      ts_computed.tb <- tibble::as_tibble(ts_computed.lst)
      ensurer::ensure_that(ts_computed.tb, (any(names(.) == "Index") | !is.null(fun_index)),
                           err_desc = "sits_apply: computed time series does not have `Index` column.
                           Add `Index` in `bands` argument or provide a function to `fun_index`
                           in order to compute a `Index` column.")
      
      if (!is.null(fun_index))
        ts_computed.tb <- dplyr::mutate(ts_computed.tb, Index = fun_index(ts.tb$Index))
      
      return(dplyr::select(ts_computed.tb, Index, dplyr::everything()))
    })
  
  return(data.tb)
}
#' @title Lagged differences of a SITS band.
#' @name sits_lag_diff
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the lagged differences of a set of time series.
#' @param data.tb       a valid sits table
#' @param bands         a vector of strings with band's names.
#' @param differences   an integer indicating the order of the difference.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_lag_diff <- function(data.tb, bands = NULL, differences = 1) {
  if (is.null(bands))
    bands <- sits_bands(data.tb)
  
  ensurer::ensure_that(bands, length(.) > 0, err_desc = "sits_ts_diff: at least one band should be provided.")
  
  result.tb <- data.tb
  
  # compute differential
  result.tb <- sits_apply(data.tb,
                          fun = function(band) diff(band, lag = 1, differences = differences),
                          fun_index = function(band) band[0:-differences],
                          bands_suffix = paste0("diff", differences))
  
  return(result.tb)
}
#' @title Inerpolation function of sits_table's time series
#' @name sits_linear_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands for a given resolution
#' using the R base function approx
#' @param data.tb       a valid sits table
#' @param n             the number of time series elements to be created between start date and end date
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_linear_interp <- function(data.tb, n = 23){
  # get the bands of the SITS tibble
  bands <- sits_bands(data.tb)
  ensurer::ensure_that(bands, length(.) > 0, err_desc = "sits_ts_approx: at least one band should be provided.")
  
  # compute linear approximation
  result.tb <- sits_apply(data.tb,
                          fun = function(band) stats::approx(band, n = n, ties=mean)$y,
                          fun_index = function(band) as.Date(stats::approx(band, n = n, ties=mean)$y,
                                                             origin = "1970-01-01"))
  
  return(result.tb)
}

#' @title Inerpolation function of sits_table's time series
#' @name sits_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands for a given resolution
#' using the R base function approx
#' @param data.tb       a valid sits table
#' @param fun           the interpolation function to be used
#' @param n             the number of time series elements to be created between start date and end date
#' @param ...           additional parameters to be used by the fun function
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_interp <- function(data.tb, fun = stats::approx, n = 23, ...){
  # get the bands of the SITS tibble
  bands <- sits_bands(data.tb)
  ensurer::ensure_that(bands, length(.) > 0, err_desc = "sits_interp: at least one band should be provided.")
  
  # compute linear approximation
  result.tb <- sits_apply(data.tb,
                          fun = function(band) fun(band, n = n, ...)$y,
                          fun_index = function(band) as.Date(fun(band, n = n, ...)$y,
                                                             origin = "1970-01-01"))
  
  return(result.tb)
}
#' @title Remove missing values
#' @name sits_missing_values
#' @author Gilberto Camarara, \email{gilberto.camara@inpe.br}
#' @description  This function removes the missing values from an image time series
#' @param data.tb   a valid sits table
#' @param mv        a number indicating missing values in a time series.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
#'
sits_missing_values <-  function(data.tb, mv = NULL) {
  # get the bands in the data
  bands <- sits_bands(data.tb)
  ensurer::ensure_that(bands, length(.) > 0, err_desc = "sits_missing_values: at least one band should be provided.")
  
  # copy the results
  time_series <- data.tb$time_series
  
  # update missing values to NA
  for (b in bands){
    time_series[,b][time_series[,b] == mv] <- NA
  }
  
  # interpolate missing values
  time_series[,bands] <- zoo::na.spline(time_series[,bands])
  
  data.tb$time_series <- time_series
  
  return (data.tb)
}


#' @title Envelope filter
#' @name sits_envelope
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function computes the envelope of a time series using the
#' streaming algorithm proposed by Lemire (2009). This functions calls `dtwclust::compute_envelop` function.
#' @param data.tb       a valid sits table
#' @param window_size   an integer informing the window size for envelop calculation. See compute_envelop details.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_envelope <- function(data.tb, window_size = 1){
  # compute envelopes
  result.tb <- sits_apply(data.tb,
                          fun = function(band) dtwclust::compute_envelope(band, window.size = window_size, error.check = FALSE),
                          fun_index = function(band) band)
  
  return(result.tb)
  
}

#' Smooth the time series using Whittaker smoother (based on PTW package)
#' @name sits_whittaker
#' @description  The algorithm searches for an optimal polynomial describing the warping.
#' The degree of smoothing depends on smoothing factor lambda (usually from 0.5 to 10.0)
#' Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong smoothing
#'
#' @param data.tb    The SITS tibble containing the original time series
#' @param lambda     double   - the smoothing factor to be applied
#' @param bands_suffix the suffix to be appended to the smoothed filters
#' @return output.tb a tibble with smoothed sits time series
#' @export
sits_whittaker <- function (data.tb, lambda    = 0.5, bands_suffix = "whit") {
  result.tb <- sits_apply(data.tb,
                          fun = function(band) ptw::whit2(band, lambda = lambda),
                          fun_index = function(band) band,
                          bands_suffix = bands_suffix)
  
  return(result.tb)
}

#' Smooth the time series using Savitsky-Golay filter (based on signal package)
#' @name sits_sgolay
#' @description  The algorithm searches for an optimal polynomial describing the warping.
#' The degree of smoothing depends on smoothing factor lambda (usually from 0.5 to 10.0)
#' Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong smoothing
#'
#' @param data.tb    The SITS tibble containing the original time series
#' @param order      filter order
#' @param scale      time scaling
#' @param bands_suffix the suffix to be appended to the smoothed filters
#' @return output.tb a tibble with smoothed sits time series
#' @export
sits_sgolay <- function (data.tb, order = 3, scale = 1, bands_suffix = "sg") {
  result.tb <- sits_apply(data.tb,
                          fun = function(band) signal::sgolayfilt(band, p = order, ts = scale),
                          fun_index = function(band) band,
                          bands_suffix = bands_suffix)
  
  return(result.tb)
}


#' Kalman filter
#' @name sits_kf
#' @description  A simple Kalman filter implementation
#'
#' @param data.tb    The SITS tibble containing the original time series
#' @return output.tb a tibble with smoothed sits time series
sits_kf <- function(data.tb, bands_suffix = "kf"){
  result.tb <- sits_apply(data.tb,
                          fun = function(band) .kalmanfilter(band, NULL, NULL, NULL),
                          fun_index = function(band) band,
                          bands_suffix = bands_suffix)
  return(result.tb)
}


# Compute the Kalman filter
#
# @param measurement                    A vector of measurements
# @param initial_error_in_estimate      Initial error in the estimation
# @param error_in_measurement           A vector of errors in the measuments
# @param initial_estimate               A first estimation of the measurement
# @return
.kalmanfilter <- function(measurement, initial_error_in_estimate = NULL, error_in_measurement = NULL, initial_estimate = NULL){
  kg <- vector(mode = "logical", length = length(measurement + 1))
  est <- vector(mode = "logical", length = length(measurement + 1))
  e_est <- vector(mode = "logical", length = length(measurement + 1))
  #
  if(is.null(initial_estimate)){
    initial_estimate <- base::mean(measurement)
  }
  if(is.null(initial_error_in_estimate)){
    initial_error_in_estimate <- abs(measurement[1] - base::mean(measurement))
  }
  if(is.null(error_in_measurement)){
    error_in_measurement <- rep(sd(measurement), length.out = length(measurement))
  }
  #
  est[1] <- initial_estimate[1]
  e_est[1] <- initial_error_in_estimate[1]
  for(i in 2:(length(measurement) + 1)){
    kg[i] <- .KG(e_est[i - 1], error_in_measurement[i - 1])
    est[i] <- .EST_t(kg[i], est[i - 1], measurement[i - 1])
    e_est[i] <- .E_EST_t(kg[i], e_est[i - 1])
  }
  return(list(estimation = est, error_in_estimate = e_est, kalman_gain = kg))
}

# Compute the Kalman gain
#
# @param e_est    error in estimation
# @param e_mea    error in measurement
# @return         the Kalman gain
.KG <- function(e_est, e_mea){
  return(e_est/(e_est + e_mea))
}

# Compute the KF current estimate
#
# @param kg        Kalman gain
# @param est_t1    previous estimate
# @param mea       measurement
# @return          current estimate
.EST_t <- function(kg, est_t1, mea){
  est_t1 + kg * (mea - est_t1)
}

# Compute the KF error in the estimation
#
# @param kg        Kalman gain
# @param e_est_t1  previous error in estimation
.E_EST_t <- function(kg, e_est_t1){
  (1 - kg) * e_est_t1
}

