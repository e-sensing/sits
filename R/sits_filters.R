# ---------------------------------------------------------------
#
#  This file contain a list of time series filters
#  As a rule, filters are functions that apply a 1D function to a
#  time series and produce new values as a result
#
#  The package provides the generic method sits_apply (in sits_table.R) to apply a
#  1D generic function to a time series and specific methods for
#  common tasks such as missing values removal and smoothing
#
#  The following filters are supported: Savitsky-Golay, Whittaker and envelope
#
# ---------------------------------------------------------------

#' @title Inerpolation function of sits_table's time series
#' @name sits_linear_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands for a given resolution
#'               using the R base function approx
#' @param data.tb       a valid sits table
#' @param n             the number of time series elements to be created between start date and end date
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_linear_interp <- function(data.tb, n = 23){

    # test if data.tb has data
    .sits_test_table(data.tb)

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
#'               using the R base function approx
#' @param data.tb       a valid sits table
#' @param fun           the interpolation function to be used
#' @param n             the number of time series elements to be created between start date and end date.
#'                      When a class function is passed to `n`, is is evaluated with each band time series as
#'                      an argument, e.g. n(band) (default: `length` function)
#' @param ...           additional parameters to be used by the fun function
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_interp <- function(data.tb, fun = stats::approx, n = base::length, ...){

    # test if data.tb has data
    .sits_test_table(data.tb)

    # compute linear approximation
    result.tb <- sits_apply(data.tb,
                            fun = function(band) {
                                if (class(n) == "function")
                                    return(fun(band, n = n(band), ...)$y)
                                return(fun(band, n = n, ...)$y)
                            },
                            fun_index = function(band) as.Date(fun(band, n = n, ...)$y,
                                                               origin = "1970-01-01"))
    return(result.tb)
}
#' @title Remove missing values
#' @name sits_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@inpe.br}
#' @description  This function removes the missing values from an image time series by substituting them by NA
#' @param data.tb     a valid sits table
#' @param miss_value  a number indicating missing values in a time series.
#' @return result.tb  a sits_table with same samples and the new bands
#' @export
#'
sits_missing_values <-  function(data.tb, miss_value) {

    # test if data.tb has data
    .sits_test_table(data.tb)

    # remove missing values by NAs
    result.tb <- sits_apply(data.tb, fun = function(band) return(ifelse(band == miss_value, NA, band)))
    return (result.tb)
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
#' @param data.tb      The SITS tibble containing the original time series
#' @param lambda       double   - the smoothing factor to be applied (default 1.0)
#' @param differences  an integer indicating the order of differences of contiguous elements (default 3)
#' @param bands_suffix the suffix to be appended to the smoothed filters (default "whit")
#' @return output.tb a tibble with smoothed sits time series
#' @export
sits_whittaker <- function (data.tb, lambda    = 1.0, differences = 3, bands_suffix = "whit") {

    result.tb <- sits_apply(data.tb,
                            fun = function(band){
                                # According to: Whittaker (1923). On a new method of graduation.
                                # Proceedings of the Edinburgh Mathematical Society, 41, 63-73.
                                id.mtx <- diag(length(band))
                                diff.mtx <- diff(id.mtx, lag = 1, differences = differences)

                                # system of equations to be solved for band values
                                smooth.mtx <- id.mtx + (lambda * t(diff.mtx) %*% diff.mtx)

                                # compute solution and return
                                return(solve(smooth.mtx, band))
                            },
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
#' @param data.tb      The SITS tibble containing the original time series
#' @param bands_suffix The suffix to be appended to the smoothed filters
#' @return output.tb   A tibble with smoothed sits time series
#' @export
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
# @param error_in_measurement           A vector of errors in the measuments
# @param initial_estimate               A first estimation of the measurement
# @param initial_error_in_estimate      A first error in the estimation
# @return                               A matrix of 3 columns estimate, error_in_estimate, and kalman_gain
.kalmanfilter <- function(measurement,
                          error_in_measurement = NULL,
                          initial_estimate = NULL,
                          initial_error_in_estimate = NULL){
    kg <- vector(mode = "logical", length = length(measurement) + 1)
    est <- vector(mode = "logical", length = length(measurement) + 1)
    e_est <- vector(mode = "logical", length = length(measurement) + 1)
    #
    # default values
    if(is.null(initial_estimate) || is.na(initial_estimate)){
        initial_estimate <- base::mean(measurement, na.rm = TRUE)
    }
    if(is.null(initial_error_in_estimate) || is.na(initial_error_in_estimate)){
        initial_error_in_estimate <- base::abs(stats::sd(measurement, na.rm = TRUE))
    }
    if(is.null(error_in_measurement)){
        error_in_measurement <- rep(stats::sd(measurement, na.rm = TRUE), length.out = base::length(measurement))
    }
    #
    # Compute the Kalman gain
    # @param e_est    error in estimation
    # @param e_mea    error in measurement
    # @return         the Kalman gain
    .KG <- function(e_est, e_mea){
        return(e_est/(e_est + e_mea))
    }
    # Compute the KF current estimate
    # @param kg        Kalman gain
    # @param est_t1    previous estimate
    # @param mea       measurement
    # @return          current estimate
    .EST_t <- function(kg, est_t1, mea){
        est_t1 + kg * (mea - est_t1)
    }
    # Compute the KF error in the estimation
    # @param kg        Kalman gain
    # @param e_est_t1  previous error in estimation
    .E_EST_t <- function(kg, e_est_t1){
        (1 - kg) * e_est_t1
    }
    # add initial results
    est[1] <- initial_estimate[1]
    e_est[1] <- initial_error_in_estimate[1]
    kg[1] <- NA
    # compute
    for(i in 2:(length(measurement) + 1)){
        kg[i] <- .KG(e_est[i - 1], error_in_measurement[i - 1])
        m <- measurement[i - 1]
        if(is.na(m)){
            m <- est[i - 1]                                                           # if the measurement is missing, use the estimation instead
        }
        est[i] <- .EST_t(kg[i], est[i - 1], m)
        e_est[i] <- .E_EST_t(kg[i], e_est[i - 1])
    }
    # format the results: remove the row before the first measurement (t-1)
    return(
        list(
            estimation = est[2:length(est)],
            error_in_estimate = e_est[2:length(e_est)],
            kalman_gain = kg[2:length(kg)]
        )
    )
}
