# ---------------------------------------------------------------
#
#  This file contain a list of time series filters
#  As a rule, filters are functions that apply a 1D function to a
#  time series and produce new values as a result
#
#  Most of the filters provides the generic method sits_apply to apply a
#  1D generic function to a time series and specific methods for
#  common tasks such as missing values removal and smoothing
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
#' @param data.tb       a valid sits table
#' @param fun           a function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     a function with one parameter as input and a Date vector as output.
#' @param bands_suffix  a string informing the resulting bands name's suffix.
#' @return data.tb    a sits_table with same samples and the new bands
#' @export
sits_apply <- function(data.tb, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {

    # veify if data.tb has values
    .sits_test_table(data.tb)

    #get the bands
    bands <- sits_bands (data.tb)

    # computes fun and fun_index for all time series and substitutes the original time series data
    data.tb$time_series <- data.tb$time_series %>%
        purrr::map(function(ts.tb) {
            ts_computed.lst <- dplyr::select(ts.tb, -Index) %>%
                purrr::map(fun)

            # append bands names' suffixes
            if (nchar(bands_suffix) != 0)
                names(ts_computed.lst) <- paste0(bands, ".", bands_suffix)

            # unlist if there are more than one result from `fun`
            if (is.recursive(ts_computed.lst[[1]]))
                ts_computed.lst <- unlist(ts_computed.lst, recursive = FALSE)

            # convert to tibble
            ts_computed.tb <- tibble::as_tibble(ts_computed.lst)

            # compute Index column
            ts_computed.tb <- dplyr::mutate(ts_computed.tb, Index = fun_index(ts.tb$Index))

            # reorganizes time series tibble
            return(dplyr::select(ts_computed.tb, Index, dplyr::everything()))
        })
    return(data.tb)
}

#' @title Add new SITS bands.
#' @name sits_mutate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and preserves existing in a sits_table's time series,
#' using dplyr::mutate function
#' @param data.tb       a valid sits table
#' @param ...           Name-value pairs of expressions. Use NULL to drop a variable.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_mutate <- function(data.tb, ...){
    result.tb <- data.tb

    result.tb$time_series <- result.tb$time_series %>% purrr::map(function(ts.tb) {
        ts_computed.tb <- dplyr::mutate(ts.tb, ...)
        return(ts_computed.tb)
    })

    return(result.tb)
}

#' @title Add new SITS bands and drops existing.
#' @name sits_transmute
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in a sits_table's time series,
#' using dplyr::transmute function
#' @param data.tb       a valid sits table
#' @param ...           Name-value pairs of expressions.
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_transmute <- function(data.tb, ...){
    result.tb <- data.tb

    result.tb$time_series <- result.tb$time_series %>% purrr::map(function(ts.tb) {
        ts_computed.tb <- dplyr::transmute(ts.tb, ...)
        if (!("Index" %in% colnames(ts_computed.tb)))
            ts_computed.tb <- dplyr::bind_cols(dplyr::select(ts.tb, Index), ts_computed.tb)
        return(ts_computed.tb)
    })

    return(result.tb)
}

#' @title Interpolation function of sits_table's time series
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


#' @title Cloud filter
#' @name sits_cloud_filter
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  This function tries to remove clouds in the satellite image time series
#' @param data.tb       a valid sits table containing the "ndvi" band
#' @param cutoff        a numeric value for the maximum acceptable value of a NDVI difference
#' @param order         the order of the ARIMA model to estimate cloud-covered valued
#' @return result.tb    a sits_table with same samples and the new bands
#' @export
sits_cloud_filter <- function(data.tb, cutoff = -0.25, order = 3){

    # find the bands of the data
    bands <- sits_bands (data.tb)
    ensurer::ensure_that(bands, ("ndvi" %in% (.)), err_desc = "data does not contain the ndvi band")

    # predictive model for missing values
    pred_arima <- function (x, order) {
        idx <- which (is.na(x))
        for (i in idx) {
               prev3 <- x[(i - order):(i - 1)]
               ensurer::ensure_that(prev3, !anyNA(.),
                                    err_desc = "Cannot remove clouds, please reduce filter order")
               arima.ml <- stats::arima(prev3, c(0,0,order))
               x[i] <- as.vector(stats::predict (arima.ml, n.ahead = 1)$pred)
        }
        return (x)
    }
    # prepare result SITS table
    result.tb <- data.tb

    # select the chosen bands for the time series
    result.tb$time_series <- data.tb$time_series %>%
        purrr::map (function (ts) {
            ndvi <- dplyr::pull(ts[, "ndvi"])
            idx <- which (c(0, diff(ndvi)) < cutoff)
            idx <- idx[!idx %in% 1:order]
            ts[,bands][idx] <- NA
            # interpolate missing values
            bands %>%
                purrr::map (function (b)
                    ts[,b] <<- pred_arima (dplyr::pull(ts[,b]), order = order))
            return (ts)
        })

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
