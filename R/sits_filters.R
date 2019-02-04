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

#' @title cloud filter
#' @name sits_cloud_filter
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  This function tries to remove clouds in the ndvi band of
#' a satellite image time series. It looks for points where the value of the NDVI
#' band goes down abruptly. These points are taken as those whose difference is more
#' than a cutoff value which is set by the user. Then it applies an spline interploation.
#' Finally, the function applies a whitakker smoother.
#'
#' @param data.tb       A tibble with time series data and metadata with only the "ndvi" band.
#' @param cutoff        A numeric value for the maximum acceptable value of a NDVI difference.
#' @param bands_suffix  Suffix to rename the filtered bands.
#' @param apply_whit    Apply the whittaker smoother after filtering? The default value is FALSE.
#' @param lambda_whit   Lambda parameter of the whittaker smoother.
#' @return A sits tibble with same samples and the new bands.
#'
#' @examples
#' \donttest{
#' # Read a set of samples of forest/non-forest in Amazonia
#' # This is an area full of clouds
#' data(prodes_226_064)
#' # Select the NDVI band of the first point
#' point_ndvi.tb <- sits_select_bands(prodes_226_064[1,], ndvi)
#' # Apply the cloud filter
#' point_cld.tb <- sits_cloud_filter(point_ndvi.tb)
#' # Merge the filtered with the raw data
#' point2.tb <- sits_merge (point_ndvi.tb, point_cld.tb)
#' # Plot the result
#' sits_plot (point2.tb)
#' }
#' @export
sits_cloud_filter <- function(data.tb = NULL, cutoff = 0.25,
                              bands_suffix = "cf", apply_whit = FALSE, lambda_whit = 1.0){
    filter_fun <- function(data.tb) {
        # find the bands of the data
        bands <- sits_bands(data.tb)
        ensurer::ensure_that(bands, ("ndvi" %in% (.)), err_desc = "data does not contain the ndvi band")

        # prepare result sits tibble
        result.tb <- data.tb
        env.tb <- sits_select_bands_(data.tb, "ndvi") %>% sits_envelope(operations = "UU")

        # select the chosen bands for the time series
        result.tb$time_series <- purrr::pmap(list(data.tb$time_series, env.tb$time_series, result.tb$time_series),
                    function (ts_data, ts_env, ts_res) {
                        ndvi <- dplyr::pull(ts_data[, "ndvi"])
                        env  <- dplyr::pull(ts_env[, "ndvi.env"])
                        idx <- which(abs(env - ndvi) > cutoff)

                        # interpolate missing values
                        bands %>%
                            purrr::map(function(b) {
                                    ts <- dplyr::pull(ts_res[, b])
                                    ts[idx] <- NA
                                    ts_res[,b] <<- imputeTS::na.interpolation(ts, option = "spline")
                                })
                        return(ts_res)
                    })
        # rename the output bands
        new_bands <- paste0(bands, ".", bands_suffix)
        result.tb <- sits_rename(result.tb, new_bands)

        if (apply_whit)
            result.tb <- sits_whittaker(result.tb, lambda = lambda_whit)

        return(result.tb)

    }
    result <- .sits_factory_function(data.tb, filter_fun)
}

#' @title Envelope filter
#' @name sits_envelope
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function computes the envelope of a time series using the
#' streaming algorithm proposed by Lemire (2009). This functions calls `dtwclust::compute_envelope` function.
#' @param data.tb      A tibble with time series data and metadata.
#' @param operations   A character sequence indicating which operations must be taken. "U" for upper filter, "L" for down filter.
#' @param bands_suffix Suffix to be appended to the resulting data (default "env").
#' @return A tibble with filtered time series values.
#' @examples
#' \donttest{
#' # Read a set of samples of forest/non-forest in Amazonia
#' # This is an area full of clouds
#' data(prodes_226_064)
#' # Select the NDVI band of the first point
#' point_ndvi.tb <- sits_select_bands(prodes_226_064[1,], ndvi)
#' # Apply the envelope filter
#' point_env.tb <- sits_envelope(point_ndvi.tb)
#' # Merge the filtered with the raw data
#' point2.tb <- sits_merge (point_ndvi.tb, point_env.tb)
#' # Plot the result
#' sits_plot (point2.tb)
#' }
#' @export
sits_envelope <- function(data.tb = NULL, operations = "UULL", bands_suffix = "env"){
    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwclust needed for this function to work. Please install it.", call. = FALSE)
    }

    filter_fun <- function(data.tb) {
        # definitions of operations and the key returned by `dtwclust::compute_envelope`
        def_op <- list("U" = "upper", "L" = "lower", "u" = "upper", "l" = "lower")

        # split envelope operations
        operations <- strsplit(operations, "")[[1]]

        # verify if operations are either "U" or "L"
        ensurer::ensure_that(operations, all(. %in% names(def_op)),
                             err_desc = "sits_envelope: invalid operation sequence")

        # compute envelopes
        result.tb <- sits_apply(data.tb,
                                fun = function(band) {
                                    for (op in operations) {
                                        upper_lower.lst <- dtwclust::compute_envelope(band, window.size = 1, error.check = FALSE)
                                        band <- upper_lower.lst[[def_op[[op]]]]
                                    }
                                    return(band)
                                },
                                fun_index = function(band) band,
                                bands_suffix = bands_suffix)
        return(result.tb)
    }
    result <- .sits_factory_function(data.tb, filter_fun)
}

#' @title Interpolation function of the time series of a sits_tibble
#' @name sits_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands for a given resolution
#'               using the R base function approx.
#' @param data.tb       A tibble with time series data and metadata.
#' @param fun           Interpolation function.
#' @param n             Number of time series elements to be created between start date and end date.
#'                      When a class function is passed to `n`, is is evaluated with each band time series as
#'                      an argument, e.g. n(band) (default: `length` function).
#' @param ...           Additional parameters to be used by the fun function.
#' @return A tibble with same samples and the new bands.
#' @examples
#' \donttest{
#' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # find out how many time instances are there
#' n_times <- NROW(point_ndvi$time_series[[1]])
#' # interpolate three times more points
#' point_int.tb <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)
#' # plot the result
#' sits_plot(point_int.tb)
#' }
#' @export
sits_interp <- function(data.tb = NULL, fun = stats::approx, n = base::length, ...) {
    filter_fun <- function(data.tb) {
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
    result <- .sits_factory_function(data.tb, filter_fun)
}

#' @title Kalman filter
#'
#' @name sits_kalman
#' @description  A simple Kalman filter implementation.
#'
#' @param data.tb      A sits tibble containing the original time series.
#' @param bands_suffix The suffix to be appended to the smoothed filters.
#' @return A tibble with smoothed sits time series.
#' @export
sits_kalman <- function(data.tb = NULL, bands_suffix = "kf"){
    filter_fun <- function(data.tb) {
        result.tb <- sits_apply(data.tb,
                                fun = function(band) .sits_kalman_filter(band, NULL, NULL, NULL),
                                fun_index = function(band) band,
                                bands_suffix = bands_suffix)
        return(result.tb)
    }
    result <- .sits_factory_function(data.tb, filter_fun)
    return(result)
}

#' @title Interpolation function of the time series in a sits tibble
#' @name sits_linear_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands for a given resolution
#'               using the R base function approx.
#' @param data.tb       A tibble with time series data and metadata.
#' @param n             Number of time series elements to be created between start date and end date.
#' @return A sits tibble with same samples and the new bands.
#' @examples
#' \donttest{
#' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # find out how many time instances are there
#' n_times <- NROW (point_ndvi$time_series[[1]])
#' # interpolate three times more points
#' point_int.tb <- sits_linear_interp(point_ndvi, n = 3*n_times)
#' # plot the result
#' sits_plot (point_int.tb)
#' }
#' @export
sits_linear_interp <- function(data.tb = NULL, n = 23) {
    filter_fun <- function(data.tb){
        # compute linear approximation
        result.tb <- sits_apply(data.tb,
                                fun = function(band) stats::approx(band, n = n, ties = mean)$y,
                                fun_index = function(band) as.Date(stats::approx(band, n = n, ties = mean)$y,
                                                                   origin = "1970-01-01"))
        return(result.tb)
    }
    result <- .sits_factory_function(data.tb, filter_fun)
}

#' @title Remove missing values
#' @name sits_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@inpe.br}
#' @description  This function removes the missing values from an image time series by substituting them by NA.
#' @param data.tb     A tibble with time series data and metadata.
#' @param miss_value  Number indicating missing values in a time series.
#' @return A tibble with time series data and metadata (with missing values removed).
#' @export
sits_missing_values <-  function(data.tb, miss_value) {
    # test if data.tb has data
    .sits_test_tibble(data.tb)

    # remove missing values by NAs
    result.tb <- sits_apply(data.tb, fun = function(band) return(ifelse(band == miss_value, NA, band)))
    return(result.tb)
}

#' @title NDVI filter with ARIMA model
#' @name sits_ndvi_arima_filter
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  This function tries to remove clouds in the ndvi band of
#' a satellite image time series. It looks for points where the value of the NDVI
#' band goes down abruptly. These points are taken as those whose difference is more
#' than a cutoff value which is set by the user. Then it applies an spline interploation.
#' Finally, the function applies a whitakker smoother.
#'
#' @param data.tb       A tibble with time series data and metadata with only the "ndvi" band.
#' @param cutoff        Numeric value for the maximum acceptable value of a NDVI difference.
#' @param p             Order (number of time lags) of the autoregressive model.
#' @param d             Degree of differencing (the number of times the data have had past values subtracted).
#' @param q             Order of the moving-average model.
#' @param bands_suffix  Suffix to rename the filtered bands.
#' @param apply_whit    Apply the whittaker smoother after filtering? (logical)
#' @param lambda_whit   Lambda parameter of the whittaker smoother.
#' @return A sits tibble with same samples and the new bands.
#'
#' @examples
#' \donttest{
#' # Read a set of samples of forest/non-forest in Amazonia
#' # This is an area full of clouds
#' data(prodes_226_064)
#' # Select the NDVI band of the first point
#' point_ndvi.tb <- sits_select_bands(prodes_226_064[1,], ndvi)
#' # Apply the cloud filter
#' point_cld.tb <- sits_ndvi_arima_filter(point_ndvi.tb)
#' # Merge the filtered with the raw data
#' point2.tb <- sits_merge (point_ndvi.tb, point_cld.tb)
#' # Plot the result
#' sits_plot (point2.tb)
#' }
#' @export
sits_ndvi_arima_filter <- function(data.tb = NULL, cutoff = -0.25, p = 0, d = 0, q = 3,
                              bands_suffix = "ar", apply_whit = TRUE, lambda_whit = 1.0){
    filter_fun <- function(data.tb) {
        # find the bands of the data
        bands <- sits_bands(data.tb)
        ensurer::ensure_that(bands, ("ndvi" %in% (.)), err_desc = "data does not contain the ndvi band")

        # predictive model for missing values
        pred_arima <- function(x, p, d, q) {
            idx <- which(is.na(x))
            for (i in idx) {
                prev3 <- x[(i - q):(i - 1)]
                ensurer::ensure_that(prev3, !anyNA(.),
                                     err_desc = "Cannot remove clouds, please reduce filter order")
                arima.ml <- stats::arima(prev3, c(p, d , q))
                x[i] <- as.vector(stats::predict(arima.ml, n.ahead = 1)$pred)
            }
            return(x)
        }
        # prepare result sits tibble
        result.tb <- data.tb
        env.tb <- sits_envelope(data.tb, operations = "U")

        # select the chosen bands for the time series
        result.tb$time_series <- data.tb$time_series %>%
            purrr::map(function(ts) {
                ndvi <- dplyr::pull(ts[, "ndvi"])
                idx <- which(c(0, diff(ndvi)) < cutoff)
                idx <- idx[!idx %in% 1:q]
                ts[,bands][idx,] <- NA
                # interpolate missing values
                bands %>%
                    purrr::map(function(b)
                        ts[,b] <<- pred_arima(dplyr::pull(ts[,b]), p = p, d = d, q = q))
                return(ts)
            })
        # rename the output bands
        new_bands <- paste0(bands, ".", bands_suffix)
        result.tb <- sits_rename(result.tb, new_bands)

        if (apply_whit)
            result.tb <- sits_whittaker(result.tb, lambda = lambda_whit)

        return(result.tb)

    }
    result <- .sits_factory_function(data.tb, filter_fun)
}

#' @title Smooth the time series using Whittaker smoother
#'
#' @name sits_whittaker
#' @description  The algorithm searches for an optimal polynomial describing the warping.
#' Some authors consider the Whittaker smoother to be a good method for smoothing and
#' gap filling for satellite image time series
#' The degree of smoothing depends on smoothing factor lambda (usually from 0.5 to 10.0)
#' Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong smoothing
#'
#' @references Francesco Vuolo, Wai-Tim Ng, Clement Atzberger,
#' "Smoothing and gap-filling of high resolution multi-spectral timeseries:
#' Example of Landsat data", Int Journal of Applied Earth Observation and Geoinformation,
#' vol. 57, pg. 202-213, 2107.
#'
#' @param data.tb      A tibble with time series data and metadata.
#' @param lambda       Smoothing factor to be applied (default 1.0).
#' @param differences  The order of differences of contiguous elements (default 3).
#' @param bands_suffix Suffix to be appended to the smoothed filters (default "whit").
#' @return A tibble with smoothed sits time series.
#'
#' @examples
#' \donttest{
#' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the whittaker smoother
#' point_ws.tb <- sits_whittaker (point_ndvi, lambda = 3.0)
#' # Plot the two points to see the smoothing effect
#' sits_plot(sits_merge(point_ndvi, point_ws.tb))
#' }
#' @export
sits_whittaker <- function(data.tb = NULL, lambda    = 1.0, differences = 3, bands_suffix = "whit") {
    filter_fun <- function(data.tb) {
        result.tb <- sits_apply(data.tb,
                                fun = function(band){
                                    # According to: Whittaker (1923). On a new method of graduation.
                                    # Proceedings of the Edinburgh Mathematical Society, 41, 63-73.
                                    id.mtx <- diag(length(band))
                                    diff.mtx <- diff(id.mtx, lag = 1, differences = differences)

                                    # system of equations to be solved for band values
                                    smooth.mtx <- id.mtx + (lambda * crossprod(diff.mtx))

                                    # compute solution and return
                                    return(solve(smooth.mtx, band))
                                },
                                fun_index = function(band) band,
                                bands_suffix = bands_suffix)
        return(result.tb)
    }
    result <- .sits_factory_function(data.tb, filter_fun)
}

#' @title Smooth the time series using Savitsky-Golay filter
#'
#' @name sits_sgolay
#' @description  The algorithm searches for an optimal polynomial describing the warping.
#' The degree of smoothing depends on the filter order (usually 3.0).
#' Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong smoothing.
#'
#' @param data.tb       A tibble with time series data and metadata.
#' @param order         Filter order (integer).
#' @param scale         Time scaling (integer).
#' @param bands_suffix  Suffix to be appended to the smoothed filters.
#' @return A tibble with smoothed sits time series.
#' @examples
#' \donttest{
#' #' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the Savitsky Golay smoother
#' point_sg.tb <- sits_sgolay (point_ndvi, order = 3, scale = 2)
#' # Plot the two points to see the smoothing effect
#' sits_plot(sits_merge(point_ndvi, point_sg.tb))
#' }
#' @export
sits_sgolay <- function(data.tb = NULL, order = 3, scale = 1, bands_suffix = "sg") {
    filter_fun <- function(data.tb) {
        result.tb <- sits_apply(data.tb,
                                fun = function(band) signal::sgolayfilt(band, p = order, ts = scale),
                                fun_index = function(band) band,
                                bands_suffix = bands_suffix)
        return(result.tb)
    }

    result <- .sits_factory_function(data.tb, filter_fun)
}

#' Compute the Kalman filter
#'
#' @param measurement                    A vector of measurements.
#' @param error_in_measurement           A vector of errors in the measuments.
#' @param initial_estimate               A first estimation of the measurement.
#' @param initial_error_in_estimate      A first error in the estimation.
#' @return                               A matrix of 3 columns estimate, error_in_estimate, and kalman_gain.
.sits_kalman_filter <- function(measurement,
                          error_in_measurement = NULL,
                          initial_estimate = NULL,
                          initial_error_in_estimate = NULL){
    kg <- vector(mode = "logical", length = length(measurement) + 1)
    est <- vector(mode = "logical", length = length(measurement) + 1)
    e_est <- vector(mode = "logical", length = length(measurement) + 1)
    #
    # default values
    if (is.null(initial_estimate) || is.na(initial_estimate)) {
        initial_estimate <- base::mean(measurement, na.rm = TRUE)
    }
    if (is.null(initial_error_in_estimate) || is.na(initial_error_in_estimate)) {
        initial_error_in_estimate <- base::abs(stats::sd(measurement, na.rm = TRUE))
    }
    if (is.null(error_in_measurement)) {
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
    for (i in 2:(length(measurement) + 1)) {
        kg[i] <- .KG(e_est[i - 1], error_in_measurement[i - 1])
        m <- measurement[i - 1]
        if (is.na(m)) {
            m <- est[i - 1] # if the measurement is missing, use the estimation instead
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
