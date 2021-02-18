#' @title General function for filtering
#' @name sits_filter
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a set of time series, filter them with one of
#' the available filtering algorithms:
#'
#' \itemize{
#'  \item{Whittaker smoother - see \code{\link{sits_whittaker}}}
#'  \item{Savitsky-Golay filter - see \code{\link{sits_sgolay}}}
#'  \item{Envelope filter - see \code{\link{sits_envelope}}}
#'  \item{Interpolation filter - see \code{\link{sits_interp}}}
#'  \item{Kalman filter - see \code{\link{sits_kalman}}}
#' }
#'
#' @param  data          Set of time series
#' @param  filter        Filter to be applied to the data.
#' @return               A set of filtered time series
#'
#' @export
sits_filter <- function(data, filter = sits_whittaker()) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # is the input data a valid sits tibble?
    .sits_test_tibble(data)

    # is the train method a function?
    assertthat::assert_that(class(filter) == "function",
        msg = "sits_filter: filter is not a valid function"
    )

    # compute the training method by the given data
    result <- filter(data)

    # return a valid machine learning method
    return(result)
}
#' @title Envelope filter
#' @name sits_envelope
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function computes the envelope of a time series using the
#' streaming algorithm proposed by Lemire (2009).
#' This functions calls `dtwclust::compute_envelope` function.
#' @param data         A tibble with time series data and metadata.
#' @param operations   A character sequence for the sequence operations.
#'                     ("U" for upper filter, "L" for lower filter).
#' @param bands_suffix Suffix of the resulting data.
#' @return             A tibble with filtered time series values.
#' @examples
#' # Select the NDVI band of a point in Mato Grosso
#' # Apply the envelope filter
#' point_env <- sits_envelope(point_ndvi)
#' # Merge the filtered with the raw data
#' point2 <- sits_merge(point_ndvi, point_env)
#' # Plot the result
#' plot(point2)
#' @export
sits_envelope <- function(data = NULL,
                          operations = "UULL",
                          bands_suffix = "env") {
    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwclust needed for this function to work.
             Please install it.", call. = FALSE)
    }
    # backward compatibility
    data <- .sits_tibble_rename(data)

    filter_fun <- function(data) {
        # definitions of operations
        def_op <- list(
            "U" = "upper", "L" = "lower",
            "u" = "upper", "l" = "lower"
        )

        # split envelope operations
        operations <- strsplit(operations, "")[[1]]

        # verify if operations are either "U" or "L"
        assertthat::assert_that(all(operations %in% names(def_op)),
            msg = "sits_envelope: invalid operation sequence"
        )

        # compute envelopes
        result <- sits_apply(data,
            fun = function(band) {
                for (op in operations) {
                    upper_lower <- dtwclust::compute_envelope(band,
                        window.size = 1,
                        error.check = FALSE
                    )
                    band <- upper_lower[[def_op[[op]]]]
                }
                return(band)
            },
            fun_index = function(band) band,
            bands_suffix = bands_suffix
        )
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Interpolation function of the time series of a sits_tibble
#' @name sits_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands
#'               using the R base function approx.
#' @param data       A tibble with time series data and metadata.
#' @param fun           Interpolation function.
#' @param n             Number of time series elements to be created
#'                      between start date and end date.
#'                      When a class function is passed to `n`,
#'                      it is evaluated with each band time series as
#'                      an argument, e.g. n(band) (default: `length` function).
#' @param ...           Additional parameters to be used by the fun function.
#' @return A tibble with same samples and the new bands.
#' @examples
#' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # find out how many time instances are there
#' n_times <- NROW(sits_time_series(point_ndvi))
#' # interpolate three times more points
#' point_int.tb <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)
#' # plot the result
#' plot(point_int.tb)
#' @export
sits_interp <- function(data = NULL, fun = stats::approx,
                        n = base::length, ...) {
    # backward compatibility
    data <- .sits_tibble_rename(data)
    filter_fun <- function(data) {
        # compute linear approximation
        result <- sits_apply(data,
            fun = function(band) {
                if (class(n) == "function") {
                      return(fun(band, n = n(band), ...)$y)
                  }
                return(fun(band, n = n, ...)$y)
            },
            fun_index = function(band) {
                  as.Date(fun(band, n = n, ...)$y,
                      origin = "1970-01-01"
                  )
              }
        )
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Kalman filter
#'
#' @name sits_kalman
#' @description  A simple Kalman filter implementation.
#'
#' @param data      A sits tibble containing the original time series.
#' @param bands_suffix The suffix to be appended to the smoothed filters.
#' @return A tibble with smoothed sits time series.
#' @examples
#' \dontrun{
#' # Read a set of samples
#' # Select the NDVI band of a point in Mato Grosso
#' point_kf <- sits_kalman(point_ndvi)
#' # Merge the filtered with the raw data
#' point2.tb <- sits_merge(point_ndvi, point_kf)
#' # Plot the result
#' plot(point2.tb)
#' }
#' @export
sits_kalman <- function(data = NULL, bands_suffix = "kf") {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    filter_fun <- function(data) {
        result <- sits_apply(data,
            fun = function(band) {
                .sits_kalman_filter(
                    band,
                    NULL, NULL, NULL
                )
            },
            fun_index = function(band) band,
            bands_suffix = bands_suffix
        )
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}
#' @title Compute the Kalman filter
#' @name  .sits_kalman_filter
#' @keywords internal
#'
#' @param measurement                    A vector of measurements.
#' @param error_in_measurement           A vector of errors in the measurements.
#' @param initial_estimate               A first estimation of the measurement.
#' @param initial_error_in_estimate      A first error in the estimation.
#' @return                               A matrix of 3 columns: estimate,
#'                                       error_in_estimate, and kalman_gain.
.sits_kalman_filter <- function(measurement,
                                error_in_measurement = NULL,
                                initial_estimate = NULL,
                                initial_error_in_estimate = NULL) {
    kg <- vector(mode = "logical", length = length(measurement) + 1)
    est <- vector(mode = "logical", length = length(measurement) + 1)
    e_est <- vector(mode = "logical", length = length(measurement) + 1)
    #
    # default values
    if (purrr::is_null(initial_estimate) || is.na(initial_estimate)) {
        initial_estimate <- base::mean(measurement, na.rm = TRUE)
    }
    if (purrr::is_null(initial_error_in_estimate) ||
        is.na(initial_error_in_estimate)) {
        initial_error_in_estimate <- base::abs(stats::sd(measurement,
            na.rm = TRUE
        ))
    }
    if (purrr::is_null(error_in_measurement)) {
        error_in_measurement <- rep(stats::sd(measurement, na.rm = TRUE),
            length.out = base::length(measurement)
        )
    }
    #
    # Compute the Kalman gain
    # @param e_est    error in estimation
    # @param e_mea    error in measurement
    # @return         the Kalman gain
    .kalman_gain <- function(e_est, e_mea) {
        return(e_est / (e_est + e_mea))
    }
    # Compute the KF current estimate
    # @param kg        Kalman gain
    # @param est_t1    previous estimate
    # @param mea       measurement
    # @return          current estimate
    .estimate_t <- function(kg, est_t1, mea) {
        est_t1 + kg * (mea - est_t1)
    }
    # Compute the KF error in the estimation
    # @param kg        Kalman gain
    # @param e_est_t1  previous error in estimation
    .estimated_error_t <- function(kg, e_est_t1) {
        (1 - kg) * e_est_t1
    }
    # add initial results
    est[1] <- initial_estimate[1]
    e_est[1] <- initial_error_in_estimate[1]
    kg[1] <- NA
    # compute
    for (i in 2:(length(measurement) + 1)) {
        kg[i] <- .kalman_gain(e_est[i - 1], error_in_measurement[i - 1])
        m <- measurement[i - 1]
        if (is.na(m)) {
            m <- est[i - 1] # measurement is missing, use estimation instead
        }
        est[i] <- .estimate_t(kg[i], est[i - 1], m)
        e_est[i] <- .estimated_error_t(kg[i], e_est[i - 1])
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

#' @title Interpolation function of the time series in a sits tibble
#' @name sits_linear_interp
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Computes the linearly interpolated bands for a given resolution
#'               using the R base function approx.
#' @param data       A tibble with time series data and metadata.
#' @param n          Number of time series elements to be created
#'                   between start date and end date.
#' @return           A sits tibble with same samples and the new bands.
#' @examples
#' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # find out how many time instances are there
#' n_times <- NROW(sits_time_series(point_ndvi))
#' # interpolate three times more points
#' point_int.tb <- sits_linear_interp(point_ndvi, n = 3 * n_times)
#' # plot the result
#' plot(point_int.tb)
#' @export
sits_linear_interp <- function(data = NULL, n = 23) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    filter_fun <- function(data) {
        # compute linear approximation
        result <- sits_apply(data,
            fun = function(band) stats::approx(band, n = n, ties = mean)$y,
            fun_index = function(band) {
                  as.Date(stats::approx(band, n = n, ties = mean)$y,
                      origin = "1970-01-01"
                  )
              }
        )
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Remove missing values
#' @name sits_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@inpe.br}
#' @description       This function removes the missing values from
#'                    an image time series by substituting them by NA.
#' @param data        A tibble with time series data and metadata.
#' @param miss_value  Number indicating missing values in a time series.
#' @return            Time series data and metadata (missing values removed).
#' @export
sits_missing_values <- function(data, miss_value) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    # test if data has data
    .sits_test_tibble(data)

    # remove missing values by NAs
    result <- sits_apply(data, fun = function(band) {
          return(ifelse(band == miss_value, NA, band))
      })
    return(result)
}

#' @title NDVI filter with ARIMA model
#' @name sits_ndvi_arima
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description  This function tries to remove clouds in the NDVI band of
#' a time series. It looks for points where the value of the NDVI
#' band goes down abruptly. These points are those whose difference is more
#' than a cutoff value which is set by the user.
#' Then it applies an spline interploation.
#' Finally, the function applies a whitakker smoother.
#'
#' @param data          Time series data and metadata (only the "NDVI" band).
#' @param cutoff        Maximum acceptable value of a NDVI difference.
#' @param p             Order (number of time lags) of the autoregressive model.
#' @param d             Degree of differencing  (the number of times
#'                      the data has had past values subtracted).
#' @param q             Order of the moving-average model.
#' @param bands_suffix  Suffix to rename the filtered bands.
#' @param apply_whit    Apply the whittaker smoother after filtering? (logical)
#' @param lambda_whit   Lambda parameter of the whittaker smoother.
#' @return A sits tibble with same samples and the new bands.
#'
#' @examples
#' # Select the NDVI band of a point
#' # Apply the filter
#' point_ar <- sits_ndvi_arima(point_ndvi)
#' # Merge the filtered with the raw data
#' point2 <- sits_merge(point_ndvi, point_ar)
#' # Plot the result
#' plot(point2)
#' @export
sits_ndvi_arima <- function(data = NULL, cutoff = -0.25,
                            p = 0, d = 0, q = 3,
                            bands_suffix = "ar", apply_whit = TRUE,
                            lambda_whit = 1.0) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    filter_fun <- function(data) {
        # find the bands of the data
        bands <- sits_bands(data)
        assertthat::assert_that("NDVI" %in% bands,
            msg = "data does not contain the NDVI band"
        )

        # predictive model for missing values
        pred_arima <- function(x, p, d, q) {
            idx <- which(is.na(x))
            for (i in idx) {
                prev3 <- x[(i - q):(i - 1)]
                assertthat::assert_that(!anyNA(prev3),
                    msg = "sits_ndvi_arima: please reduce filter order"
                )
                arima_model <- stats::arima(prev3, c(p, d, q))
                x[i] <- as.vector(stats::predict(arima_model, n.ahead = 1)$pred)
            }
            return(x)
        }
        # prepare result sits tibble
        result <- data

        # select the chosen bands for the time series
        result$time_series <- data$time_series %>%
            purrr::map(function(ts) {
                ndvi <- dplyr::pull(ts[, "NDVI"])
                idx <- which(c(0, diff(ndvi)) < cutoff)
                idx <- idx[!idx %in% 1:q]
                ts[, bands][idx, ] <- NA
                # interpolate missing values
                bands %>%
                    purrr::map(function(b) {
                          ts[, b] <<- pred_arima(dplyr::pull(ts[, b]),
                              p = p, d = d, q = q
                          )
                      })
                return(ts)
            })
        # rename the output bands
        new_bands <- paste0(bands, ".", bands_suffix)
        sits_bands(result) <- new_bands

        if (apply_whit) {
              result <- sits_whittaker(result, lambda = lambda_whit)
          }

        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Smooth the time series using Savitsky-Golay filter
#'
#' @name sits_sgolay
#' @description  An optimal polynomial for warping a time series.
#' The degree of smoothing depends on the filter order (usually 3.0).
#' The order of the polynomial uses the parameter `order` (default = 3),
#' the size of the temporal window uses the parameter `length` (default = 5),
#' and the temporal expansion uses the parameter `scaling`.
#'
#' @references A. Savitzky, M. Golay, "Smoothing and Differentiation of Data by
#' Simplified Least Squares Procedures".
#' Analytical Chemistry, 36 (8): 1627–39, 1964.
#'
#' @param data          A tibble with time series data and metadata.
#' @param order         Filter order (integer).
#' @param length        Filter length (must be odd)
#' @param scaling       Time scaling (integer).
#' @param bands_suffix  Suffix to be appended to the smoothed filters.
#' @return              A tibble with smoothed sits time series.
#' @examples
#' #' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the Savitsky Golay smoother
#' point_sg <- sits_filter(point_ndvi, sits_sgolay(order = 3, length = 5))
#' # Plot the two points to see the smoothing effect
#' plot(sits_merge(point_ndvi, point_sg))
#' @export
sits_sgolay <- function(data = NULL, order = 3,
                        length = 5, scaling = 1, bands_suffix = "sg") {
    # verifies if signal package is installed
    if (!requireNamespace("signal", quietly = TRUE)) {
        stop("signal required for this function to work.
             Please install it.", call. = FALSE)
    }
    # backward compatibility
    data <- .sits_tibble_rename(data)

    filter_fun <- function(data) {
        if ("tbl" %in% class(data)) {
            result <- sits_apply(data,
                fun = function(band) {
                      signal::sgolayfilt(band,
                          p = order,
                          n = length, ts = scale
                      )
                  },
                fun_index = function(band) band,
                bands_suffix = bands_suffix
            )
        }
        if ("matrix" %in% class(data)) {
            result <- apply(data, 2, function(row) {
                signal::sgolayfilt(row,
                    p = order,
                    n = length,
                    ts = scale
                )
            })
        }
        return(result)
    }

    result <- .sits_factory_function(data, filter_fun)
    return(result)
}

#' @title Filter the time series using Whittaker smoother
#'
#' @name sits_whittaker
#' @description  The algorithm searches for an optimal warping polynomial.
#' The degree of smoothing depends on smoothing factor lambda
#' (usually from 0.5 to 10.0). Use lambda = 0.5 for very slight smoothing
#' and lambda = 5.0 for strong smoothing.
#'
#' @references Francesco Vuolo, Wai-Tim Ng, Clement Atzberger,
#' "Smoothing and gap-filling of high resolution multi-spectral timeseries:
#' Example of Landsat data",
#' Int Journal of Applied Earth Observation and Geoinformation,
#' vol. 57, pg. 202-213, 2107.
#'
#' @param data         A tibble with time series data and metadata.
#' @param lambda       Smoothing factor to be applied (default 1.0).
#' @param bands_suffix Suffix to be appended (default "wf").
#' @return             A tibble with smoothed sits time series.
#'
#' @examples
#' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the whittaker smoother
#' point_whit <- sits_filter(point_ndvi, sits_whittaker(lambda = 3.0))
#' # Plot the two points to see the smoothing effect
#' plot(sits_merge(point_ndvi, point_whit))
#' @export
sits_whittaker <- function(data = NULL, lambda = 1.0, bands_suffix = "wf") {
    # verifies if ptw package is installed
    if (!requireNamespace("ptw", quietly = TRUE)) {
        stop("ptw required for this function to work.
             Please install it.", call. = FALSE)
    }
    # backward compatibility
    data <- .sits_tibble_rename(data)

    filter_fun <- function(data) {
        result <- NULL
        if ("tbl" %in% class(data)) {
            result <- sits_apply(data,
                fun = function(band) {
                    ptw::whit2(band, lambda = lambda)
                },
                fun_index = function(band) band,
                bands_suffix = bands_suffix
            )
        }
        if ("matrix" %in% class(data)) {
            result <- apply(
                data, 2,
                function(row) {
                    ptw::whit2(row, lambda = lambda)
                }
            )
        }
        return(result)
    }
    result <- .sits_factory_function(data, filter_fun)
    return(result)
}
