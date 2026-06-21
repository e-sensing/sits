#' @title Replace NA values by linear interpolation
#' @name impute_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Remove NA by linear interpolation
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
impute_linear <- function(data = NULL) {
    impute_fun <- function(data) {
        if (inherits(data, "matrix")) {
            linear_interp(data)
        } else {
            linear_interp_vec(data)
        }
    }
    .factory_function(data, impute_fun)
}

#' @title Remove NA using median
#' @name impute_median
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description Remove NA using median
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
impute_median <- function(data = NULL) {
    # Define impute function
    impute_fun <- function(data) {
        # Matrix
        if (inherits(data, "matrix")) {
            C_interp_median_mat(data)
        }

        # Vector implementation
        else {
            as.vector(
                C_interp_median_vec(data)
            )
        }
    }

    .factory_function(data, impute_fun)
}

#' @title Remove NA using mean
#' @name impute_mean
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description Remove NA using mean
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
impute_mean <- function(data = NULL) {
    # Define impute function
    impute_fun <- function(data) {
        # Matrix
        if (inherits(data, "matrix")) {
            C_interp_mean_mat(data)
        }

        # Vector implementation
        else {
            as.vector(
                C_interp_mean_vec(data)
            )
        }
    }

    .factory_function(data, impute_fun)
}

#' @title Remove NA using weighted moving average
#' @name impute_mean_window
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description Remove NA using weighted moving average
#'
#' @param  data          A time series vector or matrix
#' @param  k             A integer width of the moving average window. Expands
#'                       to both sides of the center element e.g. k = 2 means 4
#'                       observations (2 left, 2 right) are taken into account.
#'                       If all observations in the current window are NA, the
#'                       window size is automatically increased until there are
#'                       at least 2 non-NA values present
#' @param weighting      A string with the weighting strategy to be used. More
#'                       details below (default is "simple").
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @note
#' The \code{weighting} parameter defines the weighting strategy used
#' in the moving window. The strategies available are:
#'
#' \itemize{
#' \item{\code{simple} - Simple Moving Average (SMA) (default option)}
#' \item{\code{linear} - Linear Weighted Moving Average (LWMA)}
#' \item{\code{exponential} - Exponential Weighted Moving Average (EWMA)}
#' }
#'
#' @references
#' The implementation of this function was adapted from the \code{imputeTS} R
#' Package. The code is open-source, under the GPL license, and is available on
#' GitHub \url{https://github.com/SteffenMoritz/imputeTS}.
#'
#' @export
impute_mean_window <- function(data = NULL, k = 2, weighting = "simple") {
    # Check parameters
    .check_int_parameter(k, min = 2)
    .check_chr_within(
        x = weighting,
        within = c("simple", "linear", "exponential")
    )

    # Define impute function
    impute_fun <- function(data) {
        # By default, use the vector implementation
        fnc <- C_interp_mean_window_vec

        # If data is a matrix, use matrix implementation
        if (inherits(data, "matrix")) {
            fnc = C_interp_mean_window_mat
        }

        # Impute!
        fnc(
            data = data,
            k = k,
            weighting = weighting
        )
    }

    .factory_function(data, impute_fun)
}

#' @title Replace NA values in time series with imputation function
#' @name sits_impute
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Remove NA
#'
#' @param  samples        A time series tibble
#' @param  impute_fn     Imputation function
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
sits_impute <- function(samples, impute_fn = impute_linear()) {
    # notify users about the deprecation
    warning(.conf("messages", "sits_impute"))
    # check data is time series
    .check_samples_ts(samples)
    .samples_foreach_ts(samples, function(row) {
        .ts_values(row) <- tibble::as_tibble(
            purrr::map_dfc(.ts_bands(row), function(band) {
                # get band values
                band_value <- as.vector(as.matrix(row[[band]]))
                # impute data
                band_value <- .factory_function(band_value, impute_fn)
                # fix name
                stats::setNames(
                    tibble::tibble(band = band_value), band
                )
            })
        )
        row
    })
}
