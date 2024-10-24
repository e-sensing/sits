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
            return(linear_interp(data))
        } else {
            return(linear_interp_vec(data))
        }
    }

    result <- .factory_function(data, impute_fun)

    return(result)
}
#' @title Replace NA values by Kalman Smoothing
#' @name impute_kalman
#' @description Remove NA by Kalman Smoothing
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
impute_kalman <- function(data = NULL) {
    impute_fun <- function(data) {
        is_integer <- is.integer(data)
        data <- imputeTS::na_kalman(data)

        if (is_integer) {
            data <- as.integer(data)
        }

        return(data)
    }

    result <- .factory_function(data, impute_fun)

    return(result)
}
#' @title Replace NA values by Last Observation Carried Forward
#' @name impute_locf
#' @description Remove NA by Last Observation Carried Forward
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
impute_locf <- function(data = NULL) {
    impute_fun <- function(data) {
        is_integer <- is.integer(data)
        data <- imputeTS::na_locf(data)

        if (is_integer) {
            data <- as.integer(data)
        }

        return(data)
    }

    result <- .factory_function(data, impute_fun)

    return(result)
}
#' @title Replace NA values by Weighted Moving Average
#' @name impute_weighted_moving_average
#' @description Remove NA by Weighted Moving Average
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
impute_weighted_moving_average <- function(data = NULL) {
    impute_fun <- function(data) {
        is_integer <- is.integer(data)
        data <- imputeTS::na_ma(data)

        if (is_integer) {
            data <- as.integer(data)
        }

        return(data)
    }

    result <- .factory_function(data, impute_fun)

    return(result)
}
#' @title Replace NA values by Mean Value
#' @name impute_mean
#' @description Remove NA by Mean Value
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
#' @export
impute_mean <- function(data = NULL) {
    impute_fun <- function(data) {
        is_integer <- is.integer(data)
        data <- imputeTS::na_mean(data)

        if (is_integer) {
            data <- as.integer(data)
        }

        return(data)
    }

    result <- .factory_function(data, impute_fun)

    return(result)
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
    # check data is time series
    .check_samples(samples)
    .samples_foreach_ts(samples, function(row) {
        .ts_values(row) <- tibble::as_tibble(
            purrr::map_df(.ts_bands(row), function(band) {
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
        return(row)
    })
}
