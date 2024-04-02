#' @title Replace NA values with linear interpolation
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
#' @title Replace NA values with linear interpolation
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
sits_impute <- function(samples, impute_fn = impute_linear() ) {
    # check data is time series
    .check_samples(samples)
    # extract time series
    data <- .ts(samples)
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
