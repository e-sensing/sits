#' @title Replace NA values with linear interpolation
#' @name .impute_linear
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Remove NA by linear interpolation
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#'
.impute_linear <- function(data = NULL) {
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
