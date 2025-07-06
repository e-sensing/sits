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
