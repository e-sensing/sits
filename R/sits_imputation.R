#' @title Linear imputation of NA values
#' @name sits_impute_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Remove NA by linear interpolation
#'
#' @param  data          A time series vector
#' @return               A set of filtered time series
#' @export
sits_impute_linear  <- function(data = NULL) {

    impute_fun <- function(data){
        return(imputeTS::na_interpolation(data))
    }
    result <- .sits_factory_function(data, impute_fun)
    return(result)
}
#' @title Imputation of NA values by Stineman interpolation
#' @name sits_impute_stine
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Remove NA by linear interpolation
#'
#' @param  data          A time series vector
#' @return               A set of filtered time series
#' @export
sits_impute_stine <- function(data = NULL) {

    impute_fun <- function(data){
        return(imputeTS::na_interpolation(data, option = "stine"))
    }
    result <- .sits_factory_function(data, impute_fun)
    return(result)
}
#' @title Imputation of NA values by Stineman interpolation
#' @name sits_impute_kalman
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Remove NA by linear interpolation
#'
#' @param  data          A time series vector
#' @return               A set of filtered time series
#' @export
sits_impute_kalman <- function(data = NULL) {

    impute_fun <- function(data){
        return(imputeTS::na_kalman(data))
    }
    result <- .sits_factory_function(data, impute_fun)
    return(result)
}
