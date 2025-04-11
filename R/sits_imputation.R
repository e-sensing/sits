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
    .check_samples_ts(samples)
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
        row
    })
}
