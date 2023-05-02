#' @title Replace NA values with linear interpolation
#' @name sits_impute_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Remove NA by linear interpolation
#'
#' @param  data          A time series vector or matrix
#' @return               A set of filtered time series using
#'                       the imputation function.
#' @examples
#' if (sits_run_examples()) {
#'     # reading a lat/long from a local cube
#'     # create a cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     raster_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     samples <- tibble::tibble(longitude = -55.66738, latitude = -11.76990)
#'     point_ndvi <- sits_get_data(
#'                   cube = raster_cube,
#'                   samples = samples,
#'                   impute_fn = sits_impute_linear())
#'     #
#'     # reading samples from a cube based on a  CSV file
#'     csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
#'         package = "sits"
#'     )
#'     points <- sits_get_data(cube = raster_cube, samples = csv_file)
#' }
#' @export
#'
sits_impute_linear <- function(data = NULL) {
    impute_fun <- function(data) {
        if (inherits(data, "matrix")) {
            return(linear_interp(data))
        } else {
            return(linear_interp_vec(data))
        }
    }

    result <- .sits_factory_function(data, impute_fun)

    return(result)
}
