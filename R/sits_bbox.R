#' @title Get the bounding box of the data
#'
#' @name sits_bbox
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Obtain a vector of limits (either on lat/long for time series
#'               or in projection coordinates in the case of cubes)
#'
#' @param data   \code{samples} data or \code{cube}.
#' @param crs    CRS of the samples points.
#' @param as_crs CRS to project the resulting \code{bbox}.
#' @param ...    Additional parameters.
#'
#' @return A \code{bbox}.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # get the bbox of a set of samples
#'     sits_bbox(samples_modis_ndvi)
#'     # get the bbox of a cube in WGS84
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     sits_bbox(cube, as_crs = "EPSG:4326")
#' }
#' @export
sits_bbox <- function(data, ..., as_crs = NULL) {
    UseMethod("sits_bbox", data)
}
#' @rdname sits_bbox
#' @export
sits_bbox.sits <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {
    # Pre-conditions
    .check_samples(data)
    # Convert to bbox
    bbox <- .bbox(.point(x = data, crs = crs, as_crs = as_crs))
    return(bbox)
}
#' @rdname sits_bbox
#' @export
sits_bbox.raster_cube <- function(data, ..., as_crs = NULL) {
    # Pre-condition
    .check_is_raster_cube(data)
    # Convert to bbox
    bbox <- .bbox(x = data, as_crs = as_crs)
    return(bbox)
}
#' @rdname sits_bbox
#' @export
sits_bbox.tbl_df <- function(x) {
    if (all(.conf("sits_cube_cols") %in% colnames(x))) {
        class(x) <- c("raster_cube", class(x))
    } else if (all(.conf("sits_tibble_cols") %in% colnames(x))) {
        class(x) <- c("sits", class(x))
    } else
        stop("Input should be a sits tibble or a data cube")
    x <- sits_bbox(x)
    return(x)
}
#' @rdname sits_bbox
#' @export
sits_bbox.default <- function(x, ...){
    x <- tibble::as_tibble(x)
    bbox <- sits_bbox(x)
    return(bbox)
}
