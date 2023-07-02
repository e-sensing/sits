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
#'     sits_bbox(samples_modis_ndvi)
#' }
#' @export
sits_bbox <- function(data, ..., as_crs = NULL) {
    # Get the meta-type (sits or cube)
    data <- .conf_data_meta_type(data)

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
