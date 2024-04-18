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
#' @param data   samples (class "sits") or \code{cube}.
#' @param crs    CRS of the samples points (single char)
#' @param as_crs CRS to project the resulting \code{bbox}.
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
sits_bbox <- function(data, crs = "EPSG:4326", as_crs = NULL) {
    # set caller to show in errors
    .check_set_caller("sits_bbox")
    UseMethod("sits_bbox", data)
}
#' @rdname sits_bbox
#' @export
sits_bbox.sits <- function(data, crs = "EPSG:4326", as_crs = NULL) {
    # Pre-conditions
    data <- .check_samples(data)
    # Convert to bbox
    bbox <- .bbox(.point(x = data, crs = crs, as_crs = as_crs))
    return(bbox)
}
#' @rdname sits_bbox
#' @export
sits_bbox.raster_cube <- function(data, crs = "EPSG:4326", as_crs = NULL) {
    # Pre-condition
    .check_is_raster_cube(data)
    # Convert to bbox
    bbox <- .bbox(x = data, as_crs = as_crs)
    return(bbox)
}
#' @rdname sits_bbox
#' @export
sits_bbox.tbl_df <- function(data, crs = "EPSG:4326", as_crs = NULL) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else
        stop(.conf("messages", "sits_bbox_default"))
    bbox <- sits_bbox(data, crs, as_crs)
    return(bbox)
}
#' @rdname sits_bbox
#' @export
sits_bbox.default <- function(data, crs = "EPSG:4326", as_crs = NULL) {
    stop(.conf("messages", "sits_bbox_default"))
}
