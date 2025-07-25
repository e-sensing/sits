#' @title Get the bounding box of the data
#'
#' @name sits_bbox
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description  Obtain a vector of limits (either on lat/long for time series
#'               or in projection coordinates in the case of cubes)
#'
#' @param data   samples (class "sits") or \code{cube}.
#' @param ...    parameters for specific types
#' @param crs    CRS of the time series.
#' @param as_crs CRS to project the resulting \code{bbox}.
#'
#' @return A \code{bbox}.
#'
#' @note
#' Time series in \code{sits} are associated with lat/long
#' values in WGS84, while each data cubes is associated to a
#' cartographic projection. To obtain the bounding box
#' of a data cube in a different projection than the original,
#' use the \code{as_crs} parameter.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # get the bbox of a set of samples
#'     sits_bbox(samples_modis_ndvi)
#'     # get the bbox of a cube in WGS84
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     sits_bbox(cube, as_crs = "EPSG:4326")
#' }
#' @export
sits_bbox <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {
    # set caller to show in errors
    .check_set_caller("sits_bbox")
    UseMethod("sits_bbox", data)
}
#' @rdname sits_bbox
#' @export
sits_bbox.sits <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {
    # Pre-conditions
    .check_samples(data)
    # Convert to bbox
    .bbox(.point(x = data, crs = crs, as_crs = as_crs))
}
#' @rdname sits_bbox
#' @export
sits_bbox.raster_cube <- function(data, ..., as_crs = NULL) {
    # Pre-condition
    .check_is_raster_cube(data)
    # Convert to bbox
    .bbox(x = data, as_crs = as_crs)
}
#' @rdname sits_bbox
#' @export
sits_bbox.tbl_df <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_bbox_default"))
    }
    sits_bbox(data, crs, as_crs)
}
#' @rdname sits_bbox
#' @export
sits_bbox.default <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_bbox_default"))
    }
    sits_bbox(data, crs, as_crs)
}
