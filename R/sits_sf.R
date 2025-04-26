#' @title Return a sits_tibble or raster_cube as an sf object.
#' @name sits_as_sf
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Converts a sits_tibble or raster_cube as an sf object.
#'
#' @param data   A sits tibble or sits cube.
#' @param as_crs Output coordinate reference system.
#' @param ...    Additional parameters.
#' @param crs    Input coordinate reference system.
#' @return       An sf object of point or polygon geometry.
#' @examples
#' if (sits_run_examples()) {
#'     # convert sits tibble to an sf object (point)
#'     sf_object <- sits_as_sf(cerrado_2classes)
#'
#'     # convert sits cube to an sf object (polygon)
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     sf_object <- sits_as_sf(cube)
#' }
#' @export
sits_as_sf <- function(data, ...) {
    .check_set_caller("sits_as_sf")
    UseMethod("sits_as_sf", data)
}

#' @export
#' @rdname sits_as_sf
sits_as_sf.sits <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {
    # Pre-conditions
    .check_samples(data)
    data <- .samples_convert_to_sits(data)
    # Convert samples to sf
    geom <- .point_as_sf(.point(data, crs = crs), as_crs = as_crs)
    # Bind columns
    data <- dplyr::bind_cols(geom, data)
    return(data)
}

#' @export
#' @rdname sits_as_sf
sits_as_sf.raster_cube <- function(data, ..., as_crs = NULL) {
    # Pre-conditions
    .check_is_raster_cube(data)
    # Convert cube bbox to sf
    data_sf <- .cube_as_sf(data, as_crs = as_crs)
    dplyr::bind_cols(data_sf, .discard(data, "file_info"))
}

#' @export
#' @rdname sits_as_sf
sits_as_sf.vector_cube <- function(data, ..., as_crs = NULL) {
    # Pre-conditions
    .check_is_raster_cube(data)
    # Convert cube bbox to sf
    data_sf <- .cube_as_sf(data, as_crs = as_crs)
    # Bind columns
    dplyr::bind_cols(data_sf, .discard(data, c("file_info", "vector_info")))
}
#' @export
#' @rdname sits_as_sf
#' @export
sits_as_sf.default <- function(data, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_select"))
    }
    sits_as_sf(data, ...)
}
