#' @title Get the bounding box of the data
#' @name sits_bbox
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Obtain a vector of limits (either on lat/long for time series
#'               or in projection coordinates in the case of cubes)
#'
#' @param data      Valid sits tibble (time series or a cube).
#' @return named vector with bounding box in WGS 84 for time series and
#'         on the cube projection for a data cube.
#'
#' @export
sits_bbox <- function(data) {
    # get the meta-type (sits or cube)
    data <- .sits_config_data_meta_type(data)

    UseMethod("sits_bbox", data)
}
#' @rdname sits_bbox
#' @export
sits_bbox.sits <- function(data) {
    # is the data a valid set of time series
    .sits_test_tibble(data)

    # get the max and min longitudes and latitudes
    lon_max <- max(data$longitude)
    lon_min <- min(data$longitude)
    lat_max <- max(data$latitude)
    lat_min <- min(data$latitude)
    # create and return the bounding box
    bbox <- c(lon_min, lon_max, lat_min, lat_max)
    names(bbox) <- c("lon_min", "lon_max", "lat_min", "lat_max")
    return(bbox)
}
#' @rdname sits_bbox
#' @export
sits_bbox.cube <- function(data) {

    # create and return the bounding box
    if (nrow(data) == 1) {
        bbox <- c(data$xmin, data$xmax, data$ymin, data$ymax)
    } else {
        bbox <- c(min(data$xmin), max(data$xmax),
                  min(data$ymin), max(data$ymax)
        )
    }

    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")
    return(bbox)
}
