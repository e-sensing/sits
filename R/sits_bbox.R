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
#' @param data      Valid sits tibble (time series or a cube).
#' @param wgs84     Take effect only for data cubes.
#'                  Reproject bbox to WGS84 (EPSG:4326).
#' @param ...       Additional parameters (not implemented).
#'
#' @return named vector with bounding box in WGS84 for time series and
#'         on the cube projection for a data cube unless wgs84 parameter
#'         is TRUE.
#'
#' @export
#'
sits_bbox <- function(data, wgs84 = FALSE, ...) {

    # get the meta-type (sits or cube)
    data <- .config_memory_bloat(data)

    UseMethod("sits_bbox", data)
}

#' @export
#'
sits_bbox.sits <- function(data, ...) {
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

#' @export
#'
sits_bbox.cube <- function(data, wgs84 = FALSE, ...) {

    # create and return the bounding box
    if (nrow(data) == 1) {
        bbox <- c(data$xmin, data$xmax, data$ymin, data$ymax)
    } else {
        bbox <- c(min(data$xmin), max(data$xmax),
                  min(data$ymin), max(data$ymax)
        )
    }

    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")


    # convert to WGS84?
    if (wgs84) {

        bbox <- c(
            .sits_proj_to_latlong(x = bbox[["xmin"]],
                                  y = bbox[["ymin"]],
                                  crs = data$crs),
            .sits_proj_to_latlong(x = bbox[["xmax"]],
                                  y = bbox[["ymax"]],
                                  crs = data$crs)
        )

        names(bbox) <- c("lon_min", "lon_max", "lat_min", "lat_max")
    }

    return(bbox)
}
