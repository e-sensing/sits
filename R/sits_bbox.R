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

    .check_set_caller("sits_bbox")

    # get the meta-type (sits or cube)
    data <- .config_data_meta_type(data)

    UseMethod("sits_bbox", data)
}

#' @export
#'
sits_bbox.sits <- function(data, ...) {

    # is the data a valid set of time series
    .sits_tibble_test(data)

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
sits_bbox.sits_cube <- function(data, wgs84 = FALSE, ...) {

    # pre-condition
    .cube_check(data)

    if (!wgs84)
        .check_that(length(unique(.crs(data))) == 1,
                    local_msg = "use `wgs84 = TRUE` for a global bbox",
                    msg = "cube has more than one projection")

    # create and return the bounding box
    bbox <- c(xmin = min(data[["xmin"]]),
              xmax = max(data[["xmax"]]),
              ymin = min(data[["ymin"]]),
              ymax = max(data[["ymax"]]))

    # convert to WGS84?
    if (wgs84)
        bbox <- .sits_coords_to_bbox(
            xmin = bbox[["xmin"]],
            xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]],
            ymax = bbox[["ymax"]],
            crs = data[["crs"]][[1]])

    return(bbox)
}

.sits_coords_to_bbox <- function(xmin, xmax, ymin, ymax, crs) {

    pt1 <- c(xmin, ymax)
    pt2 <- c(xmax, ymax)
    pt3 <- c(xmax, ymin)
    pt4 <- c(xmin, ymin)

    bbox <- sf::st_sfc(
        sf::st_polygon(list(rbind(pt1, pt2, pt3, pt4, pt1))), crs = crs
    )

    # create a polygon and transform the proj
    bbox_latlng <- sf::st_bbox(sf::st_transform(bbox, crs = 4326))

    names(bbox_latlng) <- c("lon_min", "lat_min", "lon_max", "lat_max")

    return(bbox_latlng)
}

#' @title Intersection between a bounding box and a cube
#' @name .sits_bbox_intersect
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param bbox           bounding box for a region of interest
#' @param cube           data cube
#' @return               vector the bounding box intersection
#'
.sits_bbox_intersect <- function(bbox, cube) {
    bbox_out <- vector("double", length = 4)
    names(bbox_out) <- c("xmin", "xmax", "ymin", "ymax")

    if (bbox["xmin"] > cube$xmax |
        bbox["xmax"] < cube$xmin |
        bbox["ymin"] > cube$ymax |
        bbox["ymax"] < cube$ymin) {
        return(NULL)
    }

    if (bbox["xmin"] < cube$xmin) {
        bbox_out["xmin"] <- cube$xmin
    } else {
        bbox_out["xmin"] <- bbox["xmin"]
    }

    if (bbox["xmax"] > cube$xmax) {
        bbox_out["xmax"] <- cube$xmax
    } else {
        bbox_out["xmax"] <- bbox["xmax"]
    }

    if (bbox["ymin"] < cube$ymin) {
        bbox_out["ymin"] <- cube$ymin
    } else {
        bbox_out["ymin"] <- bbox["ymin"]
    }

    if (bbox["ymax"] > cube$ymax) {
        bbox_out["ymax"] <- cube$ymax
    } else {
        bbox_out["ymax"] <- bbox["ymax"]
    }

    return(bbox_out)
}
