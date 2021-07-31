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


#' @title Find the bounding box for a set of time series
#' @name .sits_bbox_time_series
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a set of time series, find the bounding box.
#'
#' @param data            A tibble with a set of time series
#' @return A vector the bounding box
.sits_bbox_time_series <- function(data) {
    # check if the data is a time series
    .sits_tibble_test(data)
    # return the bounding box
    bbox <- vector(length = 4)
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    bbox["xmin"] <- min(data$longitude)
    bbox["xmax"] <- max(data$longitude)
    bbox["ymin"] <- min(data$latitude)
    bbox["ymax"] <- max(data$latitude)

    return(bbox)
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
