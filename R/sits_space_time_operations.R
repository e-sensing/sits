#' @title Coordinate transformation (lat/long to X/Y)
#' @name .sits_latlong_to_proj
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to XY coordinate
#'
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param crs             Projection definition to be converted to.
#' @return                Tibble with X and Y coordinates
.sits_latlong_to_proj <- function(longitude, latitude, crs) {
    t <- tibble::tibble(long = longitude, lat = latitude) %>%
        sf::st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") %>%
        sf::st_transform(crs = crs) %>%
        sf::st_coordinates() %>%
        tibble::as_tibble()

    colnames(t) <- c("X", "Y")
    return(t)
}

#' @title Coordinate transformation (X/Y to lat/long)
#' @name .sits_proj_to_latlong
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a XY coordinate to a latitude and longitude
#'
#' @param x               X coordinate of the chosen location.
#' @param y               Y coordinate of the chosen location.
#' @param crs             Projection definition to be converted from.
#' @return Matrix with latlong coordinates.
.sits_proj_to_latlong <- function(x, y, crs) {
    ll <- tibble::tibble(xc = x, yc = y) %>%
        sf::st_as_sf(coords = c("xc", "yc"), crs = crs) %>%
        sf::st_transform(crs = "EPSG:4326") %>%
        sf::st_coordinates()

    colnames(ll) <- c("longitude", "latitude")
    return(ll)
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
    .sits_test_tibble(data)
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
#' @title Extract a sub_image from a bounding box and a cube
#' @name .sits_sub_image_from_bbox
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param bbox           bounding box for a region of interest
#' @param cube           data cube
#' @return               sub_image with additional info on first row,
#'                       first col, nrows, ncols
#'
.sits_sub_image_from_bbox <- function(bbox, cube) {
    si <- vector("double", length = 8)
    names(si) <- c("first_row", "first_col", "nrows", "ncols",
        "xmin", "xmax", "ymin", "ymax"
    )

    si[c("xmin", "xmax", "ymin", "ymax")] <-
      bbox[c("xmin", "xmax", "ymin", "ymax")]

    if (bbox["xmin"] == cube$xmin) {
          si["first_col"] <- 1
      } else {
          si["first_col"] <- floor((si["xmin"] - cube$xmin) / cube$xres) + 1
      }

    if (bbox["ymax"] == cube$ymax) {
          si["first_row"] <- 1
      } else {
          si["first_row"] <- unname(
            floor((cube$ymax - si["ymax"]) / cube$yres)
            ) + 1
      }

    if (bbox["ymin"] == cube$ymin) {
          si["nrows"] <- cube$nrows - unname(si["first_row"]) + 1
      } else {
          si["nrows"] <- unname(
            floor((si["ymax"] - si["ymin"]) / cube$yres)
            ) + 1
      }

    if (bbox["xmax"] == cube$xmax) {
          si["ncols"] <- cube$ncols - unname(si["first_col"]) + 1
      } else {
          si["ncols"] <- unname(
            floor((si["xmax"] - si["xmin"]) / cube$xres)
            ) + 1
      }

    return(si)
}
