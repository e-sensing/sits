#' @title Coordinate transformation (lat/long to X/Y)
#' @name .proj_from_latlong
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to XY coordinate
#'
#' @param longitude Longitude of the chosen location.
#' @param latitude  Latitude of the chosen location.
#' @param crs       Projection definition to be converted to.
#' @return          Tibble with X and Y coordinates.
.proj_from_latlong <- function(longitude,
                               latitude,
                               crs) {
    t <- tibble::tibble(long = longitude, lat = latitude) |>
        sf::st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
        sf::st_transform(crs = crs) |>
        sf::st_coordinates() |>
        tibble::as_tibble()

    colnames(t) <- c("X", "Y")
    t
}

#' @title Coordinate transformation (X/Y to lat/long)
#' @name .proj_to_latlong
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a XY coordinate to a latitude and longitude
#'
#' @param x   X coordinate of the chosen location.
#' @param y   Y coordinate of the chosen location.
#' @param crs Projection definition to be converted from.
#' @return Matrix with latlong coordinates.
.proj_to_latlong <- function(x, y, crs) {
    ll <- tibble::tibble(xc = x, yc = y) |>
        sf::st_as_sf(coords = c("xc", "yc"), crs = crs) |>
        sf::st_transform(crs = "EPSG:4326") |>
        sf::st_coordinates()

    colnames(ll) <- c("longitude", "latitude")
    ll
}

#' @title Spatial intersects
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' This function is based on sf::st_intersects(). It projects y
#' to the CRS of x before compute intersection. For each geometry of x,
#' returns TRUE if it intersects with any geometry of y,
#' otherwise it returns FALSE.
#'
#' @param x,y sf geometries.
#'
#' @returns A vector indicating which geometries of x
#' intersect geometries of y.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- .bbox_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4326))
#'     y <- .roi_as_sf(c(lon_min = 1.5, lon_max = 3,
#'                       lat_min = 3.5, lat_max = 5))
#'     .intersects(x, y) # TRUE
#' }
#'
.intersects <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(suppressMessages(sf::st_intersects(x, y, sparse = FALSE)), 1L, any)
}
#' @title Spatial within
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' This function is based on sf::st_within(). It projects y
#' to the CRS of x before compute within. For each geometry of x,
#' returns TRUE if it is within any geometry of y,
#' otherwise it returns FALSE.
#'
#' @param x,y sf geometries.
#'
#' @returns A vector indicating which geometries of x
#' is within geometries of y.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- .bbox_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4326))
#'     y <- .roi_as_sf(c(lon_min = 0, lon_max = 3, lat_min = 2, lat_max = 5))
#'     .within(x, y) # TRUE
#' }
#'
.within <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(suppressMessages(sf::st_within(x, y, sparse = FALSE)), 1L, any)
}
#' @title Spatial contains
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' This function is based on sf::st_contains(). It projects y
#' to the CRS of x before compute contains operation. For each geometry of x,
#' returns TRUE if it is contained any geometry of y,
#' otherwise it returns FALSE.
#'
#' @param x,y sf geometries.
#'
#' @returns A vector indicating which geometries of x
#' is contained geometries of y.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- .roi_as_sf(c(lon_min = 0, lon_max = 3, lat_min = 2, lat_max = 5))
#'     y <- .bbox_as_sf(c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4326))
#'     .contains(x, y) # TRUE
#' }
#'
.contains <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(suppressMessages(sf::st_contains(x, y, sparse = FALSE)), 1L, any)
}
#' @title Spatial difference
#' @noRd
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' This function is based on sf::st_difference(). It projects y
#' to the CRS of x before compute difference operation. It returns the
#' difference geometries between x and y.
#'
#' @param x,y sf geometries.
#'
#' @returns A sf object with the difference geometries between x and y.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- .roi_as_sf(c(lon_min = 0, lon_max = 3, lat_min = 2, lat_max = 5))
#'     y <- .roi_as_sf(
#'         c(lon_min = 1, lon_max = 3, lat_min = 2, lat_max = 7, crs = 4326)
#'     )
#'     .difference(x, y)
#' }
#'
.difference <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    suppressMessages(sf::st_difference(x, y))
}
#' @title Find the closest points.
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @keywords internal
#' @noRd
#' @description
#' For each point in x, find the closest point in y (and their distance).
#'
#' @param x An `sf` object (points).
#' @param y An `sf` object (points).
#'
#' @return  A data.frame with the columns from (row number in a), b
#' (row number in b), and distance (in meters).
.find_closest <- function(x, y = x) {
    dist_xy <- sf::st_distance(x, y)
    class(dist_xy) <- setdiff(class(dist_xy), "units")
    attr(dist_xy, "units") <- NULL

    dist_xy[dist_xy == 0.0] <- Inf
    min_dist <- apply(dist_xy, MARGIN = 1L, FUN = min)
    dist_df <- tibble::tibble(distance = min_dist)
    return(dist_df)
}
