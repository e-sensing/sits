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
    t <- tibble::tibble(long = longitude, lat = latitude) %>%
        sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
        sf::st_transform(crs = crs) %>%
        sf::st_coordinates() %>%
        tibble::as_tibble()

    colnames(t) <- c("X", "Y")
    return(t)
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
    ll <- tibble::tibble(xc = x, yc = y) %>%
        sf::st_as_sf(coords = c("xc", "yc"), crs = crs) %>%
        sf::st_transform(crs = "EPSG:4326") %>%
        sf::st_coordinates()

    colnames(ll) <- c("longitude", "latitude")
    return(ll)
}

#' @title Transform samples to wgs84
#' @name .proj_transform_samples
#' @keywords internal
#' @noRd
#
#' @description Transforming samples points to wgs84 and replace the longitude
#' and latitude values.
#'
#' @param samples         Samples to be retrieved.
#' @param crs             A coordinate reference system of samples.
#'                        The provided crs could be a character
#'                        (e.g, "EPSG:4326" or "WGS84" or a proj4string), or a
#'                        a numeric with the EPSG code (e.g. 4326).
#'                        This parameter only works for 'csv' or data.frame'
#'                        samples. Default is 4326.
#'
#' @return A tibble with tranformed points.
.proj_transform_samples <- function(samples, crs) {

    .check_chr_contains(
        x = colnames(samples),
        contains = .point_cols,
        msg = "data input is not valid"
    )

    samples <- suppressWarnings(
        sf::st_transform(
            x = sits_as_sf(data = samples, crs = crs),
            crs = 4326
        )
    )
    pts_repr <- tibble::as_tibble(sf::st_coordinates(samples))
    samples[, c("longitude", "latitude")] <- pts_repr[, c("X", "Y")]

    samples <- sf::st_drop_geometry(samples)

    return(samples)
}
#' @title Spatial intersects
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
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
#' x <- .bbox_as_sf(c(xmin=1, xmax=2, ymin=3, ymax=4, crs=4326))
#' y <- .roi_as_sf(c(lon_min=1.5, lon_max=3, lat_min=3.5, lat_max=5))
#' .intersects(x, y) # TRUE
#' }
#'
.intersects <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(sf::st_intersects(x, y, sparse = FALSE), 1, any)
}
