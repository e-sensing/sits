#' @title Coordinate transformation (lat/long to X/Y)
#' @name .sits_proj_from_latlong
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to XY coordinate
#'
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param crs          Projection definition to be converted to.
#' @return                Tibble with X and Y coordinates.
.sits_proj_from_latlong <- function(longitude,
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

#' @title Checks if the crs provided is valid
#' @name .sits_proj_format_crs
#' @keywords internal
#'
#' @param crs a \code{numeric} or \code{character} with CRS.
#'
#' @return  a \code{character} with the formatted CRS.
.sits_proj_format_crs <- function(crs) {
    return(sf::st_crs(crs)[["input"]])
}
