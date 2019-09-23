#' @title Coordinate transformation (lat/long to X/Y)
#' @name .sits_latlong_to_proj
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to a XY projection coordinate
#'
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param crs             Projection definition to be converted to.
#' @return Matrix with (x, y) coordinates.
.sits_latlong_to_proj <- function(longitude, latitude, crs) {
    sf::st_point(c(longitude, latitude)) %>%
        sf::st_sfc(crs = "+init=epsg:4326") %>%
        sf::st_transform(crs = crs) %>%
        sf::st_coordinates()
}

#' @title Coordinate transformation (X/Y to lat/long)
#' @name .sits_proj_to_latlong
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to a XY projection coordinate.
#'
#' @param x               X coordinate of the chosen location.
#' @param y               Y coordinateof the chosen location.
#' @param crs             Projection definition to be converted from.
#' @return Matrix with latlong coordinates.
.sits_proj_to_latlong <- function(x, y, crs) {
    sf::st_point(c(x, y)) %>%
        sf::st_sfc(crs = crs) %>%
        sf::st_transform(crs = "+init=epsg:4326") %>%
        sf::st_coordinates()
}

#' @title Convert resolution from projection values to lat/long
#' @name .sits_convert_resolution
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to a meters.
#'
#' @param cube       Metadata about a data cube.
#' @return A matrix with resolution in the desired crs.
.sits_convert_resolution <- function(cube) {
    # create a vector to store the result
    res <- vector(length = 2)
    names(res) <- c("xres", "yres")

    # If the resolution of the cube is expressed in latlong, convert it to the desired crs
    # This is a hack, that covers cases of cubes with resolutions bigger than 1 meter
    # It assumes that resolutions smaller than 1 are expressed in decimal degress

    if (cube$xres < 1) {
        res["xres"] <- geosphere::distGeo(c(0.0, 0.0), c(0.0, cube$xres))
        res["yres"] <- geosphere::distGeo(c(0.0, 0.0), c(cube$yres,0.0))
    }
    else {
        res["xres"] <- cube$xres
        res["yres"] <- cube$yres
    }
    return(res)
}
