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

#' @title Find the bounding box for a set of time series
#' @name .sits_bbox_time_series
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a set of time series, find the bounding box.
#'
#' @param data            A tibble with a set of time series
#' @return A vector the bounding box
.sits_bbox_time_series <- function(data){
    # check if the data is a time series
    .sits_test_tibble(data)
    # return the bounding box
    bbox        <- vector(length = 4)
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    bbox["xmin"] <- min(data$longitude)
    bbox["xmax"] <- max(data$longitude)
    bbox["ymin"] <- min(data$latitude)
    bbox["ymax"] <- max(data$latitude)

    return(bbox)
}
