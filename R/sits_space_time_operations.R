#' @title Coordinate transformation (lat/long to X/Y)
#' @name .sits_latlong_to_proj
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to a XY projection coordinate
#'
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param crs             Projection definition to be converted to.
#' @return Matrix with (x, y) coordinates.
.sits_latlong_to_proj <- function(longitude, latitude, crs) {

    t <- tibble::tibble(long = longitude, lat = latitude) %>%
        sf::st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") %>%
        sf::st_transform(crs = crs) %>%
        sf::st_coordinates()
    return(matrix(t, ncol = 2))
}

#' @title Coordinate transformation (X/Y to lat/long)
#' @name .sits_proj_to_latlong
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a XY projection coordinate to a latitude and longitude coordinate.
#'
#' @param x               X coordinate of the chosen location.
#' @param y               Y coordinateof the chosen location.
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

#' @title Coordinate transformation (lat/long to image cell)
#' @name .sits_latlong_to_cell
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to a XY projection coordinate
#'
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param cube            data cube
#' @return                Matrix with (x, y) coordinates.
.sits_latlong_to_cell <- function(longitude, latitude, cube) {

    xy <- tibble::tibble(long = longitude, lat = latitude) %>%
        sf::st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") %>%
        sf::st_transform(crs = cube$crs) %>%
        sf::st_coordinates()

    y <- floor((cube[1,]$ymax - xy[1,"Y"])/cube$yres)
    x <- floor((xy[1,"X"] - cube[1,]$xmin)/cube$xres)

    cell <- c(x,y)

    return(cell)

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
