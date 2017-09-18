#' @title Coordinate transformation (lat/long to X/Y)
#' @name sits_latlong_to_proj
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Transform a latitude and longitude coordinate to a XY projection coordinate
#'
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param crs             projection definition to be converted to
#' @return xy             a matrix with X/Y coordinates
#' @export
sits_latlong_to_proj <- function (longitude, latitude, crs) {


    st_point <- sf::st_point (c(longitude, latitude))
    lat_long <- sf::st_sfc (st_point, crs = "+init=epsg:4326")

    obj.sf <- sf::st_transform(lat_long, crs = crs)

    xy <- sf::st_coordinates (obj.sf)

    return (xy)

}




