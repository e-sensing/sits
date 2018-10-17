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
#' @description Transform a latitude and longitude coordinate to a XY projection coordinate
#'
#' @param coverage        Metadata about a coverage.
#' @return A matrix with resolution in WGS84 coordinates.
.sits_convert_resolution <- function(coverage) {
    # create a vector to store the result
    res <- vector(length = 2)
    names(res) <- c("xres", "yres")

    # set the minimum and maximum coordinates
    xy1 <- sf::st_point(c(coverage$xmin, coverage$ymin))
    xy2 <- sf::st_point(c(coverage$xmax, coverage$ymax))

    xymin <- sf::st_sfc(xy1, crs = coverage$crs)
    xymax <- sf::st_sfc(xy2, crs = coverage$crs)

    # get the bounding box in lat/long
    llmin <- sf::st_coordinates(sf::st_transform(xymin, crs = "+init=epsg:4326"))
    llmax <- sf::st_coordinates(sf::st_transform(xymax, crs = "+init=epsg:4326"))

    res["xres"] <- (llmax["x"] - llmin["x"])/coverage$ncols
    res["yres"] <- (llmax["y"] - llmin["y"])/coverage$nrows

    return(res)
}

#' @title Tests if an XY position is inside a ST Raster Brick
#' @name .sits_XY_inside_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function compares an XY position to the extent of a RasterBrick
#'              described by a raster metadata tibble, and return TRUE if the point is
#'              inside the extent of the RasterBrick object.
#'
#' @param xy         XY extent compatible with the R raster package.
#' @param raster.tb  Tibble with metadata information about a raster data set.
#' @return TRUE if XY is inside the raster extent, FALSE otherwise.
.sits_XY_inside_raster <- function(xy, raster.tb){
    if (xy[1,"X"] < raster.tb[1,]$xmin) return(FALSE)
    if (xy[1,"X"] > raster.tb[1,]$xmax) return(FALSE)
    if (xy[1,"Y"] < raster.tb[1,]$ymin) return(FALSE)
    if (xy[1,"Y"] > raster.tb[1,]$ymax) return(FALSE)
    return(TRUE)
}

#' @title Implement a binary search to find the nearest date
#' @name .sits_binary_search
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Performs a binary search in a ordered set of integer values and
#'              returns the values closest to a reference.
#'
#' @param values     A vector of ordered integers.
#' @param val        A reference value.
#' @return The input value closest to the reference one.
.sits_binary_search <- function(values, val){
    if (length(values) == 1) return(values[1])
    if (length(values) == 2) {
        if ((values[1] - val) < (values[2] - val))
            return(values[1])
        else
            return(values[2])
    }
    mid <- as.integer(length(values)/2)
    if (val < values[mid])
        .sits_binary_search(values[1:mid], val)
    else
        .sits_binary_search(values[(mid + 1):length(values)], val)
}
