#' @title Tests if an XY position is inside a ST Raster Brick
#' @name .sits_XY_inside_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function compares an XY position to the extent of a RasterBrick
#'              described by a raster metadata tibble, and return TRUE if the point is
#'              inside the extent of the RasterBrick object.
#'
#' @param xy         XY extent compatible with the R raster package
#' @param raster.tb  Tibble with metadata information about a raster data set
#' @return bool      TRUE if XY is inside the raster extent, FALSE otherwise
#'
#' @export
.sits_XY_inside_raster <- function (xy, raster.tb){

    if (xy[1,"X"] < raster.tb[1,]$xmin) return (FALSE)
    if (xy[1,"X"] > raster.tb[1,]$xmax) return (FALSE)
    if (xy[1,"Y"] < raster.tb[1,]$ymin) return (FALSE)
    if (xy[1,"Y"] > raster.tb[1,]$ymax) return (FALSE)
    return (TRUE)
}



#' @title Implement a binary search to find the nearest date
#' @name .sits_binary_search
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Performs a binary search in a ordered set of integer values and
#'              returns the values closest to a reference
#'
#' @param values     a vector of ordered integers
#' @param val        a reference value
#' @return nearest   the input value closest to the reference one
.sits_binary_search <- function (values, val){
    if (length(values) == 1) return (values[1])
    if (length(values) == 2) {
        if ((values[1] - val) < (values[2] - val))
            return (values[1])
        else
            return (values[2])
    }
    mid <- as.integer(length(values)/2)
    if (val < values[mid])
        .sits_binary_search(values[1:mid], val)
    else
        .sits_binary_search(values[mid + 1 : length(values)], val)

}



