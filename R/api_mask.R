#' @describeIn mask_api Converts \code{mask} to an \code{sf} object.
#' @returns \code{.roi_as_sf()}: \code{sf}.
#' @noRd
.mask_as_sf <- function(mask) {
    # is the roi defined by a shapefile
    if (is.character(mask) &&
        file.exists(mask) &&
        (tools::file_ext(mask) == "shp"))
        mask <- sf::st_read(mask)
    # remove invalid geometries
    mask <- mask[sf::st_is_valid(mask), ]
    # simplify geometries
    mask <- sf::st_simplify(mask)
    # return
    mask
}
