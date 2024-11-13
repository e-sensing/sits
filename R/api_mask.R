#' @describeIn mask_api Converts \code{mask} to an \code{sf} object.
#' @returns \code{.roi_as_sf()}: \code{sf}.
#' @noRd
.mask_as_sf <- function(mask) {
    # load sf
    mask <- .roi_as_sf(mask)
    # remove invalid geometries
    mask <- mask[sf::st_is_valid(mask), ]
    # simplify geometries
    mask <- sf::st_simplify(mask, preserveTopology = TRUE)
    # return
    mask
}
