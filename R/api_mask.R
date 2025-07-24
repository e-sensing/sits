#' @title  Convert an exclusion mask to an sf object
#' @name   .mask_as_sf
#' @author Felipe Carvalho, \email{filipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @param  mask    Exclusion mask
#' @returns sf object with simplified geometries
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
