#' @title Compute the minimum distances among samples and prediction points.
#'
#' @name sits_geo_dist
#'
#' @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Compute the minimum distances among samples and samples to prediction
#' points.
#'
#' @param samples_tb A `sits` tibble.
#' @param roi        A `sf` object (polygon).
#' @param n          Maximum number of samples to consider.
#'
#' @return           A tibble.
#'
#' @export
#'
sits_geo_dist <- function(samples, roi = NULL, n = 1000) {

    stopifnot(inherits(samples, "sits"))

    samples <- samples[sample(1:nrow(samples), min(n, nrow(samples))), ]

    # NOTE: sits_tibbles are always in WGS84.
    samples_sf <- sf::st_as_sf(samples,
                               coords = c("longitude", "latitude"),
                               crs = 4326,
                               remove = FALSE)

    pred_sf <- sf::st_sample(roi, n)
    pred_sf <- sf::st_as_sf(pred_sf)
    pred_sf <- sf::st_transform(pred_sf,
                                crs = sf::st_crs(samples_sf))

    dist_ss <- .find_closest(samples_sf)
    dist_sp <- .find_closest(samples_sf, pred_sf)
    dist_ss["type"] <- "sample-to-sample"
    dist_sp["type"] <- "sample-to-prediction"
    dist_tb <- dplyr::as_tibble(dplyr::bind_rows(dist_ss, dist_sp))
    class(dist_tb) <- c("geo_distances", class(dist_tb))

    return(dist_tb)
}



# @title Find the closest points.
#
# @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#
# @description
# For each point in x, find the closest point in y (and their distance).
#
# @param x An `sf` object (points).
# @param y An `sf` object (points).
#
# @return  A data.frame with the columns from (row number in a), b
# (row number in b), and distance (in meters).
#
.find_closest <- function(x, y = x) {
    dist_xy <- sf::st_distance(x, y)
    diag(dist_xy) <- Inf
    rid <- apply(dist_xy, MARGIN = 1, FUN = function(x) which(x == min(x)))
    min_dist <- apply(dist_xy, MARGIN = 1, FUN = min)
    dist_df <- data.frame(from = 1:nrow(x),
                          to   = rid,
                          distance = min_dist)
    return(dist_df)
}
