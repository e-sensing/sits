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
#' points, following the approach proposed by Meyer and Pebesma(2022).
#'
#' @references
#' Meyer, H., Pebesma, E. "Machine learning-based global maps of
#' ecological variables and the challenge of assessing them",
#' Nature Communications 13, 2208 (2022).
#' https://doi.org/10.1038/s41467-022-29838-9
#'
#' @param samples_tb A `sits` tibble with time series samples.
#' @param roi        A `sf` object (polygon) with a region of interest
#'                   for prediction.
#' @param n          Maximum number of samples to consider.
#'
#' @return           A tibble with sample-to-sample
#'                   and sample-to-prediction distances.
#'
#' @examples
#' if (sits_run_examples()){
#' # read a shapefile for the state of Mato Grosso, Brazil
#' mt_shp <- system.file("extdata/shapefiles/mato_grosso/mt.shp",
#'           package = "sits")
#' # convert to an sf object
#' mt_sf <- sf::read_sf(mt_shp)
#' # calculate sample-to-sample and sample-to-prediction distances
#' distances <- sits_geo_dist(samples_modis_4bands, mt_sf)
#' # plot sample-to-sample and sample-to-prediction distances
#' plot(distances)
#' }
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
    dist_ss <- dplyr::mutate(dist_ss, type = "sample-to-sample")
    dist_sp <- dplyr::mutate(dist_sp, type = "sample-to-prediction")
    dist_tb <- dplyr::bind_rows(dist_ss, dist_sp)
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
    dist_xy <- sf::st_distance(x, y) %>%
        units::drop_units(.)
    dist_xy[dist_xy == 0] <- Inf
    min_dist <- apply(dist_xy, MARGIN = 1, FUN = min)
    dist_df <- tibble::tibble(distance = min_dist)
    return(dist_df)
}
