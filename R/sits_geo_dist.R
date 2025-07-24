#' @title Compute the minimum distances among samples and prediction points.
#'
#' @name sits_geo_dist
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Compute the minimum distances among samples and samples to prediction
#' points, following the approach proposed by Meyer and Pebesma(2022).
#'
#' @note
#' As pointed out by Meyer and Pebesma, many classifications using machine
#' learning assume that the reference data are independent and
#' well-distributed in space. In practice, many traninng samples are strongly
#' concentrated in some areas, and many large areas have no samples.
#' This function compares two distributions:
#' \enumerate{
#' \item{The distribution of the spatial distances of reference data
#' to their nearest neighbor (sample-to-sample.}
#' \item{The distribution of distances from all points of study area
#' to the nearest reference data point (sample-to-prediction).}
#' }
#'
#' @references
#' Meyer, H., Pebesma, E. "Machine learning-based global maps of
#' ecological variables and the challenge of assessing them",
#' Nature Communications 13, 2208 (2022).
#' https://doi.org/10.1038/s41467-022-29838-9
#'
#' @param samples Time series (tibble of class "sits").
#' @param roi     A region of interest (ROI), either a file containing a
#'                shapefile or an "sf" object
#' @param n       Maximum number of samples to consider
#'                (integer)
#' @param crs     CRS of the \code{samples}.
#'
#' @return A tibble with sample-to-sample and sample-to-prediction distances
#'         (object of class "distances").
#'
#' @examples
#' if (sits_run_examples()) {
#'     # read a shapefile for the state of Mato Grosso, Brazil
#'     mt_shp <- system.file("extdata/shapefiles/mato_grosso/mt.shp",
#'         package = "sits"
#'     )
#'     # convert to an sf object
#'     mt_sf <- sf::read_sf(mt_shp)
#'     # calculate sample-to-sample and sample-to-prediction distances
#'     distances <- sits_geo_dist(
#'         samples = samples_modis_ndvi,
#'         roi = mt_sf
#'     )
#'     # plot sample-to-sample and sample-to-prediction distances
#'     plot(distances)
#' }
#' @export
#'
sits_geo_dist <- function(samples, roi, n = 1000L, crs = "EPSG:4326") {
    .check_set_caller("sits_geo_dist")
    # Pre-conditions
    .check_samples(samples)
    samples <- .samples_convert_to_sits(samples)
    if (.has(roi)) {
        roi <- .roi_as_sf(roi = roi, as_crs = "EPSG:4326")
    }
    samples <- samples[sample.int(nrow(samples), min(n, nrow(samples))), ]
    # Convert training samples to points
    samples_sf <- .point_as_sf(
        .point(x = samples, crs = crs),
        as_crs = "EPSG:4326"
    )
    # Get random points from roi
    pred_sf <- sf::st_sample(x = roi, size = n)
    # calculate distances (sample to sample and sample to prediction)
    dist_ss <- .find_closest(samples_sf)
    dist_sp <- .find_closest(samples_sf, pred_sf)
    dist_ss <- dplyr::mutate(dist_ss, type = "sample-to-sample")
    dist_sp <- dplyr::mutate(dist_sp, type = "sample-to-prediction")
    dist_tb <- dplyr::bind_rows(dist_ss, dist_sp)
    .set_class(dist_tb, "geo_distances", class(dist_tb))
}
