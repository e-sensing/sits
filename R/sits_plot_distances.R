#' @title Make a kernel density plot of samples distances.
#'
#' @name sits_plot_distances
#'
#' @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Produce a figure showing the kernel density estimates of the distances
#' among the training samples and between the samples and random points in the
#' given region of interest.
#'
#' @param samples_tb A `sits` tibble.
#' @param roi_sf     A `sf` object (polygon).
#'
#' @return           A `ggplot2` object.
#'
#' @export
#'
sits_plot_distances <- function(samples_tb, roi_sf, n) {

    stopifnot(inherits(samples_tb, "sits"))

    test_sf <- sf::st_sample(roi_sf, n)

    samples_sf <- sf::st_as_sf(samples_tb,
                               coords = c("longitude", "latitude"),
                               crs = 4326,
                               remove = FALSE)
    samples_sf <- sf::st_transform(samples_sf,
                                   crs = sf::st_crs(roi_sf))

    dist_ss <- sf::st_distance(samples_sf, samples_sf,
                               which = "Euclidean")
    dist_ss[upper.tri(dist_ss, diag = TRUE)] <- Inf
    dist_ss <- apply(dist_ss, MARGIN = 1, FUN = min)

    dist_st <- sf::st_distance(samples_sf, test_sf)
    dist_st[upper.tri(dist_st, diag = TRUE)] <- Inf
    dist_st <- apply(dist_st, MARGIN = 1, FUN = min)

    density_plot <- ggplot2::ggplot() +
        ggplot2::geom_density(ggplot2::aes(x = dist_ss)) +
        ggplot2::geom_density(ggplot2::aes(x = dist_st)) +
        ggplot2::xlab("Distance") +
        ggplot2::ylab("")

    return(density_plot)
}

