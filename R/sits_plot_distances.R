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
sits_plot_distances <- function(samples_tb, roi_sf, n = 1000) {

    stopifnot(inherits(samples_tb, "sits"))

    samples_tb <- samples_tb[sample(1:nrow(samples_tb), n), ]
    pred_sf <- sf::st_sample(roi_sf, n)

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
    dist_ss <- dist_ss %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(type = "sample-to-sample")

    dist_sp <- sf::st_distance(samples_sf, pred_sf)
    dist_sp[upper.tri(dist_sp, diag = TRUE)] <- Inf
    dist_sp <- apply(dist_sp, MARGIN = 1, FUN = min) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(type = "sample-to-prediction")

    plot_tb <-
        dist_ss %>%
        dplyr::bind_rows(dist_sp) %>%
        dplyr::rename(distance = value)

    if (!sf::st_is_longlat(roi_sf))
        plot_tb <-
            plot_tb %>%
            dplyr::mutate(distance = distance/1000)

    density_plot <-
        plot_tb %>%
        ggplot2::ggplot(ggplot2::aes(x = distance)) +
        ggplot2::geom_density(ggplot2::aes(color = type,
                                           fill = type),
                              lwd = 1, alpha = 0.25) +
        ggplot2::scale_x_log10(labels = scales::label_number()) +
        ggplot2::xlab("Distance") +
        ggplot2::ylab("") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::ggtitle("Distribution of Nearest Neighbor Distances")

    if (!sf::st_is_longlat(roi_sf))
        density_plot <-
            density_plot +
            ggplot2::xlab("Distance (km)")

    return(density_plot)
}

