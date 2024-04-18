#' @title  Plot a classified vector cube
#' @name   .plot_class_vector
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified vector cube
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  legend        Legend for the classes
#' @param  palette       A sequential RColorBrewer palette
#' @param  scale         Global scale for plot
#'
#' @return               A plot object
#'
.plot_class_vector  <- function(tile,
                                legend,
                                palette,
                                scale) {
    # set caller to show in errors
    .check_set_caller(".plot_class_vector")
    # retrieve the segments for this tile
    sf_seg <- .segments_read_vec(tile)
    # check that segments have been classified
    .check_that("class" %in% colnames(sf_seg))
    # get the labels
    labels <- sf_seg |>
        sf::st_drop_geometry() |>
        dplyr::select("class") |>
        dplyr::distinct() |>
        dplyr::pull()
    names(labels) <- seq_along(labels)
    # obtain the colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # name the colors to match the labels
    names(colors) <- labels
    # join sf geometries
    sf_seg <- sf_seg |>
        dplyr::group_by(.data[["class"]]) |>
        dplyr::summarise()
    # plot the data using tmap
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_fill(
            col = "class",
            palette = colors
        ) +
        tmap::tm_graticules(
            labels.size = as.numeric(.conf("tmap", "graticules_labels_size"))
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale,
            legend.bg.color = .conf("tmap", "legend_bg_color"),
            legend.bg.alpha = as.numeric(.conf("tmap", "legend_bg_alpha"))
        ) +
        tmap::tm_borders(lwd = 0.2)
    return(p)
}
#' @title  Plot a probs vector cube
#' @name   .plot_probs_vector
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified vector cube
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  labels_plot   Labels to be plotted
#' @param  palette       A sequential RColorBrewer palette
#' @param  style         Method to process the color scale
#'                       ("cont", "order", "quantile", "fisher",
#'                        "jenks", "log10")
#' @param  rev           Revert the color of the palette?
#' @param  scale.        Global map scale
#'
#' @return               A plot object
#'
.plot_probs_vector  <- function(tile,
                                labels_plot,
                                palette,
                                style,
                                rev,
                                scale) {
    # set caller to show in errors
    .check_set_caller(".plot_probs_vector")
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # precondition - check color palette
    .check_palette(palette)
    # revert the palette
    if (rev) {
        palette <- paste0("-", palette)
    }
    # get all labels to be plotted
    labels <- sits_labels(tile)
    names(labels) <- seq_len(length(labels))
    # check the labels to be plotted
    # if NULL, use all labels
    if (.has_not(labels_plot))
        labels_plot <- labels
    .check_that(all(labels_plot %in% labels))

    # get the segments to be plotted
    sf_seg <- .segments_read_vec(tile)

    # plot the segments by facet
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_fill(labels_plot,
                          style = style,
                          palette = palette,
                          midpoint = 0.5,
                          title = labels[labels %in% labels_plot]) +
        tmap::tm_graticules(
            labels.size = as.numeric(.conf("tmap", "graticules_labels_size"))
        ) +
        tmap::tm_facets() +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale,
            legend.bg.color = .conf("tmap", "legend_bg_color"),
            legend.bg.alpha = as.numeric(.conf("tmap", "legend_bg_alpha"))
        ) +
        tmap::tm_borders(lwd = 0.1)

    return(suppressWarnings(p))
}
#' @title  Plot uncertainty vector cube
#' @name   .plot_uncertainty_vector
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots an uncertainty vector cube
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  palette       A sequential RColorBrewer palette
#' @param  style         Method to process the color scale
#'                       ("cont", "order", "quantile", "fisher",
#'                        "jenks", "log10")
#' @param  rev           Revert the color of the palette?
#' @param  scale         Global map scale
#'
#' @return               A plot object
#'
.plot_uncertainty_vector <- function(tile,
                                     palette,
                                     style,
                                     rev,
                                     scale) {
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # precondition - check color palette
    .check_palette(palette)
    # revert the palette
    if (rev) {
        palette <- paste0("-", palette)
    }
    # get the segements to be plotted
    sf_seg <- .segments_read_vec(tile)
    # obtain the uncertainty type
    uncert_type <- .vi(tile)$band
    # plot the segments by facet
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_polygons(uncert_type,
                          palette = palette,
                          style = style) +
        tmap::tm_graticules(
            labels.size = as.numeric(.conf("tmap", "graticules_labels_size"))
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale,
            legend.bg.color = .conf("tmap", "legend_bg_color"),
            legend.bg.alpha = as.numeric(.conf("tmap", "legend_bg_alpha"))
        ) +
        tmap::tm_borders(lwd = 0.2)

    return(suppressWarnings(p))
}
