#' @title  Plot a classified vector cube
#' @name   .plot_class_vector
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified vector cube
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  legend        Legend for the classes
#' @param  palette       A sequential RColorBrewer palette
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_class_vector  <- function(tile,
                                legend,
                                palette,
                                tmap_options) {
    # retrieve the segments for this tile
    sf_seg <- .segments_read_vec(tile)
    # check that segments have been classified
    .check_that("class" %in% colnames(sf_seg),
                msg = "segments have not been classified"
    )
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
    # set the tmap options
    tmap_params <- .plot_tmap_params(tmap_options)
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
            labels.size = tmap_params[["labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.show = TRUE,
            legend.outside = FALSE,
            legend.bg.color = tmap_params[["bg_color"]],
            legend.bg.alpha = tmap_params[["bg_alpha"]],
            legend.title.size = tmap_params[["title_size"]],
            legend.text.size = tmap_params[["text_size"]]
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
#' @param  rev           Revert the color of the palette?
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_probs_vector  <- function(tile,
                                labels_plot,
                                palette,
                                rev,
                                tmap_options) {
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
    if (purrr::is_null(labels_plot)) {
        labels_plot <- labels
    } else {
        .check_that(all(labels_plot %in% labels),
                    msg = "labels not in cube"
        )
    }
    # get the segements to be plotted
    sf_seg <- .segments_read_vec(tile)
    # set the tmap options
    tmap_params <- .plot_tmap_params(tmap_options)
    # set the mode to plot
    tmap::tmap_mode("plot")
    # plot the segments by facet
    # fix number of cols
    if (length(labels_plot) < 2 )
        ncol_plot <- 1
    else
        ncol_plot <- 2
    # plot the segments by facet
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_polygons(labels_plot) +
        tmap::tm_facets(sync = FALSE, ncol = 2, scale.factor = 1) +
        tmap::tm_graticules(
            labels.size = tmap_params[["labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.show = TRUE,
            legend.outside = FALSE,
            outer.margins = c(0.00001, 0.00001, 0.00001, 0.00001),
            inner.margins = c(0, 0, 0, 0),
            between.margin = 0,
            asp = 0,
            legend.bg.color = tmap_params[["bg_color"]],
            legend.bg.alpha = tmap_params[["bg_alpha"]],
            legend.title.size = tmap_params[["title_size"]],
            legend.text.size = tmap_params[["text_size"]]
        ) +
        tmap::tm_borders(lwd = 0.2)

    return(suppressWarnings(p))
}
