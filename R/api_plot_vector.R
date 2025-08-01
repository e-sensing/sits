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
#' @param  tmap_params   Parameters for tmap control
#' @return               A plot object
#'
.plot_class_vector <- function(tile,
                               legend,
                               palette,
                               scale,
                               tmap_params) {
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

    # plot
    .tmap_vector_class(
        sf_seg = sf_seg,
        colors = colors,
        scale = scale,
        tmap_params = tmap_params
    )
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
#' @param  scale         Global map scale
#' @param  tmap_params   tmap parameters
#'
#' @return               A plot object
#'
.plot_probs_vector <- function(tile,
                               labels_plot,
                               palette,
                               rev,
                               scale,
                               tmap_params) {
    # set caller to show in errors
    .check_set_caller(".plot_probs_vector")
    # precondition - check color palette
    .check_palette(palette)
    # get all labels to be plotted
    labels <- .tile_labels(tile)
    names(labels) <- seq_along(labels)
    # check the labels to be plotted
    # if NULL, use all labels
    if (.has_not(labels_plot)) {
        labels_plot <- labels
    } else {
        .check_that(all(labels_plot %in% labels))
    }
    # get the segments to be plotted
    sf_seg <- .segments_read_vec(tile)

    # plot the segments by facet
    .tmap_vector_probs(
        sf_seg = sf_seg,
        palette = palette,
        rev = rev,
        labels = labels,
        labels_plot = labels_plot,
        scale = scale,
        tmap_params = tmap_params
    )
}
#' @title  Plot uncertainty vector cube
#' @name   .plot_uncertainty_vector
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots an uncertainty vector cube
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Revert the color of the palette?
#' @param  scale         Global map scale
#' @param  tmap_params   tmap parameters
#'
#' @return               A plot object
#'
.plot_uncertainty_vector <- function(tile,
                                     palette,
                                     rev,
                                     scale,
                                     tmap_params) {
    # precondition - check color palette
    .check_palette(palette)
    # get the segments to be plotted
    sf_seg <- .segments_read_vec(tile)
    # obtain the uncertainty type
    uncert_type <- .vi(tile)[["band"]]

    .tmap_vector_uncert(
        sf_seg = sf_seg,
        palette = palette,
        rev = rev,
        type = uncert_type,
        scale = scale,
        tmap_params = tmap_params
    )
}
