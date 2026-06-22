#' @title  Plot a probs vector cube
#' @name   .plot_probs_vector
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Plots a probability raster with segment vector overlay.
#'     Overlays segments (as polygon borders without fill) on top of the
#'     probability raster map.
#' @keywords internal
#' @noRd
#' @param  tile             Tile to be plotted.
#' @param  roi              Region of interest
#' @param  labels_plot      Labels to be plotted
#' @param  palette          A sequential RColorBrewer palette
#' @param  rev              Revert the color of the palette?
#' @param  quantile         Minimum quantile to plot
#' @param  scale            Global map scale
#' @param  max_cog_size     Maximum size of COG overviews
#' @param  seg_color        Color for segment borders (default = "black")
#' @param  line_width       Line width for segment borders (default = 0.5)
#' @param  tmap_params      tmap parameters
#'
#' @return                  A plot object
#'
.plot_probs_vector <- function(tile,
                               roi,
                               labels_plot,
                               palette,
                               rev,
                               quantile,
                               scale,
                               max_cog_size,
                               seg_color,
                               line_width,
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
    # retrieve the segments for this tile
    sf_seg <- .segments_read_vec(tile)
    # crop if there is a ROI
    if (.has(roi)) {
        sf_bbox <- sf::st_bbox(.roi_as_sf(roi))
        sf_seg <- sf::st_crop(sf_seg, sf_bbox)
        gc()
    }
    # plot the probability raster as base layer
    p <- .plot_probs(
        tile = tile,
        roi = roi,
        labels_plot = labels_plot,
        palette = palette,
        rev = rev,
        scale = scale,
        quantile = quantile,
        max_cog_size = max_cog_size,
        tmap_params = tmap_params
    )
    # overlay segments without fill (borders only)
    p + .tmap_segments(
        sf_seg = sf_seg,
        seg_color = seg_color,
        line_width = line_width
    )
}
#' @title  Plot uncertainty vector cube
#' @name   .plot_uncertainty_vector
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots an uncertainty vector cube
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  roi           Region of interest
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Revert the color of the palette?
#' @param  scale         Global map scale
#' @param  tmap_params   tmap parameters
#'
#' @return               A plot object
#'
.plot_uncertainty_vector <- function(tile,
                                     roi,
                                     palette,
                                     rev,
                                     scale,
                                     tmap_params) {
    # precondition - check color palette
    .check_palette(palette)
    # obtain the uncertainty type
    uncert_type <- .vi(tile)[["band"]]

    # retrieve the segments for this tile
    sf_seg <- .segments_read_vec(tile)
    if (.has(roi)) {
        sf_bbox <- sf::st_bbox(.roi_as_sf(roi))
        sf_seg <- sf::st_crop(sf_seg, sf_bbox)
        gc()
    }
    # use tmap to plot uncertainty
    .tmap_vector_uncert(
        sf_seg = sf_seg,
        palette = palette,
        rev = rev,
        type = uncert_type,
        scale = scale,
        tmap_params = tmap_params
    )
}
