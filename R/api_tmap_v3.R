#' @export
.tmap_false_color.tmap_v3 <- function(probs_rast,
                                      band,
                                      sf_seg,
                                      seg_color,
                                      line_width,
                                      palette,
                                      rev,
                                      scale,
                                      tmap_params){
    if (rev || palette == "Greys")
        cols4all_name <- paste0("-", palette)

    # generate plot
    p <- tmap::tm_shape(probs_rast) +
        tmap::tm_raster(
            palette = palette,
            title = band,
            midpoint = NA,
            style = "cont",
            style.args = list(na.rm = TRUE)
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            scale = scale
        )
    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }
    return(p)
}
#
#' @export
.tmap_dem_map.tmap_v3 <- function(r, band,
                                  palette, rev,
                                  scale, tmap_params){
    # reverse the color palette?
    if (rev || palette == "Greys")
        cols4all_name <- paste0("-", palette)
    # generate plot
    p <- tmap::tm_shape(r, raster.downsample = FALSE) +
        tmap::tm_raster(
            palette = palette,
            title = band,
            midpoint = NA,
            style = "cont",
            style.args = list(na.rm = TRUE)
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            scale = scale
        )
    return(p)
}
#' @export
.tmap_rgb_color.tmap_v3 <- function(rgb_st,
                                    sf_seg, seg_color, line_width,
                                    scale, tmap_params) {

    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]

    p <- tmap::tm_shape(rgb_st, raster.downsample = FALSE) +
        tmap::tm_raster() +
        tmap::tm_graticules(
            labels.size = labels_size
        ) +
        tmap::tm_layout(
            scale = scale
        ) +
        tmap::tm_compass()

    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }

    return(p)
}
#' @export
#'
.tmap_probs_map.tmap_v3 <- function(probs_rast,
                                    labels,
                                    labels_plot,
                                    palette,
                                    rev,
                                    scale,
                                    tmap_params){
    # reverse the color palette?
    if (rev || palette == "Greys")
        cols4all_name <- paste0("-", palette)

    # select stars bands to be plotted
    bds <- as.numeric(names(labels[labels %in% labels_plot]))

    p <- tmap::tm_shape(probs_rast[[bds]]) +
        tmap::tm_raster(
            style = "cont",
            palette = palette,
            midpoint = NA,
            title = labels[labels %in% labels_plot]
        ) +
        tmap::tm_facets(sync = FALSE) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.show     = TRUE,
            legend.outside  = FALSE,
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            scale = scale
        )
    return(p)
}
#' @export
.tmap_class_map.tmap_v3 <- function(st, colors, scale, tmap_params) {

    # plot using tmap
    p <- tmap::tm_shape(st, raster.downsample = FALSE) +
        tmap::tm_raster(
            style = "cat",
            labels = colors[["label"]],
            palette = colors[["color"]]
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]],
            ndiscr = 50
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        )
    return(p)
}
#' @export
.tmap_vector_probs.tmap_v3 <- function(sf_seg, palette, rev,
                                       labels, labels_plot,
                                       scale, tmap_params){
    if (rev || palette == "Greys")
        cols4all_name <- paste0("-", palette)

    # plot the segments
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_fill(
            labels_plot,
            style = "cont",
            palette = palette,
            midpoint = NA,
            title = labels[labels %in% labels_plot]) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.show     = TRUE,
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.1)

    return(p)
}
#
#' @export
.tmap_vector_class.tmap_v3 <- function(sf_seg,
                                       colors,
                                       scale,
                                       tmap_params){
    # plot the data using tmap
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_fill(
            col = "class",
            palette = colors
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.2)

    return(p)
}
.tmap_vector_uncert.tmap_v3 <- function(sf_seg, palette, rev,
                                        type, scale, tmap_params){
    # revert the palette
    if (rev) {
        palette <- paste0("-", palette)
    }
    # plot
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_fill(
            col = type,
            palette = palette
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.2)

    return(suppressWarnings(p))
}
