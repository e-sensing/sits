#' @export
.tmap_false_color.tmap_v4 <- function(rast,
                                      band,
                                      sf_seg,
                                      seg_color,
                                      line_width,
                                      palette,
                                      rev,
                                      scale,
                                      tmap_params){

    # recover palette name used by cols4all
    cols4all_name <- .colors_cols4all_name(palette)
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")

    p <- tmap::tm_shape(rast) +
        tmap::tm_raster(
            col.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA),
            col.legend = tmap::tm_legend(
                title = band,
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]],
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]],
                position = position,
                frame = TRUE
            )
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        )
    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }

    return(p)
}
#' @export
#'
.tmap_dem_map.tmap_v4 <- function(r, band,
                                  palette, rev,
                                  scale, tmap_params){
    cols4all_name <- .colors_cols4all_name(palette)
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)
    # position
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")
    # generate plot
    p <- tmap::tm_shape(r, raster.downsample = FALSE) +
        tmap::tm_raster(
            col.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA
            ),
            col.legend = tmap::tm_legend(
                title = band,
                position = position,
                frame = TRUE,
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]],
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]]
            )
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        )
    return(p)
}
#' @export
.tmap_rgb_color.tmap_v4 <- function(red_file,
                                    green_file,
                                    blue_file,
                                    scale,
                                    max_value,
                                    first_quantile,
                                    last_quantile,
                                    tmap_params,
                                    sf_seg,
                                    seg_color,
                                    line_width,
                                    sizes)  {

    # open RGB file
    rast <- .raster_open_rast(c(red_file, green_file, blue_file))
    names(rast) <- c("red", "green", "blue")

    p <- tmap::tm_shape(rast, raster.downsample = FALSE) +
        tmap::tm_rgb(
            col = tmap::tm_vars(n = 3, multivariate = TRUE),
            col.scale = tmap::tm_scale_rgb(
                value.na = NA,
                stretch = TRUE,
                probs = c(first_quantile, last_quantile),
                max_color_value = max_value
            )
            ) +
        tmap::tm_graticules(
            labels_size = tmap_params[["graticules_labels_size"]]
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
#
#' @export
#'
.tmap_probs_map.tmap_v4 <- function(probs_rast,
                                    labels,
                                    labels_plot,
                                    palette,
                                    rev,
                                    scale,
                                    tmap_params){

    # recover palette name used by cols4all
    cols4all_name <- .colors_cols4all_name(palette)
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)

    # select stars bands to be plotted
    bds <- as.numeric(names(labels[labels %in% labels_plot]))

    # by default legend position for probs maps is outside
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "inside") {
        cols_free <- TRUE
        position <- tmap::tm_pos_in()
    } else {
        cols_free <- FALSE
        position <- tmap::tm_pos_out(pos.h = "right", pos.v = "top")
    }

    p <- tmap::tm_shape(probs_rast[[bds]]) +
        tmap::tm_raster(
            col.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA),
            col.free = cols_free,
            col.legend = tmap::tm_legend(
                title = tmap_params[["legend_title"]],
                show     = TRUE,
                frame = TRUE,
                position = position,
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]],
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]],
            )
        ) +
        tmap::tm_facets() +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_layout(
            scale = scale
        )
}
#' @export
.tmap_vector_probs.tmap_v4 <- function(sf_seg, palette, rev,
                                       labels, labels_plot,
                                       scale, tmap_params){

    cols4all_name <- .colors_cols4all_name(palette)
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)
    # position
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")

    # plot the segments
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_polygons(
            fill = labels_plot,
            fill.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA),
            fill.legend = tmap::tm_legend(
                frame = TRUE,
                position = position,
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]],
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]]
            )
        ) +
        tmap::tm_facets() +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        )
    return(p)
}
#' @export
.tmap_class_map.tmap_v4 <- function(st, colors, scale, tmap_params) {

    # position
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")

    # plot using tmap
    p <- tmap::tm_shape(st, raster.downsample = FALSE) +
        tmap::tm_raster(
            col.scale = tmap::tm_scale_categorical(
                values = colors[["color"]],
                labels = colors[["label"]]
            ),
            col.legend = tmap::tm_legend(
                position = position,
                frame = TRUE,
                text.size = tmap_params[["legend_text_size"]],
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]]
            )
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]],
            ndiscr = 50
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        )
    return(p)
}
#' @export
.tmap_vector_class.tmap_v4 <- function(sf_seg,
                                       colors,
                                       scale,
                                       tmap_params){

    # position
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")
    # sort the color vector
    colors <- colors[sort(names(colors))]
    # plot the data using tmap
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_polygons(
            fill = "class",
            fill.scale = tmap::tm_scale_categorical(
                values = unname(colors),
                labels = names(colors)
            ),
            fill.legend = tmap::tm_legend(
                frame = TRUE,
                title = "class",
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]],
                position = position,
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]]
            )
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.2)

    return(p)
}
#' @export
.tmap_vector_uncert.tmap_v4 <- function(sf_seg, palette, rev,
                                        type, scale, tmap_params){
    # recover palette name used by cols4all
    cols4all_name <- .colors_cols4all_name(palette)
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)

    # position
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")

    # plot
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_polygons(
            col.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA),
            col.legend = tmap::tm_legend(
                title = type,
                position = position,
                frame = TRUE,
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]],
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]]
            )
        ) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.2)
}
