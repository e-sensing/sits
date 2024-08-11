#' @export
.tmap_false_color.tmap_v4 <- function(st,
                                      band,
                                      sf_seg,
                                      seg_color,
                                      line_width,
                                      palette,
                                      rev,
                                      scale,
                                      tmap_params){

    # recover palette name used by cols4all
    cols4all_name <- cols4all::c4a_info(palette)$fullname
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)

    p <- tmap::tm_shape(st, raster.downsample = FALSE) +
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
                position = tmap::tm_pos_in("left", "bottom"),
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
    cols4all_name <- cols4all::c4a_info(palette)$fullname
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)
    # generate plot
    p <- tmap::tm_shape(r, raster.downsample = FALSE) +
        tmap::tm_raster(
            col.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA
            ),
            col.legend = tmap::tm_legend(
                title = band,
                position = tmap::tm_pos_in("left", "bottom"),
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
.tmap_rgb_color.tmap_v4 <- function(rgb_st,
                                    sf_seg, seg_color, line_width,
                                    scale, tmap_params) {

    p <- tmap::tm_shape(rgb_st, raster.downsample = FALSE) +
        tmap::tm_raster() +
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
.tmap_probs_map.tmap_v4 <- function(probs_st,
                                    labels,
                                    labels_plot,
                                    palette,
                                    rev,
                                    scale,
                                    tmap_params){

    # recover palette name used by cols4all
    cols4all_name <- cols4all::c4a_info(palette)$fullname
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)

    # select stars bands to be plotted
    bds <- as.numeric(names(labels[labels %in% labels_plot]))

    p <- tmap::tm_shape(probs_st[, , , bds]) +
        tmap::tm_raster(
            col.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA),
            col.legend = tmap::tm_legend(
                show     = TRUE,
                frame = TRUE,
                title = "val",
                position = tmap::tm_pos_in("left", "bottom"),
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]],
                bg.color = tmap_params[["legend_bg_color"]],
                bg.alpha = tmap_params[["legend_bg_alpha"]],
            )
        ) +
        tmap::tm_facets(sync = FALSE) +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        )
}
#' @export
.tmap_vector_probs.tmap_v4 <- function(sf_seg, palette, rev,
                                       labels, labels_plot,
                                       scale, tmap_params){

    cols4all_name <- cols4all::c4a_info(palette)$fullname
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)
    # plot the segments
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_polygons(
            fill = labels_plot,
            fill.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA),
            fill.legend = tmap::tm_legend(
                frame = TRUE,
                position = tmap::tm_pos_in("left", "bottom"),
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

    # plot using tmap
    p <- tmap::tm_shape(st, raster.downsample = FALSE) +
        tmap::tm_raster(
            col.scale = tmap::tm_scale_categorical(
                values = colors[["color"]],
                labels = colors[["label"]]
            ),
            col.legend = tmap::tm_legend(
                position = tmap::tm_pos_out(),
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
                position = tmap::tm_pos_out(),
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
    cols4all_name <- cols4all::c4a_info(palette)$fullname
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)
    # plot
    p <- tmap::tm_shape(sf_seg) +
        tmap::tm_polygons(
                col.scale = tmap::tm_scale_continuous(
                              values = cols4all_name,
                              midpoint = NA),
                col.legend = tmap::tm_legend(
                    title = type,
                    position = tmap::tm_pos_in("left", "bottom"),
                    frame = TRUE,
                    bg.color = tmap_params[["legend_bg_color"]],
                    bg.alpha = tmap_params[["legend_bg_alpha"]],
                    title.size = tmap_params[["legend_title_size"]],
                    text.size = tmap_params[["legend_text_size"]]
                )
        ) +
        tmap::tm_graticules(
            tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.2)
}
#' @title  Prepare tmap params for dots value
#' @name .tmap_params_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @noRd
#' @keywords internal
#' @param   dots     params passed on dots
#' @description The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{first_quantile}: 1st quantile for stretching images (default = 0.05)
#' \item \code{last_quantile}: last quantile for stretching images (default = 0.95)
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.8)
#' \item \code{legend_title_size}: relative size of legend title (default = 1.0)
#' \item \code{legend_text_size}: relative size of legend text (default = 1.0)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.5)
#' \item \code{legend_position}: 2D position of legend (default = c("left", "bottom"))
#' }
.tmap_params_set <- function(dots){

    # tmap params
    graticules_labels_size <- as.numeric(.conf("plot", "graticules_labels_size"))
    legend_bg_color <- .conf("plot", "legend_bg_color")
    legend_bg_alpha <- as.numeric(.conf("plot", "legend_bg_alpha"))
    legend_title_size <- as.numeric(.conf("plot", "legend_title_size"))
    legend_text_size <- as.numeric(.conf("plot", "legend_text_size"))
    legend_position <- .conf("plot", "legend_position")

    if ("graticules_labels_size" %in% names(dots))
        graticules_labels_size <- dots[["graticules_labels_size"]]
    if ("legend_bg_color" %in% names(dots))
        legend_bg_color <- dots[["legend_bg_color"]]
    if ("legend_bg_alpha" %in% names(dots))
        legend_bg_alpha <- dots[["legend_bg_alpha"]]
    if ("legend_title_size" %in% names(dots))
        legend_title_size <- dots[["legend_title_size"]]
    if ("legend_text_size" %in% names(dots))
        legend_text_size <- dots[["legend_text_size"]]
    if ("legend_position" %in% names(dots))
        legend_position <- dots[["legend_position"]]

    tmap_params <- list(
        "graticules_labels_size" = graticules_labels_size,
        "legend_bg_color" = legend_bg_color,
        "legend_bg_alpha" = legend_bg_alpha,
        "legend_title_size" = legend_title_size,
        "legend_text_size" = legend_text_size,
        "legend_position" = legend_position
    )
    return(tmap_params)
}

