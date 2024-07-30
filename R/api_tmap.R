#' @title  Plot a false color image with tmap
#' @name   .tmap_false_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a set of false color image
#' @keywords internal
#' @noRd
#' @param  st            stars object.
#' @param  band          Band to be plotted.
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  palette       A sequential RColorBrewer palette
#' @param  main_title    Main title for the plot
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.tmap_false_color <- function(st,
                              band,
                              sf_seg,
                              seg_color,
                              line_width,
                              main_title,
                              palette,
                              rev,
                              scale,
                              tmap_params){

    if (as.numeric_version(packageVersion("tmap")) < "3.9")
        class(st) <- "tmap_v3"
    else
        class(st) <- "tmap_v4"
    UseMethod(".tmap_false_color", st)
}
#' @export
.tmap_false_color.tmap_v3 <- function(st,
                                      band,
                                      sf_seg,
                                      seg_color,
                                      line_width,
                                      main_title,
                                      palette,
                                      rev,
                                      scale,
                                      tmap_params){

    # reverse the color palette?
    if (rev || palette == "Greys")
        palette <- paste0("-", palette)

    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]
    legend_width <- tmap_params[["legend_width"]]
    legend_height <- tmap_params[["legend_height"]]
    legend_position <- tmap_params[["legend_position"]]

    # generate plot
    p <- tmap::tm_shape(st, raster.downsample = FALSE) +
        tmap::tm_raster(
            palette = palette,
            title = band,
            midpoint = NA,
            style = "cont",
            style.args = list(na.rm = TRUE)
        ) +
        tmap::tm_graticules(
            labels.size = labels_size
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            main.title = main_title,
            main.title.size = 1,
            main.title.position = "center",
            legend.bg.color = legend_bg_color,
            legend.bg.alpha = legend_bg_alpha,
            legend.title.size = legend_title_size,
            legend.text.size = legend_text_size,
            legend.height = legend_height,
            legend.width  = legend_width,
            legend.position = legend_position,
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
.tmap_false_color.tmap_v4 <- function(st,
                                      band,
                                      sf_seg,
                                      seg_color,
                                      line_width,
                                      main_title,
                                      palette,
                                      rev,
                                      scale,
                                      tmap_params){
    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]
    legend_width <- tmap_params[["legend_width"]]
    legend_height <- tmap_params[["legend_height"]]
    legend_position <- tmap_params[["legend_position"]]

    cols4all_name <- cols4all::c4a_info(palette)$fullname

    p <- tmap::tm_shape(st, raster.downsample = FALSE) +
        tmap::tm_raster(
            col.scale = tm_scale_continuous(values = cols4all_name,
                                            midpoint = NA),
            col.legend = tmap::tm_legend(
                title = band,
                position = tmap::tm_pos_in("left", "bottom"),
                frame = TRUE,
                bg.color = "white",
                bg.alpha = 0.3
            )
        ) +
        tmap::tm_graticules(labels.size = 0.7) +
        tmap::tm_compass()

    return(p)
}
#' @title  Plot a RGB color image with tmap
#' @name   .tmap_rgb_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  st            Stars object.
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  main_title    Main title for the plot
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.tmap_rgb_color <- function(rgb_st,
                            sf_seg, seg_color, line_width,
                            main_title, scale, tmap_params) {

    if (as.numeric_version(packageVersion("tmap")) < "3.9")
        class(rgb_st) <- "tmap_v3"
    else
        class(rgb_st) <- "tmap_v4"
    UseMethod(".tmap_rgb_color", rgb_st)
}
#' @export
.tmap_rgb_color.tmap_v3 <- function(rgb_st,
                                    sf_seg, seg_color, line_width,
                                    main_title, scale, tmap_params) {

    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]

    p <- tmap::tm_shape(rgb_st, raster.downsample = FALSE) +
        tmap::tm_raster() +
        tmap::tm_graticules(
            labels.size = labels_size
        ) +
        tmap::tm_layout(
            main.title = main_title,
            main.title.size = 1,
            main.title.position = "center",
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
.tmap_rgb_color.tmap_v4 <- function(rgb_st,
                                    sf_seg, seg_color, line_width,
                                    main_title, scale, tmap_params) {
    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]

    p <- tmap::tm_shape(rgb_st, raster.downsample = FALSE) +
        tmap::tm_raster() +
        tmap::tm_graticules(
            labels.size = labels_size
        ) +
        tmap::tm_layout(
            main.title = main_title,
            main.title.size = 1,
            main.title.position = "center",
            scale = scale
        ) +
        tmap::tm_compass()

    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }
}
#' @title  Plot a color image with legend
#' @name   .tmap_class_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  st            Stars object.
#' @param  colors        Named vector with colors to be displayed
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.tmap_class_map <- function(st, colors, scale, tmap_params) {

    if (as.numeric_version(packageVersion("tmap")) < "3.9")
        class(st) <- "tmap_v3"
    else
        class(st) <- "tmap_v4"
    UseMethod(".tmap_class_map", st)
}
#' @export
.tmap_class_map.tmap_v3 <- function(st, colors, scale, tmap_params) {

    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]
    legend_width <- tmap_params[["legend_width"]]
    legend_height <- tmap_params[["legend_height"]]
    legend_position <- tmap_params[["legend_position"]]
    # plot using tmap
    p <- suppressMessages(
        tmap::tm_shape(st, raster.downsample = FALSE) +
            tmap::tm_raster(
                style = "cat",
                labels = colors[["label"]],
                palette = colors[["color"]]
            ) +
            tmap::tm_graticules(
                labels.size = labels_size,
                ndiscr = 50
            ) +
            tmap::tm_compass() +
            tmap::tm_layout(
                scale = scale,
                legend.bg.color = legend_bg_color,
                legend.bg.alpha = legend_bg_alpha,
                legend.title.size = legend_title_size,
                legend.text.size = legend_text_size,
                legend.width = legend_width,
                legend.height = legend_height,
                legend.position = legend_position
            )
    )
    return(p)
}
#' @export
.tmap_class_map.tmap_v4 <- function(st, colors, scale, tmap_params) {

    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]
    legend_width <- tmap_params[["legend_width"]]
    legend_height <- tmap_params[["legend_height"]]
    legend_position <- tmap_params[["legend_position"]]
    # plot using tmap
    p <- suppressMessages(
        tmap::tm_shape(st, raster.downsample = FALSE) +
            tmap::tm_raster(
                style = "cat",
                labels = colors[["label"]],
                palette = colors[["color"]]
            ) +
            tmap::tm_graticules(
                labels.size = labels_size,
                ndiscr = 50
            ) +
            tmap::tm_compass() +
            tmap::tm_layout(
                scale = scale,
                legend.bg.color = legend_bg_color,
                legend.bg.alpha = legend_bg_alpha,
                legend.title.size = legend_title_size,
                legend.text.size = legend_text_size,
                legend.width = legend_width,
                legend.height = legend_height,
                legend.position = legend_position
            )
    )
    return(p)
}
#' @title  Plot a probs image
#' @name   .tmap_probs_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  st            Stars object.
#' @param  labels        Class labels
#' @param  labels_plot   Class labels to be plotted
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.tmap_probs_map <- function(probs_st,
                            labels,
                            labels_plot,
                            palette,
                            rev,
                            scale,
                            tmap_params){
    if (as.numeric_version(packageVersion("tmap")) < "3.9")
        class(probs_st) <- "tmap_v3"
    else
        class(probs_st) <- "tmap_v4"
    UseMethod(".tmap_probs_map", probs_st)
}
#
#' @export
#'
#'
.tmap_probs_map.tmap_v3 <- function(probs_st,
                                    labels,
                                    labels_plot,
                                    palette,
                                    rev,
                                    scale,
                                    tmap_params){
    # revert the palette
    if (rev) {
        palette <- paste0("-", palette)
    }
    # select stars bands to be plotted
    bds <- as.numeric(names(labels[labels %in% labels_plot]))
    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]
    legend_width <- tmap_params[["legend_width"]]
    legend_height <- tmap_params[["legend_height"]]
    legend_position <- tmap_params[["legend_position"]]

    p <- tmap::tm_shape(probs_st[, , , bds]) +
        tmap::tm_raster(
            style = "cont",
            palette = palette,
            midpoint = NA,
            title = labels[labels %in% labels_plot]
        ) +
        tmap::tm_graticules(
            labels.size = labels_size
        ) +
        tmap::tm_facets(sync = FALSE) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale           = scale,
            legend.show     = TRUE,
            legend.outside  = FALSE,
            legend.bg.color = legend_bg_color,
            legend.bg.alpha = legend_bg_alpha,
            legend.title.size = legend_title_size,
            legend.text.size = legend_text_size,
            legend.width = legend_width,
            legend.height = legend_height,
            legend.position = legend_position
        )

}
#
#' @export
#'
#'
.tmap_probs_map.tmap_v4 <- function(probs_st,
                                    labels,
                                    labels_plot,
                                    palette,
                                    rev,
                                    scale,
                                    tmap_params){
    # revert the palette
    if (rev) {
        palette <- paste0("-", palette)
    }
    # select stars bands to be plotted
    bds <- as.numeric(names(labels[labels %in% labels_plot]))
    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]
    legend_width <- tmap_params[["legend_width"]]
    legend_height <- tmap_params[["legend_height"]]
    legend_position <- tmap_params[["legend_position"]]

    p <- tmap::tm_shape(probs_st[, , , bds]) +
        tmap::tm_raster(
            style = "cont",
            palette = palette,
            midpoint = NA,
            title = labels[labels %in% labels_plot]
        ) +
        tmap::tm_graticules(
            labels.size = labels_size
        ) +
        tmap::tm_facets(sync = FALSE) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale           = scale,
            legend.show     = TRUE,
            legend.outside  = FALSE,
            legend.bg.color = legend_bg_color,
            legend.bg.alpha = legend_bg_alpha,
            legend.title.size = legend_title_size,
            legend.text.size = legend_text_size,
            legend.width = legend_width,
            legend.height = legend_height,
            legend.position = legend_position
        )

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
#' \item \code{legend_width}: relative width of legend (default = 1.0)
#' \item \code{legend_position}: 2D position of legend (default = c("left", "bottom"))
#' \item \code{legend_height}: relative height of legend (default = 1.0)
#' }
.tmap_params_set <- function(dots){

    # tmap params
    graticules_labels_size <- as.numeric(.conf("plot", "graticules_labels_size"))
    legend_bg_color <- .conf("plot", "legend_bg_color")
    legend_bg_alpha <- as.numeric(.conf("plot", "legend_bg_alpha"))
    legend_title_size <- as.numeric(.conf("plot", "legend_title_size"))
    legend_text_size <- as.numeric(.conf("plot", "legend_text_size"))
    legend_height <- as.numeric(.conf("plot", "legend_height"))
    legend_width <- as.numeric(.conf("plot", "legend_width"))
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
    if ("legend_height" %in% names(dots))
        legend_height <- dots[["legend_height"]]
    if ("legend_width" %in% names(dots))
        legend_width <- dots[["legend_width"]]
    if ("legend_position" %in% names(dots))
        legend_position <- dots[["legend_position"]]

    tmap_params <- list(
        "graticules_labels_size" = graticules_labels_size,
        "legend_bg_color" = legend_bg_color,
        "legend_bg_alpha" = legend_bg_alpha,
        "legend_title_size" = legend_title_size,
        "legend_text_size" = legend_text_size,
        "legend_height" = legend_height,
        "legend_width" = legend_width,
        "legend_position" = legend_position
    )
    return(tmap_params)
}

