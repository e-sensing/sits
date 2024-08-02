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
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.tmap_false_color <- function(st,
                              band,
                              sf_seg,
                              seg_color,
                              line_width,
                              palette,
                              rev,
                              scale,
                              tmap_params){

    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(st) <- "tmap_v3"
    else
        class(st) <- "tmap_v3"
    UseMethod(".tmap_false_color", st)
}
#' @export
.tmap_false_color.tmap_v3 <- function(st,
                                      band,
                                      sf_seg,
                                      seg_color,
                                      line_width,
                                      palette,
                                      rev,
                                      scale,
                                      tmap_params){

    # reverse the color palette?
    if (rev || palette == "Greys")
        palette <- paste0("-", palette)

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
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.bg.color = tmap_params[["legend_bg_color"]],
            legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
            legend.title.size = tmap_params[["legend_title_size"]],
            legend.text.size = tmap_params[["legend_text_size"]],
            legend.height = tmap_params[["legend_height"]],
            legend.width  = tmap_params[["legend_width"]],
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        )
    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }
    return(p)
}
#' @title  Plot a DEM
#' @name   .tmap_dem_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  r             Raster object.
#' @param  band          Band of DEM cube
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A plot object
.tmap_dem_map <- function(r, band,
                          palette, rev,
                          scale, tmap_params){
    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(r) <- "tmap_v3"
    else
        class(r) <- "tmap_v3"
    UseMethod(".tmap_dem_map", r)
}
#
#' @export
.tmap_dem_map.tmap_v3 <- function(r, band,
                                  palette, rev,
                                  scale, tmap_params){
    # reverse order of colors?
    if (rev)
        palette <- paste0("-", palette)
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
            legend.height = tmap_params[["legend_height"]],
            legend.width  = tmap_params[["legend_width"]],
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        )
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
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.tmap_rgb_color <- function(rgb_st,
                            sf_seg, seg_color, line_width,
                            scale, tmap_params) {

    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(rgb_st) <- "tmap_v3"
    else
        class(rgb_st) <- "tmap_v3"
    UseMethod(".tmap_rgb_color", rgb_st)
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
#' @return               A plot object
.tmap_probs_map <- function(probs_st,
                            labels,
                            labels_plot,
                            palette,
                            rev,
                            scale,
                            tmap_params){
    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(probs_st) <- "tmap_v3"
    else
        class(probs_st) <- "tmap_v3"
    UseMethod(".tmap_probs_map", probs_st)
}
#
#' @export
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

    p <- tmap::tm_shape(probs_st[, , , bds]) +
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
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        )
    return(p)
}

#' @title  Plot a vector probs map
#' @name   .tmap_vector_probs
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  sf_seg        sf
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Reverse the color palette?
#' @param  labels        Class labels
#' @param  labels_plot   Class labels to be plotted
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   Tmap parameters
#' @return               A plot object
.tmap_vector_probs <- function(sf_seg, palette, rev,
                               labels, labels_plot,
                               scale, tmap_params){
    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(sf_seg) <- "tmap_v3"
    else
        class(sf_seg) <- "tmap_v3"
    UseMethod(".tmap_vector_probs", sf_seg)
}
#' @export
.tmap_vector_probs.tmap_v3 <- function(sf_seg, palette, rev,
                                       labels, labels_plot,
                                       scale, tmap_params){
    # revert the palette?
    if (rev) {
        palette <- paste0("-", palette)
    }

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
#' @return               A plot object
.tmap_class_map <- function(st, colors, scale, tmap_params) {

    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(st) <- "tmap_v3"
    else
        class(st) <- "tmap_v3"
    UseMethod(".tmap_class_map", st)
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
#' @title  Plot a vector class map
#' @name   .tmap_vector_class
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  sf_seg        sf object.
#' @param  colors        Named vector with colors to be displayed
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   Parameters to control tmap output
#' @return               A plot object
.tmap_vector_class <- function(sf_seg, colors, scale, tmap_params){
    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(sf_seg) <- "tmap_v3"
    else
        class(sf_seg) <- "tmap_v3"
    UseMethod(".tmap_vector_class", sf_seg)
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
            legend.height = tmap_params[["legend_height"]],
            legend.width  = tmap_params[["legend_width"]],
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.2)

    return(p)
}
#' @title  Plot a vector uncertainty map
#' @name   .tmap_vector_uncert
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  sf_seg        sf
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Reverse the color palette?
#' @param  type          Uncertainty type
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   Tmap parameters
#' @return               A plot object
.tmap_vector_uncert <- function(sf_seg, palette, rev,
                                type, scale, tmap_params){
    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(sf_seg) <- "tmap_v3"
    else
        class(sf_seg) <- "tmap_v3"
    UseMethod(".tmap_vector_uncert", sf_seg)

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
            legend.height = tmap_params[["legend_height"]],
            legend.width  = tmap_params[["legend_width"]],
            legend.position = tmap_params[["legend_position"]],
            scale = scale
        ) +
        tmap::tm_borders(lwd = 0.2)

    return(suppressWarnings(p))
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

