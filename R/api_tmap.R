#' @title  Plot a false color image with tmap
#' @name   .tmap_false_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a set of false color image
#' @keywords internal
#' @noRd
#' @param  rast          terra spRast object.
#' @param  band          Band to be plotted.
#' @param  title         Title of the plot
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.tmap_false_color <- function(rast,
                              band,
                              title,
                              sf_seg,
                              seg_color,
                              line_width,
                              palette,
                              rev,
                              scale,
                              tmap_params) {

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
        tmap::tm_credits(
            text = title,
            size = 0.85,
            position = tmap::tm_pos_in("right", "bottom"),
            bg.color = "white",
            bg.alpha = 0.65
        ) +
        tmap::tm_layout(
            scale = scale
        )
    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }

    p
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
.tmap_dem_map <- function(r,
                          band,
                          palette,
                          rev,
                          scale,
                          tmap_params) {
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
    p
}

#' @title  Plot a RGB color image with tmap
#' @name   .tmap_rgb_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  red_file      File to be plotted in red
#' @param  green_file    File to be plotted in green
#' @param  blue_file     File to be plotted in blue
#' @param  title         Title of the plot
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  max_value     Maximum value
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  tmap_params   List with tmap params for detailed plot control
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @return               A list of plot objects
.tmap_rgb_color <- function(red_file,
                            green_file,
                            blue_file,
                            title,
                            scale,
                            max_value,
                            first_quantile,
                            last_quantile,
                            tmap_params,
                            sf_seg,
                            seg_color,
                            line_width) {
    # open RGB file
    rast <- .raster_open_rast(c(red_file, green_file, blue_file))
    names(rast) <- c("red", "green", "blue")
    .raster_set_minmax(rast)

    p <- tmap::tm_shape(rast, raster.downsample = FALSE) +
        tmap::tm_rgb(
            col = tmap::tm_vars(n = 3L, multivariate = TRUE),
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
        tmap::tm_credits(
            text = title,
            size = 0.85,
            position = tmap::tm_pos_in("right", "bottom"),
            bg.color = "white",
            bg.alpha = 0.65
        )

    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }
    p
}
#' @title  Plot a probs image
#' @name   .tmap_probs_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  probs_rast    Spatial raster object.
#' @param  labels        Class labels
#' @param  labels_plot   Class labels to be plotted
#' @param  palette       A color palette available in cols4all
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A plot object
.tmap_probs_map <- function(probs_rast,
                            labels,
                            labels_plot,
                            palette,
                            rev,
                            scale,
                            tmap_params) {
    # recover palette name used by cols4all
    cols4all_name <- .colors_cols4all_name(palette)
    # reverse order of colors?
    if (rev)
        cols4all_name <- paste0("-", cols4all_name)

    # select bands to be plotted
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
                bg.alpha = tmap_params[["legend_bg_alpha"]]
            )
        ) +
        tmap::tm_facets() +
        tmap::tm_graticules(
            labels.size = tmap_params[["graticules_labels_size"]]
        ) +
        tmap::tm_layout(
            scale = scale
        )
    p
}
#
#' @title  Plot a color image with legend
#' @name   .tmap_class_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a RGB color image
#' @keywords internal
#' @noRd
#' @param  rast          Categorical terra Spatial Raster
#' @param  colors        Named vector with colors to be displayed
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A plot object
.tmap_class_map <- function(rast, colors, scale, tmap_params) {

    # position
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")

    .raster_set_minmax(rast)

    # plot using tmap
    p <- tmap::tm_shape(rast, raster.downsample = FALSE) +
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
            ndiscr = 50L
        ) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale = scale
        )
    p
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
                               scale, tmap_params) {
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
    tmap::tm_shape(sf_seg) +
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
.tmap_vector_class <- function(sf_seg, colors, scale, tmap_params) {
    # position
    legend_position <- tmap_params[["legend_position"]]
    if (legend_position == "outside")
        position <- tmap::tm_pos_out()
    else
        position <- tmap::tm_pos_in("left", "bottom")
    # sort the color vector
    colors <- colors[sort(names(colors))]
    # plot the data using tmap
    tmap::tm_shape(sf_seg) +
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
                                type, scale, tmap_params) {
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
            fill = type,
            fill.scale = tmap::tm_scale_continuous(
                values = cols4all_name,
                midpoint = NA),
            fill.legend = tmap::tm_legend(
                frame = TRUE,
                title = "uncert",
                position = position,
                title.size = tmap_params[["legend_title_size"]],
                text.size = tmap_params[["legend_text_size"]],
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
    p
}
#' @title  Prepare tmap params for dots value
#' @name .tmap_params_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @noRd
#' @keywords internal
#' @param   dots               params passed on dots
#' @param   legend_position    position of legend ("inside", "outside"))
#' @param   legend_title       title of legend
#' @description The following optional parameters are available
#'       to allow for detailed control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coord labels (default = 0.8)
#' \item \code{legend_title_size}: relative size of legend title (default = 1.0)
#' \item \code{legend_text_size}: relative size of legend text (default = 1.0)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.5)
#' }
.tmap_params_set <- function(dots, legend_position, legend_title = NULL) {

    # tmap params
    graticules_labels_size <- as.numeric(.conf("plot",
                                               "graticules_labels_size"))
    legend_bg_color <- .conf("plot", "legend_bg_color")
    legend_bg_alpha <- as.numeric(.conf("plot", "legend_bg_alpha"))
    legend_title_size <- as.numeric(.conf("plot", "legend_title_size"))
    legend_text_size <- as.numeric(.conf("plot", "legend_text_size"))

    # deal with legend position separately
    if (!.has(legend_position))
        legend_position <- .conf("plot", "legend_position")

    # deal with legend title separately
    if (!.has(legend_title))
        legend_title <- .conf("plot", "legend_title")

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

    list(
        "graticules_labels_size" = graticules_labels_size,
        "legend_bg_color" = legend_bg_color,
        "legend_bg_alpha" = legend_bg_alpha,
        "legend_title" = legend_title,
        "legend_title_size" = legend_title_size,
        "legend_text_size" = legend_text_size,
        "legend_position" = legend_position
    )
}
