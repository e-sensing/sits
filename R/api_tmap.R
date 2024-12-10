#' @title  Plot a false color image with tmap
#' @name   .tmap_false_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a set of false color image
#' @keywords internal
#' @noRd
#' @param  rast          terra spRast object.
#' @param  band          Band to be plotted.
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
                              sf_seg,
                              seg_color,
                              line_width,
                              palette,
                              rev,
                              scale,
                              tmap_params){

    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(rast) <- "tmap_v3"
    else
        class(rast) <- "tmap_v4"
    UseMethod(".tmap_false_color", rast)
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
        class(r) <- "tmap_v4"
    UseMethod(".tmap_dem_map", r)
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
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  max_value     Maximum value
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  tmap_params   List with tmap params for detailed plot control
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  sizes         COG sizes to be read
#' @return               A list of plot objects
.tmap_rgb_color <- function(red_file,
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
                            sizes) {
    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(red_file) <- c("tmap_v3", class(red_file))
    else
        class(red_file) <- c("tmap_v4", class(red_file))
    UseMethod(".tmap_rgb_color", red_file)
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
                            tmap_params){
    if (as.numeric_version(utils::packageVersion("tmap")) < "3.9")
        class(probs_rast) <- "tmap_v3"
    else
        class(probs_rast) <- "tmap_v4"
    UseMethod(".tmap_probs_map", probs_rast)
}
#
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
        class(st) <- "tmap_v4"
    UseMethod(".tmap_class_map", st)
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
        class(sf_seg) <- "tmap_v4"
    UseMethod(".tmap_vector_probs", sf_seg)
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
        class(sf_seg) <- "tmap_v4"
    UseMethod(".tmap_vector_class", sf_seg)
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
        class(sf_seg) <- "tmap_v4"
    UseMethod(".tmap_vector_uncert", sf_seg)

}
#' @title  Prepare tmap params for dots value
#' @name .tmap_params_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @noRd
#' @keywords internal
#' @param   dots               params passed on dots
#' @param   legend_position    position of legend ("inside", "outside"))
#' @param   legend_title       title of legend
#' @description The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.8)
#' \item \code{legend_title_size}: relative size of legend title (default = 1.0)
#' \item \code{legend_text_size}: relative size of legend text (default = 1.0)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.5)
#' }
.tmap_params_set <- function(dots, legend_position, legend_title = NULL){

    # tmap params
    graticules_labels_size <- as.numeric(.conf("plot", "graticules_labels_size"))
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

    tmap_params <- list(
        "graticules_labels_size" = graticules_labels_size,
        "legend_bg_color" = legend_bg_color,
        "legend_bg_alpha" = legend_bg_alpha,
        "legend_title" = legend_title,
        "legend_title_size" = legend_title_size,
        "legend_text_size" = legend_text_size,
        "legend_position" = legend_position
    )
    return(tmap_params)
}

