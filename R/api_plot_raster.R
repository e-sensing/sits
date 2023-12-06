#' @title  Plot a false color image
#' @name   .plot_false_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a set of false color image
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  band          Band to be plotted.
#' @param  date          Date to be plotted.
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Reverse the color palette?
#' @param  tmap_options  Named vector with optional tmap parameters:
#'                       scale (default = 1.0)
#'                       max_cells (default: 1e+06)
#'                       graticules_labels_size (default: 0.7)
#'                       legend_title_size (default: 1.5)
#'                       legend_text_size (default: 1.2)
#'                       legend_bg_color (default: "white")
#'                       legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_false_color <- function(tile,
                              band,
                              date,
                              sf_seg    = NULL,
                              seg_color = NULL,
                              line_width = 0.2,
                              palette,
                              rev,
                              tmap_options) {
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # deal with color palette
    .check_palette(palette)
    # reverse the color palette?
    if (rev) {
        palette <- paste0("-", palette)
    }

    # select the file to be plotted
    bw_file <- .tile_path(tile, band, date)

    # size of data to be read
    size <- .plot_read_size(
        tile = tile,
        tmap_options = tmap_options
    )

    # read file
    stars_obj <- stars::read_stars(
        bw_file,
        RasterIO = list(
            "nBufXSize" = size[["xsize"]],
            "nBufYSize" = size[["ysize"]]
        ),
        proxy = FALSE
    )

    # rescale the stars object
    band_conf <- .tile_band_conf(tile = tile, band = band)
    scale <- .scale(band_conf)
    offset <- .offset(band_conf)
    stars_obj <- stars_obj * scale + offset

    # set the tmap options
    tmap_params <- .plot_tmap_params(tmap_options)
    p <- suppressMessages(
        tmap::tm_shape(stars_obj) +
            tmap::tm_raster(
                style = "cont",
                palette = palette,
                title = band,
                midpoint = NA
            ) +
            tmap::tm_graticules(
                labels.size = tmap_params[["graticules_labels_size"]]
            ) +
            tmap::tm_compass() +
            tmap::tm_layout(
                legend.bg.color = tmap_params[["legend_bg_color"]],
                legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
                legend.title.size = tmap_params[["legend_title_size"]],
                legend.text.size = tmap_params[["legend_text_size"]]
            )
    )
    # include segments
    if (!purrr::is_null(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }
    return(p)
}
#' @title  Plot a classified image
#' @name   .plot_class_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified image
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  legend        Legend for the classes
#' @param  palette       A sequential RColorBrewer palette
#' @param  tmap_options  Named vector with optional tmap parameters
#'                       max_cells (default: 1e+06)
#'                       scale (default: 0.8)
#'                       font_family (default: "plex_sans")
#'                       graticules_labels_size (default: 0.7)
#'                       legend_title_size (default: 0.8)
#'                       legend_text_size (default: 0.8)
#'                       legend_bg_color (default: "white")
#'                       legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_class_image <- function(tile, legend, palette, tmap_options) {
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")

    # deal with color palette
    .check_palette(palette)
    # get the labels
    labels <- unlist(.cube_labels(tile, dissolve = FALSE))
    # obtain the colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    names(colors) <- names(labels)
    # size of data to be read
    size <- .plot_read_size(tile = tile, tmap_options = tmap_options)
    # select the image to be plotted
    class_file <- .tile_path(tile)

    # read file
    stars_obj <- stars::read_stars(
        class_file,
        RasterIO = list(
            "nBufXSize" = size[["xsize"]],
            "nBufYSize" = size[["ysize"]]
        ),
        proxy = FALSE
    )

    # rename stars object
    stars_obj <- stats::setNames(stars_obj, "labels")

    # set the tmap options
    tmap_params <- .plot_tmap_params(tmap_options)

    # plot using tmap
    p <- suppressMessages(
        tmap::tm_shape(stars_obj) +
            tmap::tm_raster(
                style = "cat",
                palette = colors,
                labels = labels
            ) +
            tmap::tm_graticules(
                labels.size = tmap_params[["graticules_labels_size"]]
            ) +
            tmap::tm_compass() +
            tmap::tm_layout(
                scale           = tmap_params[["scale"]],
                fontfamily      = tmap_params[["font_family"]],
                legend.show     = TRUE,
                legend.outside  = tmap_params[["legend_outside"]],
                legend.bg.color = tmap_params[["legend_bg_color"]],
                legend.bg.alpha = tmap_params[["legend_bg_alpha"]],
                legend.title.size = tmap_params[["legend_title_size"]],
                legend.text.size = tmap_params[["legend_text_size"]],
                legend.width     = tmap_params[["legend_width"]],
                legend.height    = tmap_params[["legend_height"]],
                legend.position  = tmap_params[["legend_position"]]
            )
    )
    return(p)
}

#' @title  Plot probs
#' @name   .plot_probs
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  tile          Probs cube to be plotted.
#' @param  labels_plot   Labels to be plotted
#' @param  palette       A sequential RColorBrewer palette
#' @param  rev           Reverse the color palette?
#' @param  tmap_options  Named vector with optional tmap parameters
#'                       max_cells (default: 1e+06)
#'                       graticules_labels_size (default: 0.7)
#'                       legend_title_size (default: 1.5)
#'                       legend_text_size (default: 1.2)
#'                       legend_bg_color (default: "white")
#'                       legend_bg_alpha (default: 0.5)
#'                       scale (default: 1.0)
#' @return               A plot object
#'
.plot_probs <- function(tile,
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
    # size of data to be read
    size <- .plot_read_size(
        tile = tile,
        tmap_options = tmap_options
    )
    # get the path
    probs_path <- .tile_path(tile)
    # read the file using stars
    probs_st <- stars::read_stars(
        probs_path,
        RasterIO = list(
            "nBufXSize" = size[["xsize"]],
            "nBufYSize" = size[["ysize"]]
        ),
        proxy = FALSE
    )
    # get the band
    band <- .tile_bands(tile)
    band_conf <- .tile_band_conf(tile, band)
    # scale the data
    probs_st <- probs_st * .scale(band_conf)

    # rename stars object dimensions to labels
    probs_st <- stars::st_set_dimensions(probs_st, "band",
        values = labels
    )
    # select stars bands to be plotted
    bds <- as.numeric(names(labels[labels %in% labels_plot]))

    # set the tmap options
    tmap_options <- .plot_tmap_params(tmap_options)

    p <- tmap::tm_shape(probs_st[, , , bds]) +
        tmap::tm_raster(
            style = "cont",
            palette = palette,
            midpoint = 0.5,
            title = labels[labels %in% labels_plot]
        ) +
        tmap::tm_facets(free.coords = TRUE) +
        tmap::tm_compass() +
        tmap::tm_layout(
            legend.show = TRUE,
            legend.outside = FALSE,
            legend.bg.color = tmap_options[["legend_bg_color"]],
            legend.bg.alpha = tmap_options[["legend_bg_alpha"]],
            legend.title.size = tmap_options[["legend_title_size"]],
            legend.text.size = tmap_options[["legend_text_size"]],
            outer.margins = 0
        )

    return(p)
}
#' @title  Plot variance histogram
#' @name   .plot_variance_hist
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  tile          Variance cube to be plotted.
#'
#' @return               A plot object
#'
.plot_variance_hist <- function(tile) {
    # get all labels to be plotted
    labels <- sits_labels(tile)
    # get the path
    var_path <- .tile_path(tile)
    # get the bounding box as an sf object
    sf_cube <- .bbox_as_sf(.bbox(tile))
    # numbers of nrows and ncols
    nrows <- .tile_nrows(tile)
    ncols <- .tile_ncols(tile)
    # sample the pixels
    n_samples <- as.integer(nrows / 5 * ncols / 5)
    points <- sf::st_sample(sf_cube, size = n_samples)
    points <- sf::st_coordinates(points)
    # get the r object
    r_obj <- .raster_open_rast(var_path)
    # read the file
    values <- .raster_extract(r_obj, points)
    # scale the data
    band_conf <- .conf_derived_band(
        derived_class = "variance_cube",
        band = "variance"
    )
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0) {
        values <- values + offset
    }
    # convert to tibble
    values <- tibble::as_tibble(values)
    # include label names
    colnames(values) <- labels
    # dissolve the data for plotting
    values <- tidyr::pivot_longer(values,
        cols = tidyr::everything(),
        names_to = "labels",
        values_to = "variance"
    )
    # Histogram with density plot
    p <- ggplot2::ggplot(
        values,
        ggplot2::aes(x = .data[["variance"]])
    ) +
        ggplot2::geom_histogram(
            binwidth = 1,
            fill = "#69b3a2",
            color = "#e9ecef",
            alpha = 0.9
        ) +
        ggplot2::scale_x_continuous()
    p <- p + ggplot2::facet_wrap(facets = "labels")

    return(p)
}
#' @title  Plot a RGB image
#' @name   .plot_rgb
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted
#' @param  red           Band to be plotted in red
#' @param  green         Band to be plotted in green
#' @param  blue          Band to be plotted in blue
#' @param  date          Date to be plotted
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  tmap_options  Named vector with optional tmap parameters
#'                       max_cells (default: 1e+06)
#'                       graticules_labels_size (default: 0.7)
#'                       legend_title_size (default: 1.5)
#'                       legend_text_size (default: 1.2)
#'                       legend_bg_color (default: "white")
#'                       legend_bg_alpha (default: 0.5)
#'                       scale (default: 1.0)
#' @return               A plot object
#'
.plot_rgb <- function(tile,
                      red,
                      green,
                      blue,
                      date,
                      sf_seg = NULL,
                      seg_color = NULL,
                      line_width = 0.2,
                      tmap_options) {
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")

    # get RGB files for the requested timeline
    red_file <- .tile_path(tile, red, date)
    green_file <- .tile_path(tile, green, date)
    blue_file <- .tile_path(tile, blue, date)

    # size of data to be read
    size <- .plot_read_size(
        tile = tile,
        tmap_options = tmap_options
    )
    # read raster data as a stars object with separate RGB bands
    rgb_st <- stars::read_stars(
        c(red_file, green_file, blue_file),
        along = "band",
        RasterIO = list(
            "nBufXSize" = size[["xsize"]],
            "nBufYSize" = size[["ysize"]]
        ),
        proxy = FALSE
    )
    # get the max values
    band_params <- .tile_band_conf(tile, red)
    max_value <- .max_value(band_params)

    rgb_st <- stars::st_rgb(rgb_st[, , , 1:3],
        dimension = "band",
        maxColorValue = max_value,
        use_alpha = FALSE,
        probs = c(0.05, 0.95),
        stretch = TRUE
    )

    tmap_options <- .plot_tmap_params(tmap_options)

    p <- tmap::tm_shape(rgb_st) +
        tmap::tm_raster() +
        tmap::tm_graticules(
            labels.size = tmap_options[["graticules_labels_size"]]
        ) +
        tmap::tm_compass()

    # include segments
    if (!purrr::is_null(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }

    return(p)
}
#' @title  Return the cell size for the image to be reduced for plotting
#' @name .plot_read_size
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  tile       Tile to be plotted.
#' @param  tmap_options  Named vector with options
#' @return            Cell size for x and y coordinates.
#'
#'
.plot_read_size <- function(tile, tmap_options) {
    # get the maximum number of bytes to be displayed
    max_cells <- 1e+07
    # max_raster <- c(plot = max_cells, view = max_cells)
    # set the options for tmap
    # tmap::tmap_options(max.raster = max_raster)
    # numbers of nrows and ncols
    nrows <- max(.tile_nrows(tile))
    ncols <- max(.tile_ncols(tile))

    # do we need to compress?
    ratio <- max((nrows * ncols / max_cells), 1)
    # only create local files if required
    if (ratio > 1) {
        new_nrows <- round(nrows / sqrt(ratio))
        new_ncols <- round(ncols * (new_nrows / nrows))
    } else {
        new_nrows <- round(nrows)
        new_ncols <- round(ncols)
    }
    return(c(
        "xsize" = new_ncols, "ysize" = new_nrows
    ))
}

#' @title  Return the tmap params
#' @name .plot_tmap_params
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  tmap_user  Named vector with optional tmap parameters
#' @return            Updated tmap params.
#'
.plot_tmap_params <- function(tmap_user) {
    # reset the tmap params
    suppressMessages(tmap::tmap_options_reset())
    # get the tmap defaults
    tmap_options <- list(
        graticules_labels_size =
            as.numeric(.conf("tmap", "graticules_labels_size")),
        legend_title_size     =  as.numeric(.conf("tmap", "legend_title_size")),
        legend_text_size      =  as.numeric(.conf("tmap", "legend_text_size")),
        legend_width          =  as.numeric(.conf("tmap", "legend_width")),
        legend_height         =  as.numeric(.conf("tmap", "legend_height")),
        legend_position       =  .conf("tmap", "legend_position"),
        legend_outside        =  .conf("tmap", "legend_outside"),
        legend_outside_position = .conf("tmap", "legend_outside_position"),
        legend_bg_color       =  .conf("tmap", "legend_bg_color"),
        legend_bg_alpha       = as.numeric(.conf("tmap", "legend_bg_alpha")),
        scale                 = as.numeric(.conf("tmap", "scale")),
        font_family           = .conf("tmap", "font_family")
    )
    if (!purrr::is_null(tmap_user)) {
        keys <- unique(c(names(tmap_user), names(tmap_options)))
        .check_that(
            all(keys %in% names(tmap_options)),
            msg = paste("invalid tmap params - valid params are ",
                        keys, collapse = " ")
        )
        for (k in names(tmap_user))
            tmap_options[[k]] <- tmap_user[[k]]
    }
    # set tmap options
    tmap::tmap_options(scale = as.numeric(tmap_options[["scale"]]),
                       legend.title.size     =  as.numeric(tmap_options[["legend_title_size"]]),
                       legend.text.size      =  as.numeric(tmap_options[["legend_text_size"]]),
                       legend.width          =  as.numeric(tmap_options[["legend_width"]]),
                       legend.height         =  as.numeric(tmap_options[["legend_height"]]),
                       legend.position       =  tmap_options[["legend_position"]],
                       legend.outside        =  tmap_options[["legend_outside"]],
                       legend.outside.position = tmap_options[["legend_outside_position"]],
                       legend.bg.color       =  tmap_options[["legend_bg_color"]],
                       legend.bg.alpha       =  as.numeric(tmap_options[["legend_bg_alpha"]]),
                       fontfamily            =  tmap_options[["font_family"]]
                       )
    return(tmap_options)
}
