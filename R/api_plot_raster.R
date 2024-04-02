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
#' @param  style         Method to process the color scale
#'                       ("cont", "order", "quantile", "fisher",
#'                        "jenks", "log10")
#' @param  n_colors      Number of colors to be plotted
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
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
                              style,
                              n_colors,
                              rev,
                              scale) {
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # deal with color palette
    .check_palette(palette)
    # Grayscale palette? reverse is TRUE
    if (palette == "Greys")
        rev <- TRUE
    # reverse the color palette?
    if (rev) {
        palette <- paste0("-", palette)
    }

    # select the file to be plotted
    bw_file <- .tile_path(tile, band, date)

    # size of data to be read
    size <- .plot_read_size(tile = tile)
    # used for SAR images without tiling system
    if (tile$tile == "NoTilingSystem")  {
        bw_file <- .gdal_warp_grd(bw_file, size)
    }
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
    band_scale <- .scale(band_conf)
    band_offset <- .offset(band_conf)
    stars_obj <- stars_obj * band_scale + band_offset

    # generate plot
    p <- suppressMessages(
        tmap::tm_shape(stars_obj) +
            tmap::tm_raster(
                style = style,
                n = n_colors,
                palette = palette,
                title = band,
                midpoint = NA
            ) +
            tmap::tm_graticules(
                labels.size = as.numeric(.conf("tmap", "graticules_labels_size"))
            ) +
            tmap::tm_compass() +
            tmap::tm_layout(
                scale = scale,
                legend.bg.color = .conf("tmap","legend_bg_color"),
                legend.bg.alpha = as.numeric(.conf("tmap", "legend_bg_alpha")),
                legend.title.size = as.numeric(.conf("tmap","legend_title_size")),
                legend.text.size = as.numeric(.conf("tmap","legend_text_size"))
            )
    )
    # include segments
    if (.has(sf_seg)) {
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
#' @param  scale         Scale to plot the map
#'
#' @return               A plot object
#'
.plot_class_image <- function(tile, legend, palette, scale) {
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
    size <- .plot_read_size(tile = tile)
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

    # plot using tmap
    p <- suppressMessages(
        tmap::tm_shape(stars_obj) +
            tmap::tm_raster(
                style = "cat",
                palette = colors,
                labels = labels
            ) +
            tmap::tm_graticules(
                labels.size = as.numeric(.conf("tmap", "graticules_labels_size"))
            ) +
            tmap::tm_compass() +
            tmap::tm_layout(
                scale = scale,
                legend.bg.color = .conf("tmap","legend_bg_color"),
                legend.bg.alpha = as.numeric(.conf("tmap", "legend_bg_alpha")),
                legend.title.size = as.numeric(.conf("tmap","legend_title_size")),
                legend.text.size = as.numeric(.conf("tmap","legend_text_size"))
            )
    )
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
#' @return               A plot object
#'
.plot_rgb <- function(tile,
                      red,
                      green,
                      blue,
                      date,
                      sf_seg = NULL,
                      seg_color = NULL,
                      line_width = 0.2) {
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")

    # get RGB files for the requested timeline
    red_file <- .tile_path(tile, red, date)
    green_file <- .tile_path(tile, green, date)
    blue_file <- .tile_path(tile, blue, date)

    # size of data to be read
    size <- .plot_read_size(tile = tile)
    # used for SAR images
    if (tile$tile == "NoTilingSystem") {
        red_file   <- .gdal_warp_grd(red_file, size)
        green_file <- .gdal_warp_grd(green_file, size)
        blue_file  <- .gdal_warp_grd(blue_file, size)
    }
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

    p <- tmap::tm_shape(rgb_st) +
        tmap::tm_raster() +
        tmap::tm_graticules(
            labels.size = as.numeric(.conf("tmap", "graticules_labels_size"))
        ) +
        tmap::tm_compass()

    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }

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
#' @param  style         Method to process the color scale
#'                       ("cont", "order", "quantile", "fisher",
#'                        "jenks", "log10")
#' @param  n_colors      Number of colors to be shown
#' @param  rev           Reverse the color palette?
#' @param  scale         Global scale for plot
#' @return               A plot object
#'
.plot_probs <- function(tile,
                        labels_plot,
                        palette,
                        style,
                        n_colors,
                        rev,
                        scale) {
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
    if (.has_not(labels_plot)) {
        labels_plot <- labels
    } else {
        .check_that(all(labels_plot %in% labels),
            msg = "labels not in cube"
        )
    }
    # size of data to be read
    size <- .plot_read_size(tile = tile)
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

    p <- tmap::tm_shape(probs_st[, , , bds]) +
        tmap::tm_raster(
            style = style,
            palette = palette,
            n = n_colors,
            midpoint = NA,
            title = labels[labels %in% labels_plot]
        ) +
        tmap::tm_graticules(
            labels.size = as.numeric(.conf("tmap", "graticules_labels_size"))
        ) +
        tmap::tm_facets(sync = FALSE) +
        tmap::tm_compass() +
        tmap::tm_layout(
            scale           = scale,
            legend.show     = TRUE,
            legend.outside  = FALSE,
            legend.bg.color = .conf("tmap","legend_bg_color"),
            legend.bg.alpha = as.numeric(.conf("tmap", "legend_bg_alpha")),
            legend.title.size = as.numeric(.conf("tmap","legend_title_size")),
            legend.text.size = as.numeric(.conf("tmap","legend_text_size"))
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
#' @title  Return the cell size for the image to be reduced for plotting
#' @name .plot_read_size
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  tile       Tile to be plotted.
#' @return            Cell size for x and y coordinates.
#'
#'
.plot_read_size <- function(tile) {
    # get the maximum number of bytes to be displayed
    max_cells <- as.numeric(.conf("tmap", "max_cells"))
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
