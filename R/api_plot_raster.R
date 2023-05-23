#' @title  Plot a false color image
#' @name   .plot_false_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a set of false color image
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  band          Band to be plotted.
#' @param  date          Date to be plotted.
#' @param  palette       A sequential RColorBrewer palette?
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_false_color <- function(tile,
                              band,
                              date = NULL,
                              palette,
                              tmap_options) {

    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")

    # select the file to be plotted
    bw_file <- .tile_path(tile, band, date)

    # size of data to be read
    size <- .plot_read_size(tile = tile,
                            tmap_options = tmap_options)

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
    labels_size <- as.numeric(.conf("tmap_graticules_labels_size"))
    title_size  <- as.numeric(.conf("tmap_legend_title_size"))
    text_size   <- as.numeric(.conf("tmap_legend_text_size"))
    bg_color <- .conf("tmap_legend_bg_color")
    bg_alpha <- as.numeric(.conf("tmap_legend_bg_alpha"))
    # user specified tmap options
    if (!purrr::is_null(tmap_options)) {
        # graticules label size
        if (!purrr::is_null(tmap_options[["tmap_graticules_labels_size"]]))
            labels_size <- as.numeric(
                tmap_options[["tmap_graticules_labels_size"]])
        # legend title size
        if (!purrr::is_null(tmap_options[["tmap_legend_title_size"]]))
            title_size <- as.numeric(
                tmap_options[["tmap_legend_title_size"]])
        # legend text size
        if (!purrr::is_null(tmap_options[["tmap_legend_text_size"]]))
            text_size <- as.numeric(
                tmap_options[["tmap_legend_text_size"]])
        # tmap legend bg color
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_color"]]))
            bg_color <- tmap_options[["tmap_legend_bg_color"]]
        # tmap legend bg alpha
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_alpha"]]))
            bg_alpha <- as.numeric(tmap_options[["tmap_legend_bg_alpha"]])
    }

    p <- suppressMessages(
        tmap::tm_shape(stars_obj) +
            tmap::tm_raster(
                style = "cont",
                palette = palette,
                title = band,
                midpoint = NA) +
            tmap::tm_graticules(
                labels.size = labels_size
            )  +
            tmap::tm_compass() +
            tmap::tm_layout(legend.title.size = title_size,
                            legend.text.size = text_size,
                            legend.bg.color = bg_color,
                            legend.bg.alpha = bg_alpha)
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
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_rgb <- function(tile, red, green, blue, date, tmap_options) {

    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")

    # get RGB files for the requested timeline
    red_file   <- .tile_path(tile, red, date)
    green_file <- .tile_path(tile, green, date)
    blue_file  <- .tile_path(tile, blue, date)

    # size of data to be read
    size <- .plot_read_size(tile = tile,
                            tmap_options = tmap_options)
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
    band_params   <- .tile_band_conf(tile, red)
    max_value <- .max_value(band_params)

    rgb_st <- stars::st_rgb(rgb_st[, , , 1:3],
                            dimension = "band",
                            maxColorValue = max_value,
                            use_alpha = FALSE,
                            probs = c(0.05, 0.95),
                            stretch = TRUE)

    p <- tmap::tm_shape(rgb_st) +
        tmap::tm_raster() +
        tmap::tm_graticules() +
        tmap::tm_compass()

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
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_class_image <- function(tile, legend, palette, tmap_options) {

    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")

    # get the labels
    labels <- sits_labels(tile)
    names(labels) <- seq_along(labels)
    # obtain the colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette
    )
    names(colors) <- seq_along(labels)
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
    labels_size <- as.numeric(.conf("tmap_graticules_labels_size"))
    title_size  <- as.numeric(.conf("tmap_legend_title_size"))
    text_size   <- as.numeric(.conf("tmap_legend_text_size"))
    bg_color <- .conf("tmap_legend_bg_color")
    bg_alpha <- as.numeric(.conf("tmap_legend_bg_alpha"))
    # user specified tmap options
    if (!purrr::is_null(tmap_options)) {
        # graticules label size
        if (!purrr::is_null(tmap_options[["tmap_graticules_labels_size"]]))
            labels_size <- as.numeric(
                tmap_options[["tmap_graticules_labels_size"]])
        # legend title size
        if (!purrr::is_null(tmap_options[["tmap_legend_title_size"]]))
            title_size <- as.numeric(
                tmap_options[["tmap_legend_title_size"]])
        # legend text size
        if (!purrr::is_null(tmap_options[["tmap_legend_text_size"]]))
            text_size <- as.numeric(
                tmap_options[["tmap_legend_text_size"]])
        # tmap legend bg color
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_color"]]))
            bg_color <- tmap_options[["tmap_legend_bg_color"]]
        # tmap legend bg alpha
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_alpha"]]))
            bg_alpha <- as.numeric(tmap_options[["tmap_legend_bg_alpha"]])
    }

    # plot using tmap
    # tmap requires numbers, not names
    names(colors) <- seq_along(names(colors))
    p <- suppressMessages(
        tmap::tm_shape(stars_obj) +
            tmap::tm_raster(
                style = "cat",
                palette = colors,
                labels = labels) +
            tmap::tm_graticules(
                labels.size = labels_size
            )  +
            tmap::tm_compass() +
            tmap::tm_layout(
                legend.show = TRUE,
                legend.outside = FALSE,
                legend.title.size = title_size,
                legend.text.size = text_size,
                legend.bg.color = bg_color,
                legend.bg.alpha = bg_alpha)
    )
    return(p)
}
#' @title  Plot probs
#' @name   .plot_probs
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  tile          Probs cube to be plotted.
#' @param  labels        Labels to be plotted
#' @param  palette       A sequential RColorBrewer palette
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_probs <- function(tile, labels, palette, tmap_options) {

    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # size of data to be read
    size <- .plot_read_size(tile = tile,
                            tmap_options = tmap_options)
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

    all_labels <- sits_labels(tile)
    names(all_labels) <- seq_len(length(all_labels))
    # rename stars object dimensions to labels
    probs_st <- stars::st_set_dimensions(probs_st, "band",
                                         values = all_labels)
    # select stars bands to be plotted
    bds <- as.numeric(names(all_labels[all_labels %in% labels]))

    # set the tmap options
    labels_size <- as.numeric(.conf("tmap_graticules_labels_size"))
    title_size  <- as.numeric(.conf("tmap_legend_title_size"))
    text_size   <- as.numeric(.conf("tmap_legend_text_size"))
    bg_color <- .conf("tmap_legend_bg_color")
    bg_alpha <- as.numeric(.conf("tmap_legend_bg_alpha"))
    # user specified tmap options
    if (!purrr::is_null(tmap_options)){
        # graticules label size
        if (!purrr::is_null(tmap_options[["tmap_graticules_labels_size"]]))
            labels_size <- as.numeric(
                tmap_options[["tmap_graticules_labels_size"]])
        # legend title size
        if (!purrr::is_null(tmap_options[["tmap_legend_title_size"]]))
            title_size <- as.numeric(
                tmap_options[["tmap_legend_title_size"]])
        # legend text size
        if (!purrr::is_null(tmap_options[["tmap_legend_text_size"]]))
            text_size <- as.numeric(
                tmap_options[["tmap_legend_text_size"]])
        # tmap legend bg color
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_color"]]))
            bg_color <- tmap_options[["tmap_legend_bg_color"]]
        # tmap legend bg alpha
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_alpha"]]))
            bg_alpha <- as.numeric(tmap_options[["tmap_legend_bg_alpha"]])
    }

    p <- tmap::tm_shape(probs_st[, , , bds]) +
        tmap::tm_raster(style = "cont",
                        palette = palette,
                        midpoint = 0.5,
                        title = all_labels[all_labels %in% labels]) +
        tmap::tm_facets(free.coords = TRUE) +
        tmap::tm_compass() +
        tmap::tm_layout(legend.show = TRUE,
                        legend.outside = FALSE,
                        legend.bg.color = bg_color,
                        legend.bg.alpha = bg_alpha,
                        legend.title.size = title_size,
                        legend.text.size = text_size,
                        outer.margins = 0)

    return(p)
}
#' @title  Plot variance map
#' @name   .plot_variance_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  tile          Variance cube to be plotted.
#' @param  labels.       Labels to be plotted
#' @param  palette       A sequential RColorBrewer palette
#' @param  percentile    Minimum percentile of variance to be plotted
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#'
#' @return               A plot object
#'
.plot_variance_map <- function(tile,
                               labels,
                               palette,
                               percentile,
                               tmap_options) {

    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # size of data to be read
    size <- .plot_read_size(tile = tile,
                            tmap_options = tmap_options)
    # get the path
    var_path <- .tile_path(tile)
    # read the file using stars
    var_st <- stars::read_stars(
        var_path,
        RasterIO = list(
            "nBufXSize" = size[["xsize"]],
            "nBufYSize" = size[["ysize"]]
        ),
        proxy = FALSE
    )
    # filter the data
    all_labels <- sits_labels(tile)
    names(all_labels) <- seq_len(length(all_labels))
    # rename stars object dimensions to labels
    var_st <- stars::st_set_dimensions(var_st, "band",
                                       values = all_labels)
    # get the band
    band <- .tile_bands(tile)
    band_conf <- .tile_band_conf(tile, band)
    # scale the data
    var_st <- var_st * .scale(band_conf)
    # split the data
    var_spl <- split(var_st)
    # get the percentile and filter bands
    for (lb in all_labels) {
        qt <- stats::quantile(var_spl[[lb]], percentile, na.rm = TRUE)
        var_spl[[lb]][var_spl[[lb]] < qt] <- NA
    }
    var_st <- merge(var_spl)
    # select stars bands to be plotted
    bds <- as.numeric(names(all_labels[all_labels %in% labels]))

    # set the tmap options
    labels_size <- as.numeric(.conf("tmap_graticules_labels_size"))
    title_size  <- as.numeric(.conf("tmap_legend_title_size"))
    text_size   <- as.numeric(.conf("tmap_legend_text_size"))
    bg_color    <- .conf("tmap_legend_bg_color")
    bg_alpha    <- as.numeric(.conf("tmap_legend_bg_alpha"))

    # user specified tmap options
    if (!purrr::is_null(tmap_options)) {
        # graticules label size
        if (!purrr::is_null(tmap_options[["tmap_graticules_labels_size"]]))
            labels_size <- as.numeric(
                tmap_options[["tmap_graticules_labels_size"]])
        # legend title size
        if (!purrr::is_null(tmap_options[["tmap_legend_title_size"]]))
            title_size <- as.numeric(
                tmap_options[["tmap_legend_title_size"]])
        # legend text size
        if (!purrr::is_null(tmap_options[["tmap_legend_text_size"]]))
            text_size <- as.numeric(
                tmap_options[["tmap_legend_text_size"]])
        # tmap legend bg color
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_color"]]))
            bg_color <- tmap_options[["tmap_legend_bg_color"]]
        # tmap legend bg alpha
        if (!purrr::is_null(tmap_options[["tmap_legend_bg_alpha"]]))
            bg_alpha <- as.numeric(tmap_options[["tmap_legend_bg_alpha"]])
    }

    p <- tmap::tm_shape(var_st[, , , bds]) +
        tmap::tm_raster(style = "cont",
                        palette = palette,
                        midpoint = 0.5,
                        title = all_labels[all_labels %in% labels]) +
        tmap::tm_facets(free.coords = TRUE) +
        tmap::tm_compass() +
        tmap::tm_layout(legend.show = TRUE,
                        legend.outside = FALSE,
                        legend.bg.color = bg_color,
                        legend.bg.alpha = bg_alpha,
                        legend.title.size = title_size,
                        legend.text.size = text_size,
                        outer.margins = 0)

    return(p)
}
#' @title  Plot variance histogram
#' @name   .plot_variance_hist
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  tile          Variance cube to be plotted
#' @param percentile     Minimum percentile of variance to be plotted
#' @param sample_size    Number of points to be sampled to obtain histogram
#'
#' @return               A plot object
#'
.plot_variance_hist <- function(tile, percentile, sample_size) {

    # get all labels to be plotted
    labels <- sits_labels(tile)
    # get the path
    var_path <- .tile_path(tile)
    # read the raster
    r <- terra::rast(var_path)
    # get the a sample of the values
    values <- r %>%
        terra::spatSample(size = sample_size, na.rm = TRUE)
    # get scale and offset
    band_conf <- .conf_derived_band(
        derived_class = "variance_cube",
        band = "variance"
    )
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    # include label names
    colnames(values) <- labels
    # filter by quantile
    values <- purrr::map_dfr(
        colnames(values), function(x) {
            vls <- values[[x]]
            quant <- stats::quantile(vls, percentile)
            vls <- tibble::tibble(
                variance = vls[vls > quant],
                labels = x
            )
            return(vls)
        })
    # Histogram with density plot
    p <- ggplot2::ggplot(values,
                         ggplot2::aes(x = .data[["variance"]])) +
        ggplot2::geom_histogram(binwidth = 1,
                                fill  = "#69b3a2",
                                color = "#e9ecef",
                                alpha = 0.9) +
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
#' @param  tmap_options  List with optional tmap parameters
#'                       tmap max_cells (default: 1e+06)
#'                       tmap_graticules_labels_size (default: 0.7)
#'                       tmap_legend_title_size (default: 1.5)
#'                       tmap_legend_text_size (default: 1.2)
#'                       tmap_legend_bg_color (default: "white")
#'                       tmap_legend_bg_alpha (default: 0.5)
#' @return            Cell size for x and y coordinates.
#'
#'
.plot_read_size <- function(tile, tmap_options) {

    # get the maximum number of bytes to be displayed
    if (!purrr::is_null(tmap_options[["tmap_max_cells"]]))
        max_cells <- tmap_options[["tmap_max_cells"]]
    else
        max_cells <- as.numeric(.conf("tmap_max_cells"))
    max_raster <- c(plot = max_cells, view = max_cells)
    # set the options for tmap
    tmap::tmap_options(max.raster = max_raster)
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
