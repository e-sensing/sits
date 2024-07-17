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
#' @param  main_title    Main title for the plot
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  style         Style for plotting continuous data
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A list of plot objects
.plot_false_color <- function(tile,
                              band,
                              date,
                              sf_seg,
                              seg_color,
                              line_width,
                              palette,
                              main_title,
                              rev,
                              scale,
                              style,
                              max_cog_size,
                              first_quantile,
                              last_quantile,
                              tmap_params) {
    # select the file to be plotted
    bw_file <- .tile_path(tile, band, date)
    # size of data to be read
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # scale and offset
    band_conf <- .tile_band_conf(tile, band)
    band_scale <- .scale(band_conf)
    band_offset <- .offset(band_conf)
    max_value <- .max_value(band_conf)
    # retrieve the overview if COG
    bw_file <- .gdal_warp_file(bw_file, sizes)

    # read raster data as a stars object

    st <- stars::read_stars(bw_file,
        along = "band",
        RasterIO = list(
            nBufXSize = sizes[["xsize"]],
            nBufYSize = sizes[["ysize"]]
        ),
        proxy = FALSE
    )
    st <- st * band_scale + band_offset
    # extract the values
    vals <- as.vector(st[[1]])
    # obtain the quantiles
    quantiles <- stats::quantile(
        vals,
        probs = c(0, first_quantile, last_quantile, 1),
        na.rm = TRUE
    )
    minv <- quantiles[[1]]
    minq <- quantiles[[2]]
    maxq <- quantiles[[3]]
    maxv <- quantiles[[4]]

    vals <- ifelse(vals > minq, vals, minq)
    vals <- ifelse(vals < maxq, vals, maxq)
    st[[1]] <- vals
    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]

    # generate plot
    p <- tmap::tm_shape(st, raster.downsample = FALSE) +
        tmap::tm_raster(
            palette = palette,
            title = band,
            midpoint = NA,
            style = style,
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
            scale = scale
        )
    # include segments
    if (.has(sf_seg)) {
        p <- p + tmap::tm_shape(sf_seg) +
            tmap::tm_borders(col = seg_color, lwd = line_width)
    }
    return(p)

}

#' @title  Plot a multi-date band as RGB
#' @name   .plot_band_multidate
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a set of false color image
#' @keywords internal
#' @noRd
#' @param  tile          Tile to be plotted.
#' @param  band          Band to be plotted.
#' @param  dates         Dates to be plotted.
#' @param  palette       A sequential RColorBrewer palette
#' @param  main_title    Main title for the plot
#' @param  rev           Reverse the color palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  tmap_params   List with tmap params for detailed plot control
#'
#' @return               A list of plot objects
#'
.plot_band_multidate <- function(tile,
                                 band,
                                 dates,
                                 palette,
                                 main_title,
                                 rev,
                                 scale,
                                 max_cog_size,
                                 first_quantile,
                                 last_quantile,
                                 tmap_params) {
    # select the files to be plotted
    red_file   <- .tile_path(tile, band, dates[[1]])
    green_file <- .tile_path(tile, band, dates[[2]])
    blue_file  <- .tile_path(tile, band, dates[[3]])
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # get the max values
    band_params <- .tile_band_conf(tile, band)
    max_value <- .max_value(band_params)
    # used for SAR images without tiling system
    if (tile[["tile"]] == "NoTilingSystem")  {
        red_file   <- .gdal_warp_file(red_file, sizes)
        green_file <- .gdal_warp_file(green_file, sizes)
        blue_file  <- .gdal_warp_file(blue_file, sizes)
    }
    # plot multitemporal band as RGB
    p <- .plot_rgb_stars(
            red_file = red_file,
            green_file = green_file,
            blue_file = blue_file,
            sizes = sizes,
            max_value = max_value,
            main_title = main_title,
            sf_seg = NULL,
            seg_color = NULL,
            line_width = NULL,
            scale = scale,
            first_quantile = first_quantile,
            last_quantile = last_quantile,
            tmap_params = tmap_params
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
#' @param  main_title    Main title for the plot
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A plot object
#'
.plot_rgb <- function(tile,
                      red,
                      green,
                      blue,
                      date,
                      main_title,
                      sf_seg,
                      seg_color,
                      line_width,
                      scale,
                      max_cog_size,
                      first_quantile,
                      last_quantile,
                      tmap_params) {
    # get RGB files for the requested timeline
    red_file <- .tile_path(tile, red, date)
    green_file <- .tile_path(tile, green, date)
    blue_file <- .tile_path(tile, blue, date)

    # get the max values
    band_params <- .tile_band_conf(tile, red)
    max_value <- .max_value(band_params)
    # size of data to be read
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # used for SAR images
    if (tile[["tile"]] == "NoTilingSystem") {
        red_file   <- .gdal_warp_file(red_file, sizes)
        green_file <- .gdal_warp_file(green_file, sizes)
        blue_file  <- .gdal_warp_file(blue_file, sizes)
    }
    p <- .plot_rgb_stars(
        red_file = red_file,
        green_file = green_file,
        blue_file = blue_file,
        sizes = sizes,
        max_value = max_value,
        main_title = main_title,
        sf_seg = sf_seg,
        seg_color = seg_color,
        line_width = line_width,
        scale = scale,
        first_quantile = first_quantile,
        last_quantile = last_quantile,
        tmap_params = tmap_params
    )
    return(p)
}
#' @title  Plot a RGB image using stars and tmap
#' @name   .plot_rgb_stars
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  red_file      File to be plotted in red
#' @param  green_file    File to be plotted in green
#' @param  blue_file     File to be plotted in blue
#' @param  sizes         Image sizes for overview
#' @param  max_value     Maximum value
#' @param  main_title    Main title
#' @param  sf_seg        Segments (sf object)
#' @param  seg_color     Color to use for segment borders
#' @param  line_width    Line width to plot the segments boundary
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A plot object
#'
.plot_rgb_stars <- function(red_file,
                            green_file,
                            blue_file,
                            sizes,
                            max_value,
                            main_title,
                            sf_seg,
                            seg_color,
                            line_width,
                            scale,
                            first_quantile,
                            last_quantile,
                            tmap_params) {

    # read raster data as a stars object with separate RGB bands
    rgb_st <- stars::read_stars(
        c(red_file, green_file, blue_file),
        along = "band",
        RasterIO = list(
            nBufXSize = sizes[["xsize"]],
            nBufYSize = sizes[["ysize"]]
        ),
        proxy = FALSE
    )
    # open RGB stars
    rgb_st <- stars::st_rgb(rgb_st[, , , 1:3],
                            dimension = "band",
                            maxColorValue = max_value,
                            use_alpha = FALSE,
                            probs = c(first_quantile,
                                      last_quantile),
                            stretch = TRUE
    )
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
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  tmap_params   List with tmap params for detailed plot control
#' @return               A plot object
#'
.plot_class_image <- function(tile, legend, palette,
                              scale, max_cog_size, tmap_params) {
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # deal with color palette
    .check_palette(palette)
    # get the labels
    labels <- .cube_labels(tile)
    # obtain the colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # prepare labels
    labels <- tibble::rownames_to_column(
        as.data.frame(labels), var = "label_id"
    ) |>
        dplyr::mutate(label_id = as.numeric(.data[["label_id"]])) |>
        dplyr::rename("label" = "labels")
    # prepare colors
    colors <- tibble::rownames_to_column(
        as.data.frame(colors), var = "label"
    )
    # merge colors and labels
    colors <- dplyr::inner_join(colors, labels, by = "label") |>
                dplyr::rename("color" = "colors")
    # size of data to be read
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # select the image to be plotted
    class_file <- .tile_path(tile)
    # read file
    stars_obj <- stars::read_stars(
        class_file,
        RasterIO = list(
            nBufXSize = sizes[["xsize"]],
            nBufYSize = sizes[["ysize"]]
        ),
        proxy = FALSE
    )
    # rename stars object and set variables as factor
    stars_obj <- stats::setNames(stars_obj, "labels")
    stars_obj[["labels"]] <- factor(
        stars_obj[["labels"]],
        labels = colors[["label"]],
        levels = colors[["label_id"]]
    )
    # tmap params
    labels_size <- tmap_params[["graticules_labels_size"]]
    legend_bg_color <- tmap_params[["legend_bg_color"]]
    legend_bg_alpha <- tmap_params[["legend_bg_alpha"]]
    legend_title_size <- tmap_params[["legend_title_size"]]
    legend_text_size <- tmap_params[["legend_text_size"]]
    # plot using tmap
    p <- suppressMessages(
        tmap::tm_shape(stars_obj, raster.downsample = FALSE) +
            tmap::tm_raster(
                style = "cat",
                labels = colors[["label"]],
                palette = colors[["color"]]
            ) +
            tmap::tm_graticules(
                labels.size = labels_size
            ) +
            tmap::tm_compass() +
            tmap::tm_layout(
                scale = scale,
                legend.bg.color = legend_bg_color,
                legend.bg.alpha = legend_bg_alpha,
                legend.title.size = legend_title_size,
                legend.text.size = legend_text_size
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
#' @param  scale         Global scale for plot
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @return               A plot object
#'
.plot_probs <- function(tile,
                        labels_plot,
                        palette,
                        rev,
                        scale,
                        max_cog_size) {
    # set caller to show in errors
    .check_set_caller(".plot_probs")
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
    labels <- .tile_labels(tile)
    names(labels) <- seq_len(length(labels))
    # check the labels to be plotted
    # if NULL, use all labels
    if (.has_not(labels_plot)) {
        labels_plot <- labels
    } else {
        .check_that(all(labels_plot %in% labels))
    }
    # size of data to be read
    max_size <- .conf("plot", "max_size")
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # get the path
    probs_path <- .tile_path(tile)
    # read the file using stars
    probs_st <- stars::read_stars(
        probs_path,
        RasterIO = list(
            nBufXSize = sizes[["xsize"]],
            nBufYSize = sizes[["ysize"]]
        ),
        proxy = FALSE
    )
    # get the band
    band <- .tile_bands(tile)
    band_conf <- .tile_band_conf(tile, band)
    # scale the data
    probs_st <- probs_st * .scale(band_conf)

    # rename stars object dimensions to labels
    probs_st <- stars::st_set_dimensions(probs_st, "band", values = labels)
    # select stars bands to be plotted
    bds <- as.numeric(names(labels[labels %in% labels_plot]))

    labels_size <- as.numeric(.conf("plot", "graticules_labels_size"))
    legend_bg_color <- .conf("plot", "legend_bg_color")
    legend_bg_alpha <- as.numeric(.conf("plot", "legend_bg_alpha"))
    legend_title_size <- as.numeric(.conf("plot", "legend_title_size"))
    legend_text_size <- as.numeric(.conf("plot", "legend_text_size"))

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
            legend.text.size = legend_text_size
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
    labels <- .tile_labels(tile)
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
#' @title  Prepare tmap params for dots value
#' @name .plot_tmap_params
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
.plot_tmap_params <- function(dots){

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
