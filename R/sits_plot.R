#' @title  Plot time series
#' @method plot sits
#' @name plot
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.  See each function description for the
#' required parameters:
#' \itemize{
#'  \item{sits tibble: }          {see \code{\link{plot.sits}}}
#'  \item{patterns: }             {see \code{\link{plot.patterns}}}
#'  \item{SOM map: }              {see \code{\link{plot.som_map}}}
#'  \item{SOM evaluate cluster: } {see \code{\link{plot.som_evaluate_cluster}}}
#'  \item{classified time series: } {see \code{\link{plot.predicted}}}
#'  \item{raster cube: }         {see \code{\link{plot.raster_cube}}}
#'  \item{random forest model:} {see \code{\link{plot.rfor_model}}}
#'  \item{xgboost model:} {see \code{\link{plot.xgb_model}}}
#'  \item{torch ML model: } {see \code{\link{plot.torch_model}}}
#'  \item{classification probabilities: }{see \code{\link{plot.probs_cube}}}
#'  \item{model uncertainty: } {see \code{\link{plot.uncertainty_cube}}}
#'  \item{classified image: }     {see \code{\link{plot.classified_image}}}
#' }
#'
#' In the case of time series, the plot function produces different plots
#' based on the input data:
#' \itemize{
#'  \item{"all years": }{Plot all samples from the same location together}
#'  \item{"together": }{Plot all samples of the same band and label together}
#' }
#' The plot.sits function makes an educated guess of what plot is required,
#' based on the input data. If the input data has less than 30 samples, it
#' will default to "all years". If there are more than 30 samples,
#' it will default to "together".
#'
#' @param  x    Object of class "sits"
#' @param  y    Ignored.
#' @param ...   Further specifications for \link{plot}.
#'
#' @return A series of plot objects produced by ggplot2 showing all
#'   time series associated to each combination of band and label,
#'   and including the median, and first and third quartile ranges.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' if (sits_run_examples()) {
#' # plot sets of time series
#' plot(cerrado_2classes)
#' }
#'
#' @export
#'
plot.sits <- function(x, y, ...) {
    stopifnot(missing(y))

    # Are there more than 30 samples? Plot them together!
    if (nrow(x) > 30) {
        p <- .sits_plot_together(x)
    } # If no conditions are met, take "allyears" as the default
    else {
        p <- .sits_plot_allyears(x)
    }
    # return the plot
    return(invisible(p))
}

#' @title  Plot patterns that describe classes
#' @name   plot.patterns
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @description   Plots the patterns to be used for classification
#'
#' @description Given a sits tibble with a set of patterns, plot them.
#'
#' @param  x             Object of class "patterns".
#' @param  y             Ignored.
#' @param  ...           Further specifications for \link{plot}.
#' @return               A plot object produced by ggplot2
#'                       with one average pattern per label.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' This code is reused from the dtwSat package by Victor Maus.
#' @examples
#' if (sits_run_examples()) {
#'     # plot patterns
#'     plot(sits_patterns(cerrado_2classes))
#' }
#' @export
#'
plot.patterns <- function(x, y, ...) {
    stopifnot(missing(y))
    # verifies if scales package is installed
    .check_require_packages("scales")

    # put the time series in the data frame
    plot.df <- purrr::pmap_dfr(
        list(x$label, x$time_series),
        function(label, ts) {
            lb <- as.character(label)
            # extract the time series and convert
            df <- data.frame(Time = ts$Index, ts[-1], Pattern = lb)
            return(df)
        }
    )

    plot.df <- tidyr::pivot_longer(plot.df, cols = sits_bands(x))

    # Plot temporal patterns
    gp <- ggplot2::ggplot(plot.df, ggplot2::aes_string(
        x = "Time",
        y = "value",
        colour = "name"
    )) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~Pattern) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_x_date(labels = scales::date_format("%b")) +
        ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
        ggplot2::ylab("Value")

    p <- graphics::plot(gp)

    return(invisible(p))
}

#' @title  Plot time series predictions
#' @name   plot.predicted
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Given a sits tibble with a set of predictions, plot them
#'
#' @param  x             Object of class "predicted".
#' @param  y             Ignored.
#' @param  ...           Further specifications for \link{plot}.
#' @param  bands         Bands for visualization.
#' @param  palette       HCL palette used for visualization
#'                       in case classes are not in the default sits palette.
#' @return               A plot object produced by ggplot2
#'                       showing the time series and its label.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train a tempCNN model
#'     ml_model <- sits_train(samples_modis_4bands, ml_method = sits_tempcnn)
#'     # classify the point
#'     bands_model <- sits_bands(ml_model)
#'     point_4bands <- sits_select(point_mt_6bands, bands = bands_model)
#'     point_class <- sits_classify(point_4bands, ml_model)
#'     plot(point_class)
#' }
#' @export
#'
plot.predicted <- function(x, y, ...,
                           bands = "NDVI",
                           palette = "Harmonic") {
    stopifnot(missing(y))
    # verifies if scales package is installed
    .check_require_packages("scales")

    if (purrr::is_null(bands)) {
        bands <- sits_bands(x)
    }
    if (!all(bands %in% sits_bands(x))) {
        bands <- sits_bands(x)
    }
    # configure plot colors
    # get labels from predicted tibble
    labels <- unique(x$predicted[[1]]$class)
    colors <- .config_colors(
        labels = labels,
        palette = palette,
        rev = FALSE
    )

    # put the time series in the data frame
    p  <- purrr::pmap(
        list(
            x$latitude, x$longitude, x$label,
            x$time_series, x$predicted
        ),
        function(row_lat, row_long, row_label,
                 row_time_series, row_predicted) {
            lb <- .sits_plot_title(row_lat, row_long, row_label)
            # extract the time series
            ts <- row_time_series
            # convert to data frame
            df_x <- data.frame(
                Time = ts$Index, ts[, bands],
                Series = as.factor(lb)
            )
            # melt the time series data for plotting
            df_x <- tidyr::pivot_longer(df_x,
                                        cols = -c("Time", "Series"),
                                        names_to = "variable"
            )
            # define a nice set of breaks for value plotting
            y_labels <- scales::pretty_breaks()(range(df_x$value,
                                                      na.rm = TRUE
            ))
            y_breaks <- y_labels

            # create a data frame with values and intervals
            nrows_p <- nrow(row_predicted)
            df_pol <- purrr::pmap_dfr(
                list(
                    row_predicted$from, row_predicted$to,
                    row_predicted$class, seq(1:nrows_p)
                ),
                function(rp_from, rp_to, rp_class, i) {
                    best_class <- as.character(rp_class)

                    df_p <- data.frame(
                        Time = c(
                            lubridate::as_date(rp_from),
                            lubridate::as_date(rp_to),
                            lubridate::as_date(rp_to),
                            lubridate::as_date(rp_from)
                        ),
                        Group = rep(i, 4),
                        Class = rep(best_class, 4),
                        value = rep(range(y_breaks,
                                          na.rm = TRUE
                        ), each = 2)
                    )
                }
            )

            df_pol$Group <- factor(df_pol$Group)
            df_pol$Class <- factor(df_pol$Class)
            df_pol$Series <- rep(lb, length(df_pol$Time))

            I <- min(df_pol$Time, na.rm = TRUE) - 30 <= df_x$Time &
                df_x$Time <= max(df_pol$Time, na.rm = TRUE) + 30

            df_x <- df_x[I, , drop = FALSE]

            gp <- ggplot2::ggplot() +
                ggplot2::facet_wrap(~Series,
                                    scales = "free_x", ncol = 1
                ) +
                ggplot2::geom_polygon(
                    data = df_pol,
                    ggplot2::aes_string(
                        x = "Time",
                        y = "value",
                        group = "Group",
                        fill = "Class"
                    ),
                    alpha = .7
                ) +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::geom_line(
                    data = df_x,
                    ggplot2::aes_string(
                        x = "Time",
                        y = "value",
                        colour = "variable"
                    )
                ) +
                ggplot2::scale_color_brewer(palette = "Set1") +
                ggplot2::scale_y_continuous(
                    expand = c(0, 0),
                    breaks = y_breaks,
                    labels = y_labels
                ) +
                ggplot2::scale_x_date(
                    breaks = ggplot2::waiver(),
                    labels = ggplot2::waiver()
                ) +
                ggplot2::theme(legend.position = "bottom") +
                ggplot2::guides(
                    colour =
                        ggplot2::guide_legend(title = "Bands")
                ) +
                ggplot2::ylab("Value") +
                ggplot2::xlab("Time")

            g <- graphics::plot(gp)
            return(g)
        }
    )
    return(invisible(p))
}
#' @title  Plot RGB data cubes
#' @name plot.raster_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plot RGB raster cube
#'
#' @param  x             Object of class "raster_cube".
#' @param  ...           Further specifications for \link{plot}.
#' @param  band          Band for plotting grey images.
#' @param  red           Band for red color.
#' @param  green         Band for green color.
#' @param  blue          Band for blue color.
#' @param  tile          Tile to be plotted.
#' @param  date          Date to be plotted.
#'
#' @return               A plot object produced by the terra package
#'                       with an RGB or B/W image.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # plot NDVI band of the second date date of the data cube
#'     plot(cube, band = "NDVI", date = sits_timeline(cube)[2])
#' }
#' @export
plot.raster_cube <- function(x, ...,
                             band = NULL,
                             red = NULL,
                             green = NULL,
                             blue = NULL,
                             tile = x$tile[[1]],
                             date = NULL) {
    .check_chr_contains(
        x = x$tile,
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = "tile is not included in the cube"
    )

    # pre-condition 2
    .check_that(
        purrr::is_null(band) ||
            (purrr::is_null(red) &&
                purrr::is_null(green) &&
                purrr::is_null(blue)),
        local_msg = paste0(
            "either 'band' parameter or 'red', 'green', and",
            "'blue' parameters should be informed"
        )
    )

    # check if bands are valid
    if (!purrr::is_null(band)) {
        .check_chr_within(
            band,
            within = sits_bands(x),
            discriminator = "any_of",
            msg = "invalid band"
        )
        # plot as grayscale
        red <- band
        green <- band
        blue <- band
        r_index <- 1
        g_index <- 1
        b_index <- 1
    } else {
        .check_chr_within(
            c(red, green, blue),
            within = sits_bands(x),
            discriminator = "all_of",
            msg = "invalid RGB bands selection"
        )
        r_index <- 1
        g_index <- 2
        b_index <- 3
    }

    # select only one tile
    row <- dplyr::filter(x, .data[["tile"]] == !!tile)

    # if dates are not informed, show the first possible date
    if (purrr::is_null(date))
        date <- sits_timeline(row)[1]
    else {
        # use only one date
        date <- as.Date(date)
        .check_that(
            length(date) == 1,
            msg = "plot handles one date at a time"
        )
        # check if date is inside the timeline
        tile_dates <- sits_timeline(row)
        if (!date %in% tile_dates) {
            idx_date <- which.min(abs(date - tile_dates))
            date <- tile_dates[idx_date]
        }
    }
    # plot the selected tile
    # select the bands for the timeline
    bds_date <- dplyr::filter(.file_info(row), .data[["date"]] == !!date)
    # get RGB files for the requested timeline
    red_file <- dplyr::filter(bds_date, .data[["band"]] == red)$path
    green_file <- dplyr::filter(bds_date, .data[["band"]] == green)$path
    blue_file <- dplyr::filter(bds_date, .data[["band"]] == blue)$path
    # put the band on a raster/terra stack
    rgb_stack <- c(red_file, green_file, blue_file)

    # use the raster package to obtain a raster object from a stack
    r_obj <- .raster_open_stack.terra(rgb_stack)
    # plor the data using terra
    suppressWarnings(
        terra::plotRGB(r_obj,
            r = r_index,
            g = g_index,
            b = b_index,
            stretch = "hist"
        )
    )

    return(invisible(r_obj))
}
#' @title  Plot probability cubes
#' @name   plot.probs_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             Object of class "probs_image".
#' @param  ...           Further specifications for \link{plot}.
#' @param tiles          Tiles to be plotted.
#' @param labels         Labels to plot (optional).
#' @param breaks         Type of class intervals.
#' @param n_colors       Number of colors to plot.
#' @param palette        HCL palette used for visualization.
#' @return               A plot object produced by the stars package
#'                       containing maps of probabilities associated
#'                       to each class for each pixel.
#'
#' @note
#' \itemize{Possible class intervals
#'  \item{"sd":} {intervals based on the average and standard deviation.}
#'  \item{"equal": } {divides the range of the variable into n parts.}
#'  \item{"pretty": } {number of breaks likely to be legible.}
#'  \item{"quantile": } {quantile breaks}
#'  \item{"log": }{logarithm plot}
#'  }
#'
#' @note
#' The function accepts color palettes are defined in grDevices::hcl.pals()
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # plot the resulting probability cube
#'     plot(probs_cube)
#' }
#'
#' @export
#'
plot.probs_cube <- function(x, ...,
                            tiles = NULL,
                            labels = NULL,
                            breaks = "pretty",
                            n_colors = 20,
                            palette = "Terrain") {
    # verifies if stars package is installed
    .check_require_packages("stars")

    # precondition - check breaks parameter
    .check_chr_within(
        x = breaks,
        within = .config_get("class_intervals"),
        discriminator = "any_of",
        msg = "invalid class interval"
    )
    # precondition - check palette
    .check_chr_within(
        x = palette,
        within = grDevices::hcl.pals(),
        discriminator = "any_of",
        msg = "invalid color palette"
    )
    # precondition
    if (purrr::is_null(tiles)) {
        tiles <- x$tile[[1]]
    } else {
        .check_chr_contains(
            x = x$tile,
            contains = tiles,
            case_sensitive = FALSE,
            discriminator = "all_of",
            can_repeat = FALSE,
            msg = "tiles are not included in the cube"
        )
    }
    # filter the cube
    x <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    # define the number of colors
    n_breaks <- n_colors + 1
    # define the output color palette
    col <- grDevices::hcl.colors(
        n = n_colors,
        palette = palette,
        alpha = 1,
        rev = TRUE
    )


    # read the paths to plot
    paths <- slider::slide_chr(x, function(row) {
        return(.file_info_path(row))
    })
    if (length(paths) == 1) {
        stars_mosaic <- stars::read_stars(paths[[1]])
    } else {
        stars_lst <- purrr::map(paths, function(path) {
            stars::read_stars(path)
        })
        stars_mosaic <- stars::st_mosaic(
            stars_lst[[1]],
            stars_lst[[2:length(stars_lst)]]
        )
    }
    # get the labels
    labels_cube <- sits_labels(x)

    # resize to the [0..1] interval
    stars_mosaic <- stars_mosaic * .config_get("raster_cube_scale_factor")
    # verify if label is not NULL
    if (!purrr::is_null(labels)) {
        # label is not null, then plot only the label
        layers <- match(labels, labels_cube)

        out <- utils::capture.output({
            p <- stars_mosaic %>%
                dplyr::slice(index = layers, along = "band") %>%
                plot(
                    breaks = breaks,
                    nbreaks = n_breaks,
                    col = col,
                    main = labels
                )
        })
    } else {
        out <- utils::capture.output({
            p <- plot(stars_mosaic,
                breaks = breaks,
                nbreaks = n_breaks,
                col = col,
                main = labels_cube
            )
        })
    }
    return(invisible(p))
}



#' @title  Plot uncertainty cubes
#' @name   plot.uncertainty_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             Object of class "probs_image".
#' @param  ...           Further specifications for \link{plot}.
#' @param tiles          Tiles to be plotted.
#' @param n_colors       Number of colors to plot.
#' @param intervals      Type of class intervals.
#' @param palette        HCL palette used for visualization.
#'
#' @return               A plot object produced by the stars package
#'                       with a map showing the uncertainty associated
#'                       to each classified pixel.
#'
#' @note
#' \itemize{Possible class intervals
#'  \item{"sd":} {intervals based on the average and standard deviation.}
#'  \item{"equal": } {divides the range of the variable into n parts.}
#'  \item{"quantile": } {quantile breaks}
#'  \item{"pretty": } {number of breaks likely to be legible.}
#'  \item{"log" :} {logarithm plot.}
#'  }
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # calculate uncertainty
#'     uncert_cube <- sits_uncertainty(probs_cube)
#'     # plot the resulting uncertainty cube
#'     plot(uncert_cube)
#' }
#' @export
#'
plot.uncertainty_cube <- function(x, ...,
                                  tiles = NULL,
                                  n_colors = 14,
                                  intervals = "log",
                                  palette = "YlOrRd") {

    # verifies if stars package is installed
    .check_require_packages("stars")
    # precondition - check breaks parameter
    .check_chr_within(
        x = intervals,
        within = .config_get("class_intervals"),
        discriminator = "any_of",
        msg = "invalid class interval"
    )
    # precondition - check palette
    .check_chr_within(
        x = palette,
        within = grDevices::hcl.pals(),
        discriminator = "any_of",
        msg = "invalid color palette"
    )
    # precondition
    if (purrr::is_null(tiles)) {
        tiles <- x$tile[[1]]
    } else {
        .check_chr_contains(
            x = x$tile,
            contains = tiles,
            case_sensitive = FALSE,
            discriminator = "all_of",
            can_repeat = FALSE,
            msg = "tiles are not included in the cube"
        )
    }
    # filter the cube
    x <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    # define the number of colors
    n_breaks <- n_colors + 1
    # define the output color palette
    col <- grDevices::hcl.colors(
        n = n_colors,
        palette = palette,
        alpha = 1,
        rev = TRUE
    )
    if (intervals == "log") {
        breaks <- as.integer(
            1e+04 * (log(c(1:n_breaks))^1.6) / (log(n_breaks)^1.6)
        )
    } else {
        breaks <- intervals
    }

    # read the paths to plot
    paths <- slider::slide_chr(x, function(row) {
        return(.file_info_path(row))
    })
    if (length(paths) == 1) {
        stars_mosaic <- stars::read_stars(paths[[1]])
    } else {
        stars_lst <- purrr::map(paths, function(path) {
            stars::read_stars(path)
        })
        stars_mosaic <- stars::st_mosaic(
            stars_lst[[1]],
            stars_lst[[2:length(stars_lst)]]
        )
    }

    p <- suppressMessages(plot(stars_mosaic,
        breaks = breaks,
        nbreaks = n_breaks,
        col = col,
        main = "Uncertainty"
    ))
    return(invisible(p))
}

#' @title  Plot classified images
#' @name   plot.classified_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified raster using ggplot.
#'
#' @param  x               Object of class "classified_image".
#' @param  y               Ignored.
#' @param  ...             Further specifications for \link{plot}.
#' @param  tiles           Tiles to be plotted.
#' @param  title           Title of the plot.
#' @param  legend          Named vector that associates labels to colors.
#' @param  palette         Alternative palette that uses grDevices::hcl.pals().
#' @param  rev             Invert the order of hcl palette?
#'
#' @return                 A plot object produced by the ggplot2 package
#'                         with a color maps, where each pixel has the color
#'                         associated to a label, as defined by the legend
#'                         parameter.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # label cube with the most likely class
#'     label_cube <- sits_label_classification(probs_cube)
#'     # plot the resulting classified image
#'     plot(label_cube)
#' }
#' @export
#'
plot.classified_image <- function(x, y, ...,
                                  tiles = NULL,
                                  title = "Classified Image",
                                  legend = NULL,
                                  palette = "Spectral",
                                  rev = TRUE) {
    stopifnot(missing(y))
    # set caller to show in errors
    .check_set_caller(".sits_plot_classified_image")

    # precondition - cube must be a labelled cube
    cube <- x
    .check_chr_within(
        x = "classified_image",
        within = class(cube),
        discriminator = "any_of",
        msg = "cube must be a classified image"
    )

    # precondition - check palette
    .check_chr_within(
        x = palette,
        within = grDevices::hcl.pals(),
        discriminator = "any_of",
        msg = "invalid color palette"
    )

    # precondition
    if (purrr::is_null(tiles)) {
        tiles <- cube$tile[[1]]
    } else {
        .check_chr_contains(
            x = cube$tile,
            contains = tiles,
            case_sensitive = FALSE,
            discriminator = "all_of",
            can_repeat = FALSE,
            msg = "tiles are not included in the cube"
        )
    }
    # select only one tile
    cube <- dplyr::filter(cube, .data[["tile"]] %in% tiles)

    r_objs <- slider::slide(cube, function(row) {
        # get the raster object
        r <- suppressWarnings(terra::rast(.file_info_path(row)))
        return(r)
    })

    # merge two or more raster objects
    if (length(r_objs) == 1) {
        r_merge <- r_objs[[1]]
    } else {
        raster_collection <- terra::sprc(r_objs)
        r_merge <- terra::merge(raster_collection)
    }
    # compress the image
    max_Mbytes <- .config_get("plot_max_Mbytes")

    # find out of image needs to be resampled
    size <- .sits_plot_resample_class(
        terra::nrow(r_merge),
        terra::ncol(r_merge),
        max_Mbytes
    )
    # resample image
    if (as.numeric(size[["ratio"]] > 1)) {
        new_nrows <- as.integer(size[["nrows"]])
        new_ncols <- as.integer(size[["ncols"]])
        new_rast <- terra::rast(nrows = new_nrows,
                                ncols = new_ncols,
                                xmin = terra::xmin(r_merge),
                                xmax = terra::xmax(r_merge),
                                ymin = terra::ymin(r_merge),
                                ymax = terra::ymax(r_merge),
                                crs  = terra::crs(r_merge)
        )
        r_merge <- terra::resample(r_merge, new_rast, method = "near")
    }
    # convert from raster to points
    df <- terra::as.data.frame(r_merge, xy = TRUE)
    # define the column names for the data frame
    colnames(df) <- c("x", "y", "class")

    # get the labels and how many there are
    labels <- sits_labels(cube)
    nclasses <- length(labels)
    # create a mapping from classes to labels
    names(labels) <- as.character(c(1:nclasses))

    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .config_colors(
            labels = labels,
            palette = palette,
            rev = rev
        )
    } else {
        .check_chr_within(
            x = labels,
            within = names(legend),
            discriminator = "all_of",
            msg = "some labels are missing from the legend"
        )

        colors <- unname(legend[labels])
    }
    # set the names of the color vector
    # names(colors) <- as.character(c(1:nclasses))
    fill_colors <- unname(colors[labels])

    # plot the data with ggplot
    g <- ggplot2::ggplot(df, ggplot2::aes(.data[["x"]], .data[["y"]])) +
        ggplot2::geom_raster(ggplot2::aes(fill = factor(class))) +
        ggplot2::labs(title = title) +
        ggplot2::scale_fill_manual(
            values = fill_colors,
            labels = labels,
            guide = ggplot2::guide_legend(
                title = "Classes"
            )
        )

    graphics::plot(g)
    return(invisible(g))
}
#' @title  Plot Random Forest  model
#' @name   plot.rfor_model
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plots the important variables in a random forest model.
#'
#'
#' @param  x             Object of class "rf_model".
#' @param  y             Ignored.
#' @param  ...           Further specifications for \link{plot}.
#' @return               A random forest object.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_4bands,  ml_method = sits_rfor())
#'     # plot the model
#'     plot(rf_model)
#' }
#' @export
#'
plot.rfor_model <- function(x, y, ...){
    # verifies if randomForestExplainer package is installed
    .check_require_packages("randomForestExplainer")
    # retrieve the random forest object from the enviroment
    rf <- environment(x)$result_rfor
    p <- randomForestExplainer::plot_min_depth_distribution(rf)
    return(p)
}



#'
#' @title  Plot confusion between clusters
#' @name   plot.som_evaluate_cluster
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#'
#' @description Plot a bar graph with informations about each cluster.
#' The percentage of mixture between the clusters.
#'
#' @param  x            Object of class "plot.som_evaluate_cluster".
#' @param  y            Ignored.
#' @param  ...          Further specifications for \link{plot}.
#' @param  name_cluster Choose the cluster to plot.
#' @param  title        Title of plot.
#' @return              A plot object produced by the ggplot2 package
#'                      containing color bars showing the confusion
#'                      between classes.
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # create a SOM map
#'     som_map <- sits_som_map(samples_modis_4bands)
#'     # evaluate the SOM cluster
#'     som_clusters <- sits_som_evaluate_cluster(som_map)
#'     # plot the SOM cluster evaluation
#'     plot(som_clusters)
#' }
#' @export
#'
plot.som_evaluate_cluster <- function(x, y, ...,
                                      name_cluster = NULL,
                                      title = "Confusion by cluster") {
    stopifnot(missing(y))
    data <- x
    if (!inherits(data, "som_evaluate_cluster")) {
        message("unable to plot - please run sits_som_evaluate_cluster")
        return(invisible(NULL))
    }

    # Filter the cluster to plot
    if (!(is.null(name_cluster))) {
        data <- dplyr::filter(data, .data[["cluster"]] %in% name_cluster)
    }
    # configure plot colors
    # get labels from cluster table
    labels <- unique(data$class)
    colors <- .config_colors(
        labels = labels,
        palette = "Spectral",
        rev = TRUE
    )

    p <- ggplot2::ggplot() +
        ggplot2::geom_bar(
            ggplot2::aes(
                y = .data[["mixture_percentage"]],
                x = .data[["cluster"]],
                fill = class
            ),
            data = data,
            stat = "identity",
            position = ggplot2::position_dodge()
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.x =
                ggplot2::element_text(angle = 60, hjust = 1)
        ) +
        ggplot2::labs(x = "Cluster", y = "Percentage of mixture") +
        ggplot2::scale_fill_manual(name = "Class label", values = colors) +
        ggplot2::ggtitle(title)

    p <- graphics::plot(p)
    return(invisible(p))
}
#' @title  Plot a SOM map
#' @name   plot.som_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a SOM map generated by "sits_som_map"
#' The plot function produces different plots based on the input data:
#' \itemize{
#'  \item{"codes": }{Plot the vector weight for in each neuron.}
#'  \item{"mapping": }{Shows where samples are mapped.}
#' }
#'
#' @param  x          Object of class "som_map".
#' @param  y          Ignored.
#' @param  ...        Further specifications for \link{plot}.
#' @param  type       Type of plot: "codes" for neuron weight (time series) and
#'                    "mapping" for the number of samples allocated in a neuron.
#' @param  band       What band will be plotted.
#'
#' @return            No return value, called for side effects.
#'
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # create a SOM map
#'     som_map <- sits_som_map(samples_modis_4bands)
#'     # plot the SOM map
#'     plot(som_map)
#' }
#' @export
#'
plot.som_map <- function(x, y, ..., type = "codes", band = 1) {
    stopifnot(missing(y))
    koh <- x
    if (!inherits(koh, "som_map")) {
        message("wrong input data; please run sits_som_map first")
        return(invisible(NULL))
    }
    if (type == "mapping") {
        graphics::plot(koh$som_properties,
                       bgcol = koh$som_properties$paint_map,
                       "mapping", whatmap = band,
                       codeRendering = "lines"
        )
    } else if (type == "codes") {
        graphics::plot(koh$som_properties,
                       bgcol = koh$som_properties$paint_map,
                       "codes", whatmap = band,
                       codeRendering = "lines"
        )
    }

    # create a legend
    leg <- cbind(koh$som_properties$neuron_label, koh$som_properties$paint_map)
    graphics::legend(
        "bottomright",
        legend = unique(leg[, 1]),
        col = unique(leg[, 2]),
        pch = 15,
        pt.cex = 2,
        cex = 1,
        text.col = "black",
        # horiz = T ,
        inset = c(0.0095, 0.05),
        xpd = TRUE,
        ncol = 1
    )
}
#' @title  Plot XGB model
#' @name   plot.xgb_model
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plots the important variables in an extreme gradient boosting.
#'
#'
#' @param  x             Object of class "xgb_model".
#' @param  ...           Further specifications for \link{plot}.
#' @param  n_trees       Number of trees to be plotted
#' @return               A plot object.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train an extreme gradient boosting
#'     xgb_model <- sits_train(samples_modis_4bands,
#'            ml_method = sits_xgboost())
#'     # plot the model
#'     plot(xgb_model)
#' }
#' @export
#'
plot.xgb_model <- function(x, ..., n_trees = 3){
    # verifies if DiagrammeR package is installed
    .check_require_packages("DiagrammeR")
    # retrieve the XGB object from the enviroment
    xgb <- environment(x)$model_xgb
    # plot the trees
    p <- xgboost::xgb.plot.tree(model = xgb, trees = seq_len(n_trees) - 1)
    return(p)
}
#' @title  Plot Torch (deep learning) model
#' @name   plot.torch_model
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Plots a deep learning model developed using torch.
#'
#' @note This code has been lifted from the "keras" package.
#'
#' @param  x             Object of class "torch_model".
#' @param  y             Ignored.
#' @param  ...           Further specifications for \link{plot}.
#' @return               A plot object produced by the ggplot2 package
#'                       showing the evolution of the loss and
#'                       accuracy of the model.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train a tempCNN model
#'     ml_model <- sits_train(samples_modis_4bands, ml_method = sits_tempcnn)
#'     # plot the model
#'     plot(ml_model)
#' }
#' @export
#'
plot.torch_model <- function(x, y, ...) {
    stopifnot(missing(y))

    model <- x
    # set the model variables to be plotted
    model_vars <- c("records", "metrics")
    # retrieve the model variables from the environment
    metrics_lst <- environment(model)[["torch_model"]][[model_vars]]

    metrics_dfr <- purrr::map_dfr(names(metrics_lst), function(name) {
        met <- metrics_lst[[name]]

        purrr::map_dfr(met, tibble::as_tibble_row) %>%
            dplyr::mutate(epoch = seq_len(dplyr::n()), data = name) %>%
            tidyr::pivot_longer(cols = 1:2, names_to = "metric")
    })

    p <- ggplot2::ggplot(metrics_dfr, ggplot2::aes(
        x = .data[["epoch"]],
        y = .data[["value"]],
        color = .data[["data"]],
        fill = .data[["data"]]
    ))

    p <- p + ggplot2::geom_point(shape = 21, col = 1, na.rm = TRUE, size = 2) +
        ggplot2::geom_smooth(
            formula = y ~ x,
            se      = FALSE,
            method  = "loess",
            na.rm   = TRUE
        )

    p <- p + ggplot2::facet_grid(metric ~ ., switch = "y", scales = "free_y") +
        ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            strip.placement = "outside",
            strip.text = ggplot2::element_text(
                colour = "black",
                size   = 11
            ),
            strip.background = ggplot2::element_rect(
                fill  = NA,
                color = NA
            )
        )

    p <- p + ggplot2::labs()

    return(p)
}

#' @title Plot all intervals of one time series for the same lat/long together
#' @name .sits_plot_allyears
#' @keywords internal
#'
#' @description For each lat/long location in the data, join temporal
#' instances of the same place together for plotting.
#' @param data    One or more time series.
#' @return        A plot object produced by the ggplot2 package
#'                showing an individual time series.
#'
.sits_plot_allyears <- function(data) {
    locs <- dplyr::distinct(data, .data[["longitude"]], .data[["latitude"]])

    plots <- purrr::pmap(
        list(locs$longitude, locs$latitude),
        function(long, lat) {
            dplyr::filter(
                data,
                .data[["longitude"]] == long,
                .data[["latitude"]] == lat
            ) %>%
                .sits_plot_ggplot_series() %>%
                graphics::plot()
        }
    )
    return(invisible(plots[[1]]))
}

#' @title Plot a set of time series for the same spatiotemporal reference
#'
#' @name .sits_plot_together
#' @keywords internal
#'
#' @description Plots all time series for the same label together.
#' This function is useful to find out the spread of the values of
#' the time series for a given label.
#'
#' @param    data    A sits tibble with the list of time series to be plotted.
#' @return           A set of plots produced by the ggplot2 package
#'                   each containing all time series associated to one band
#'                   and one label.
.sits_plot_together <- function(data) {
    Index <- NULL # to avoid setting global variable
    # create a data frame with the median, and 25% and 75% quantiles
    create_iqr <- function(dt, band) {
        V1 <- NULL # to avoid setting global variable

        data.table::setnames(dt, band, "V1")
        dt_med <- dt[, stats::median(V1), by = Index]
        data.table::setnames(dt_med, "V1", "med")
        dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
        data.table::setnames(dt_qt25, "V1", "qt25")
        dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
        data.table::setnames(dt_qt75, "V1", "qt75")
        dt_qts <- merge(dt_med, dt_qt25)
        dt_qts <- merge(dt_qts, dt_qt75)
        data.table::setnames(dt, "V1", band)
        return(dt_qts)
    }
    # this function plots the values of all time series together (for one band)
    plot_samples <- function(dt, dt_qts, band, label, number) {
        # melt the data into long format (required for ggplot to work)
        dt_melted <- data.table::melt(dt, id.vars = "Index")
        # make the plot title
        title <- paste("Samples (", number, ") for class ",
            label, " in band = ", band,
            sep = ""
        )
        # plot all data together
        g <- .sits_plot_ggplot_together(dt_melted, dt_qts, title)
        p <- graphics::plot(g)
        return(p)
    }

    # how many different labels are there?
    labels <- sits_labels(data)

    label_plots <- labels %>%
        purrr::map(function(l) {
            lb <- as.character(l)
            # filter only those rows with the same label
            data2 <- dplyr::filter(data, .data[["label"]] == lb)
            # how many time series are to be plotted?
            number <- nrow(data2)
            # what are the band names?
            bands <- sits_bands(data2)
            # what are the reference dates?
            ref_dates <- sits_timeline(data2)
            # align all time series to the same dates
            data2 <- .sits_tibble_align_dates(data2, ref_dates)

            band_plots <- bands %>%
                purrr::map(function(band) {
                    # select the band to be shown
                    band_tb <- sits_select(data2, band)
                    # create a list with all time series for this band
                    dt_lst <- purrr::map(
                        band_tb$time_series,
                        function(ts) {
                            data.table::data.table(ts)
                        }
                    )
                    # set "Index" as the key for all data.tables in the list
                    dt_lst <- purrr::map(
                        dt_lst,
                        function(dt) {
                            data.table::setkey(dt, Index)
                        }
                    )
                    # rename the columns of the data table prior to merging
                    length_dt <- length(dt_lst)
                    dt_lst <- purrr::map2(
                        dt_lst, 1:length_dt,
                        function(dt, i) {
                            data.table::setnames(
                                dt, band,
                                paste0(band, ".", as.character(i))
                            )
                        }
                    )
                    # merge the list of data.tables into a single table
                    dt <- Reduce(function(...) merge(..., all = TRUE), dt_lst)

                    # create another data.table with all the rows together
                    # (required to compute the median and quartile values)
                    ts <- band_tb$time_series
                    dt_byrows <- data.table::data.table(dplyr::bind_rows(ts))
                    # compute the median and quartile values
                    dt_qts <- create_iqr(dt_byrows, band)
                    # plot the time series together
                    # (highlighting the median and quartiles 25% and 75%)
                    p <- plot_samples(dt, dt_qts, band, lb, number)
                    return(p)
                })
            return(band_plots)
        })
    return(invisible(label_plots[[1]][[1]]))
}

#' @title Plot one time series using ggplot
#'
#' @name .sits_plot_ggplot_series
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot. This function is used
#' for showing the same lat/long location in a series of time steps.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            A plot object produced by the ggplot2 package showing
#'                    one time series.
.sits_plot_ggplot_series <- function(row) {
    # Are there NAs in the data?
    if (any(is.na(row$time_series[[1]]))) {
        g <- .sits_plot_ggplot_series_na(row)
    } else {
        g <- .sits_plot_ggplot_series_no_na(row)
    }
    return(g)
}
#' @title Plot one time series using ggplot (no NAs present)
#'
#' @name .sits_plot_ggplot_series_no_na
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot in the case the series
#'              has no NA values.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            A plot object produced by the ggplot2 package where the
#'                    the time series has no NA values.
#'
.sits_plot_ggplot_series_no_na <- function(row) {
    # create the plot title
    plot_title <- .sits_plot_title(row$latitude, row$longitude, row$label)
    #
    colors <- grDevices::hcl.colors(
        n = 20,
        palette = "Harmonic",
        alpha = 1,
        rev = TRUE
    )
    # extract the time series
    data_ts <- dplyr::bind_rows(row$time_series)
    # melt the data into long format
    melted_ts <- data_ts %>%
        tidyr::pivot_longer(cols = -.data[["Index"]], names_to = "variable") %>%
        as.data.frame()
    # plot the data with ggplot
    g <- ggplot2::ggplot(melted_ts, ggplot2::aes(
        x = .data[["Index"]],
        y = .data[["value"]],
        group = .data[["variable"]]
    )) +
        ggplot2::geom_line(ggplot2::aes(color = .data[["variable"]])) +
        ggplot2::labs(title = plot_title) +
        ggplot2::scale_fill_manual(palette = colors)
    return(g)
}
#' @title Plot one time series with NAs using ggplot
#'
#' @name .sits_plot_ggplot_series_na
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot, showing where NAs are.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            A plot object produced by the ggplot2 package
#'                    which shows the NA values of a time series.
.sits_plot_ggplot_series_na <- function(row) {

    # verifies if tidyr package is installed
    .check_require_packages("tidyr")

    # define a function to replace the NAs for unique values
    replace_na <- function(x) {
        x[is.na(x)] <- -10000
        x[x != -10000] <- NA
        x[x == -10000] <- 1
        return(x)
    }
    # create the plot title
    plot_title <- .sits_plot_title(row$latitude, row$longitude, row$label)

    # include a new band in the data to show the NAs
    data <- row$time_series[[1]]
    data <- data %>%
        dplyr::select_if(function(x) any(is.na(x))) %>%
        .[, 1] %>%
        `colnames<-`(., "X1") %>%
        dplyr::transmute(cld = replace_na(.data[["X1"]])) %>%
        dplyr::bind_cols(data, .)

    # prepare tibble to ggplot (fortify)
    ts1 <- tidyr::pivot_longer(data, -.data[["Index"]])
    g <- ggplot2::ggplot(data = ts1 %>%
        dplyr::filter(.data[["name"]] != "cld")) +
        ggplot2::geom_col(ggplot2::aes(
            x = .data[["Index"]],
            y = .data[["value"]]
        ),
        fill = "sienna",
        alpha = 0.3,
        data = ts1 %>%
            dplyr::filter(
                .data[["name"]] == "cld",
                !is.na(.data[["value"]])
            )
        ) +
        ggplot2::geom_line(ggplot2::aes(
            x = .data[["Index"]],
            y = .data[["value"]],
            color = .data[["name"]]
        )) +
        ggplot2::geom_point(ggplot2::aes(
            x = .data[["Index"]],
            y = .data[["value"]],
            color = .data[["name"]]
        )) +
        ggplot2::labs(title = plot_title)

    return(g)
}

#' @title Plot many time series together using ggplot
#'
#' @name .sits_plot_ggplot_together
#' @keywords internal
#'
#' @description Plots a set of  time series together.
#'
#' @param melted         tibble with the time series (already melted).
#' @param means          means and std deviations of the time series.
#' @param plot_title     title for the plot.
#' @return               A plot object produced by the ggplot2 package
#'                       each time series associated to one band
#'                       and one label.
#'
.sits_plot_ggplot_together <- function(melted, means, plot_title) {
    g <- ggplot2::ggplot(data = melted, ggplot2::aes(
        x = .data[["Index"]],
        y = .data[["value"]],
        group = .data[["variable"]]
    )) +
        ggplot2::geom_line(colour = "#819BB1", alpha = 0.5) +
        ggplot2::labs(title = plot_title) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = .data[["Index"]], y = .data[["med"]]),
            colour = "#B16240", size = 2, inherit.aes = FALSE
        ) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = .data[["Index"]], y = .data[["qt25"]]),
            colour = "#B19540", size = 1, inherit.aes = FALSE
        ) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = .data[["Index"]], y = .data[["qt75"]]),
            colour = "#B19540", size = 1, inherit.aes = FALSE
        )
    return(g)
}

#' @title Create a plot title to use with ggplot
#' @name .sits_plot_title
#' @keywords internal
#'
#' @description Creates a plot title from row information.
#'
#' @param latitude   latitude of the location to be plotted.
#' @param longitude  longitude of the location to be plotted.
#' @param label      label of the location to be plotted.
#' @return           title to be used in the plot.
.sits_plot_title <- function(latitude, longitude, label) {
    title <- paste("location (",
        signif(latitude, digits = 4), ", ",
        signif(longitude, digits = 4), ") - ",
        label,
        sep = ""
    )
    return(title)
}

#' @title Plot a dendrogram
#' @name .sits_plot_dendrogram
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Plot a dendrogram
#'
#' @param data          sits tibble with data used to extract the dendrogram.
#' @param cluster       cluster object produced by `sits_cluster` function.
#' @param cutree_height dashed horizontal line to be drawn
#'                      indicating the height of dendrogram cutting.
#' @param palette       hcl color palette.
#'
#' @return              The dendrogram object.
.sits_plot_dendrogram <- function(data,
                                  cluster,
                                  cutree_height,
                                  palette) {

    # set caller to show in errors
    .check_set_caller(".sits_plot_dendrogram")

    # verifies if dendextend and methods packages is installed
    .check_require_packages(
        c("dendextend", "methods"),
        msg = "please install package(s)"
    )

    # ensures that a cluster object  exists
    .check_null(
        x = cluster,
        msg = "no valid cluster object available"
    )
    # get data labels
    data_labels <- data$label

    # extract the dendrogram object
    hclust_cl <- methods::S3Part(cluster, strictS3 = TRUE)
    dend <- hclust_cl %>% stats::as.dendrogram()

    # colors vector
    colors <- .config_colors(
        labels = data_labels,
        palette = palette,
        rev = TRUE
    )
    colors_clust <- colors[data_labels]

    # set the visualization params for dendrogram
    dend <- dend %>%
        dendextend::set(
            what = "labels",
            value = character(length = length(data_labels))
        ) %>%
        dendextend::set(
            what = "branches_k_color",
            value = colors_clust,
            k = length(data_labels)
        )

    p <- graphics::plot(dend,
        ylab = paste(
            tools::file_path_sans_ext(cluster@method),
            "linkage distance"
        )
    )
    # plot cutree line
    if (!purrr::is_null(cutree_height)) {
        graphics::abline(h = cutree_height, lty = 2)
    }

    # plot legend
    graphics::legend("topright",
        fill = colors,
        legend = sits_labels(data)
    )
    return(invisible(dend))
}

#' @title Calculate resample params for classified images
#' @name .sits_plot_resample_class
#' @keywords internal
#' @param nrows    number of rows in the input image
#' @param ncols    number of cols in the input image
#' @param max_Mbytes maximum number of MB per plot
#' @return         ratio and new size of output plot
.sits_plot_resample_class <- function(nrows, ncols, max_Mbytes) {

    # input size
    in_size_Mbytes <- (nrows * ncols) / (1000 * 1000)
    # do we need to compress?
    ratio <- max((in_size_Mbytes / max_Mbytes), 1)

    # only create local files if required
    if (ratio > 1) {
        new_nrows <- round(nrows / sqrt(ratio))
        new_ncols <- round(ncols * (new_nrows / nrows))
    } else {
        new_nrows <- nrows
        new_ncols <- ncols
    }
    return(c("ratio" = ratio, "nrows" = new_nrows, "ncols" = new_ncols))
}

#' @title Plot classification alignments using the dtwSat package
#' @name .sits_plot_twdtw_alignments
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Plots the alignments from TWDTW classification
#'
#' @param matches   A list of dtwSat S4 match objects produced by
#'   sits_TWDTW_matches.
#'
#' @return Return the same input value.
#'
.sits_plot_twdtw_alignments <- function(matches) {
    # verifies if dtwSat package is installed
    if (!requireNamespace("dtwSat", quietly = TRUE)) {
        stop("Please install package dtwSat", call. = FALSE)
    }

    matches %>%
        purrr::map(function(m) {
            dtwSat::plot(m, type = "alignments") %>%
                graphics::plot()
        })
    return(invisible(matches))
}

#' @title Plot classification results using the dtwSat package
#' @name .sits_plot_twdtw_class
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description         Plots the results of TWDTW classification (uses dtwSat).
#'
#' @param  matches      dtwSat S4 objects produced by sits_TWDTW_matches.
#' @param  start_date   Start date of the plot (used for classifications).
#' @param  end_date     End date of the plot (used for classifications).
#' @param  interval     Interval between classifications.
#' @param  overlap      Minimum overlapping between one match and the
#'   interval of classification. For details see dtwSat::twdtwApply help.
#' @return Return the same input value.
#'
.sits_plot_twdtw_class <- function(matches,
                                   start_date = NULL,
                                   end_date = NULL,
                                   interval = "12 month",
                                   overlap = 0.5) {
    # verifies if dtwSat package is installed
    .check_require_packages("dtwSat")

    matches %>%
        purrr::map(function(m) {
            if (purrr::is_null(start_date) | purrr::is_null(end_date)) {
                dplot <- dtwSat::plot(m,
                    type = "classification",
                    overlap = 0.5
                )
            } else {
                dplot <- dtwSat::plot(m,
                    type = "classification",
                    from = start_date,
                    to = end_date,
                    by = interval,
                    overlap = overlap
                )
            }
            graphics::plot(dplot)
        })
    return(invisible(matches))
}





#' @title Make a kernel density plot of samples distances.
#'
#' @name   plot.geo_distances
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Make a kernel density plot of samples distances.
#'
#' @param  x             Object of class "geo_distances".
#' @param  y             Ignored.
#' @param  ...           Further specifications for \link{plot}.
#' @return               A plot showing the sample-to-sample distances
#'                       and sample-to-prediction distances.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @references Hanna Meyer and Edzer Pebesma,
#' "Machine learning-based global maps of ecological variables and the
#' challenge of assessing them" Nature Communications, 13,2022.
#' DOI: 10.1038/s41467-022-29838-9.
#' @examples
#' if (sits_run_examples()) {
#'     # read a shapefile for the state of Mato Grosso, Brazil
#'     mt_shp <- system.file("extdata/shapefiles/mato_grosso/mt.shp",
#'         package = "sits"
#'     )
#'     # convert to an sf object
#'     mt_sf <- sf::read_sf(mt_shp)
#'     # calculate sample-to-sample and sample-to-prediction distances
#'     distances <- sits_geo_dist(samples_modis_4bands, mt_sf)
#'     # plot sample-to-sample and sample-to-prediction distances
#'     plot(distances)
#' }
#' @export
#'
plot.geo_distances <- function(x, y, ...) {
    distances <- x
    .check_that(
        inherits(distances, "geo_distances"),
        "Invalid distances object. Use sits_geo_dist to create it."
    )

    density_plot <-
        distances %>%
        dplyr::mutate(distance = .data[["distance"]] / 1000) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data[["distance"]])) +
        ggplot2::geom_density(ggplot2::aes(
            color = .data[["type"]],
            fill = .data[["type"]]
        ),
        lwd = 1, alpha = 0.25
        ) +
        ggplot2::scale_x_log10(labels = scales::label_number()) +
        ggplot2::xlab("Distance (km)") +
        ggplot2::ylab("") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::ggtitle("Distribution of Nearest Neighbor Distances")
    return(density_plot)
}
