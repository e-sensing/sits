#' @title  Plot time series
#' @method plot sits
#' @name plot
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.  See each function description for the
#' required parameters.
#' \itemize{
#' \item sits tibble: see \code{\link{plot.sits}}
#' \item patterns: see \code{\link{plot.patterns}}
#' \item classified time series: see \code{\link{plot.predicted}}
#' \item raster cube: see \code{\link{plot.raster_cube}}
#' \item SAR cube: see \code{\link{plot.sar_cube}}
#' \item DEM cube: see \code{\link{plot.dem_cube}}
#' \item vector cube: see \code{\link{plot.vector_cube}}
#' \item classification probabilities: see \code{\link{plot.probs_cube}}
#' \item classification uncertainty: see \code{\link{plot.uncertainty_cube}}
#' \item uncertainty of vector cubes: see \code{\link{plot.uncertainty_vector_cube}}
#' \item classified cube: see \code{\link{plot.class_cube}}
#' \item classified vector cube: see \code{\link{plot.class_vector_cube}}
#' \item dendrogram cluster: see \code{\link{plot.sits_cluster}}
#' \item SOM map: see \code{\link{plot.som_map}}
#' \item SOM evaluate cluster: see \code{\link{plot.som_evaluate_cluster}}
#' \item geo-distances: see \code{\link{plot.geo_distances}}
#' \item random forest model: see \code{\link{plot.rfor_model}}
#' \item xgboost model: see \code{\link{plot.xgb_model}}
#' \item torch ML model: see \code{\link{plot.torch_model}}
#' }
#'
#' @param x        Object of class "sits".
#' @param y        Ignored.
#' @param together A logical value indicating whether
#'                 the samples should be plotted together.
#' @param ...      Further specifications for \link{plot}.
#'
#' @return A series of plot objects produced by ggplot2 showing all
#'   time series associated to each combination of band and label,
#'   and including the median, and first and third quartile ranges.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # plot sets of time series
#'     plot(cerrado_2classes)
#' }
#'
#' @export
plot.sits <- function(x, y, ..., together = FALSE) {
    .check_set_caller(".plot_sits")
    stopifnot(missing(y))
    # default value is set to empty char in case null
    .check_lgl_parameter(together)
    # Are there more than 30 samples? Plot them together!
    if (together || nrow(x) > 30) {
        p <- .plot_together(x)
    } else {
        # otherwise, take "allyears" as the default
        p <- .plot_allyears(x)
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
#' @param  bands         Bands to be viewed (optional).
#' @param  year_grid     Plot a grid of panels using labels as columns and
#'                       years as rows. Default is FALSE.
#' @return               A plot object produced by ggplot2
#'                       with one average pattern per label.
#'
#' @note
#' This code is reused from the dtwSat package by Victor Maus.
#' @examples
#' if (sits_run_examples()) {
#'     # plot patterns
#'     plot(sits_patterns(cerrado_2classes))
#' }
#' @export
#'
plot.patterns <- function(x, y, ..., bands = NULL, year_grid = FALSE) {
    .check_set_caller(".plot_patterns")
    stopifnot(missing(y))
    # verifies if scales package is installed
    .check_require_packages("scales")
    # extract the patterns for each band
    patterns_bands <- .ts_bands(.ts(x))
    bands <- .default(bands, patterns_bands)
    # pre-condition
    .check_chr_within(bands,
        within = patterns_bands
    )
    # extract only for the selected bands
    .ts(x) <- .ts_select_bands(.ts(x), bands)
    # put the time series in the data frame
    plot_df <- purrr::pmap_dfr(
        list(x[["label"]], x[["time_series"]]),
        function(label, ts) {
            lb <- as.character(label)
            # extract the time series and convert
            df <- tibble::tibble(Time = ts[["Index"]], ts[-1], Pattern = lb)
            return(df)
        }
    )
    # create a data.frame by melting the values per bands
    plot_df <- tidyr::pivot_longer(plot_df, cols = .samples_bands(x))
    # Do we want a multi-year grid?
    if (year_grid) {
        plot_df <- plot_df |>
            dplyr::mutate(year = format(.data[["Time"]], format = "%Y")) |>
            dplyr::mutate(Time = as.Date(format(.data[["Time"]],
                format = "2000-%m-%d"
            )))
    }
    # Plot temporal patterns
    gp <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = .data[["Time"]],
        y = .data[["value"]],
        colour = .data[["name"]]
    )) +
        ggplot2::geom_line()
    # Do we want a multi-year grid?
    if (year_grid) {
        gp <- gp + ggplot2::facet_grid(year ~ Pattern)
    } else {
        gp <- gp + ggplot2::facet_wrap(~Pattern)
    }
    # create a multi-frame plot
    gp <- gp +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_x_date(labels = scales::date_format("%b")) +
        ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
        ggplot2::ylab("Value")
    # plot the data
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
#' This code is reused from the dtwSat package by Victor Maus.
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train an svm model
#'     ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_svm)
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#'     plot(point_class)
#' }
#' @export
#'
plot.predicted <- function(x, y, ...,
                           bands = "NDVI",
                           palette = "Harmonic") {
    .check_set_caller(".plot_predicted")
    stopifnot(missing(y))
    .check_predicted(x)
    # verifies if scales package is installed
    .check_require_packages("scales")
    # check for color_palette parameter (sits 1.4.1)
    dots <- list(...)
    if (missing(palette) && "color_palette" %in% names(dots)) {
        warning(.conf("messages", ".plot_palette"))
        palette <- dots[["color_palette"]]
    }
    # are bands specified?
    if (!.has(bands)) {
        bands <- .samples_bands(x)
    }
    # are the chosen bands in the data?
    if (!all(bands %in% .samples_bands(x))) {
        bands <- .samples_bands(x)
    }
    # configure plot colors
    # get labels from predicted tibble
    labels <- unique(x[["predicted"]][[1]][["class"]])
    colors <- .colors_get(
        labels = labels,
        legend = NULL,
        palette = palette,
        rev = FALSE
    )
    # put the time series in the data frame
    p <- purrr::pmap(
        list(
            x[["latitude"]], x[["longitude"]], x[["label"]],
            x[["time_series"]], x[["predicted"]]
        ),
        function(row_lat, row_long, row_label,
                 row_time_series, row_predicted) {
            lb <- .plot_title(row_lat, row_long, row_label)
            # extract the time series
            ts <- row_time_series
            # convert to data frame
            df_x <- data.frame(
                Time = ts[["Index"]], ts[, bands],
                Series = as.factor(lb)
            )
            # melt the time series data for plotting
            df_x <- tidyr::pivot_longer(df_x,
                cols = -c("Time", "Series"),
                names_to = "variable"
            )
            # define a nice set of breaks for value plotting
            y_labels <- scales::pretty_breaks()(range(df_x[["value"]],
                na.rm = TRUE
            ))
            y_breaks <- y_labels
            # create a data frame with values and intervals
            nrows_p <- nrow(row_predicted)
            df_pol <- purrr::pmap_dfr(
                list(
                    row_predicted[["from"]], row_predicted[["to"]],
                    row_predicted[["class"]], seq(1:nrows_p)
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
                    return(df_p)
                }
            )
            # create a multi-year plot
            df_pol[["Group"]] <- factor(df_pol[["Group"]])
            df_pol[["Class"]] <- factor(df_pol[["Class"]])
            df_pol[["Series"]] <- rep(lb, length(df_pol[["Time"]]))
            # temporal adjustments - create a time index
            idx <- min(df_pol[["Time"]], na.rm = TRUE) - 30 <= df_x[["Time"]] &
                df_x[["Time"]] <= max(df_pol[["Time"]], na.rm = TRUE) + 30
            df_x <- df_x[idx, , drop = FALSE]
            # plot facets
            gp <- ggplot2::ggplot() +
                ggplot2::facet_wrap(~Series,
                    scales = "free_x", ncol = 1
                ) +
                ggplot2::geom_polygon(
                    data = df_pol,
                    ggplot2::aes(
                        x     = .data[["Time"]],
                        y     = .data[["value"]],
                        group = .data[["Group"]],
                        fill  = .data[["Class"]]
                    ),
                    alpha = 0.7
                ) +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::geom_line(
                    data = df_x,
                    ggplot2::aes(
                        x      = .data[["Time"]],
                        y      = .data[["value"]],
                        colour = .data[["variable"]]
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
#' @param  x              Object of class "raster_cube".
#' @param  ...            Further specifications for \link{plot}.
#' @param  band           Band for plotting grey images.
#' @param  red            Band for red color.
#' @param  green          Band for green color.
#' @param  blue           Band for blue color.
#' @param  tile           Tile to be plotted.
#' @param  dates          Dates to be plotted
#' @param  roi            Spatial extent to plot in WGS 84 - named vector
#'                        with either (lon_min, lon_max, lat_min, lat_max) or
#'                        (xmin, xmax, ymin, ymax)
#' @param  palette        An RColorBrewer palette
#' @param  rev            Reverse the color order in the palette?
#' @param  scale          Scale to plot map (0.4 to 1.0)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  max_cog_size   Maximum size of COG overviews (lines or columns)
#' @param  legend_position Where to place the legend (default = "outside")
#'
#' @return               A plot object with an RGB image
#'                       or a B/W image on a color scale
#'
#' @note
#'       Use \code{scale} parameter for general output control.
#'       The \code{dates} parameter indicates the date allows plotting of different dates when
#'       a single band and three dates are provided, `sits` will plot a
#'       multi-temporal RGB image for a single band (useful in the case of
#'       SAR data). For RGB bands with multi-dates, multiple plots will be
#'       produced.
#'
#' @note The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.7)
#' \item \code{legend_title_size}: relative size of legend title (default = 0.7)
#' \item \code{legend_text_size}: relative size of legend text (default = 0.7)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.3)
#' }
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # plot NDVI band of the second date date of the data cube
#'     plot(cube, band = "NDVI", dates = sits_timeline(cube)[1])
#' }
#' @export
plot.raster_cube <- function(x, ...,
                             band = NULL,
                             red = NULL,
                             green = NULL,
                             blue = NULL,
                             tile = x[["tile"]][[1]],
                             dates = NULL,
                             roi = NULL,
                             palette = "RdYlGn",
                             rev = FALSE,
                             scale = 1.0,
                             first_quantile = 0.02,
                             last_quantile = 0.98,
                             max_cog_size = 1024,
                             legend_position = "inside") {
    # check caller
    .check_set_caller(".plot_raster_cube")
    # check roi
    if (.has(roi))
        .check_roi(roi)
    # retrieve dots
    dots <- list(...)
    # deal with wrong parameter "date"
    if ("date" %in% names(dots) && missing(dates)) {
        dates <- as.Date(dots[["date"]])
    }
    # get tmap params from dots
    dots <- list(...)
    tmap_params <- .tmap_params_set(dots, legend_position)
    # is tile inside the cube?
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # verifies if stars package is installed
    .check_require_packages("stars")
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    if (.has(band)) {
        # check palette
        .check_palette(palette)
        # check rev
        .check_lgl_parameter(rev)
    }
    # check scale parameter
    .check_num_parameter(scale, min = 0.2)
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    if (.has(dates)) {
        # is this a valid date?
        dates <- as.Date(dates)
        .check_that(all(dates %in% .tile_timeline(tile)),
                    msg = .conf("messages", ".plot_raster_cube_date")
        )
    } else {
        dates <- .tile_timeline(tile)[[1]]
    }
    # BW or color?
    .check_bw_rgb_bands(band, red, green, blue)
    .check_available_bands(x, band, red, green, blue)

    if (.has(band) && length(dates) == 3) {
        p <- .plot_band_multidate(
            tile = tile,
            band = band,
            dates = dates,
            roi = roi,
            palette = palette,
            rev = rev,
            scale = scale,
            max_cog_size = max_cog_size,
            first_quantile = first_quantile,
            last_quantile = last_quantile,
            tmap_params = tmap_params
        )
        return(p)
    }
    if (length(dates) > 1) {
        warning(.conf("messages", ".plot_raster_cube_single_date"))
    }
    if (.has(band)) {
        p <- .plot_false_color(
            tile = tile,
            band = band,
            date = dates[[1]],
            roi = roi,
            sf_seg    = NULL,
            seg_color = NULL,
            line_width = NULL,
            palette = palette,
            rev = rev,
            scale = scale,
            max_cog_size = max_cog_size,
            first_quantile = first_quantile,
            last_quantile = last_quantile,
            tmap_params = tmap_params
        )
    } else {
        # plot RGB
        p <- .plot_rgb(
            tile = tile,
            red = red,
            green = green,
            blue = blue,
            date = dates[[1]],
            roi = roi,
            sf_seg    = NULL,
            seg_color = NULL,
            line_width = NULL,
            scale = scale,
            max_cog_size = max_cog_size,
            first_quantile = first_quantile,
            last_quantile = last_quantile,
            tmap_params = tmap_params
        )
    }

    return(p)
}
#' @title  Plot SAR data cubes
#' @name plot.sar_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plot SAR raster cube
#'
#' @param  x               Object of class "raster_cube".
#' @param  ...             Further specifications for \link{plot}.
#' @param  band            Band for plotting grey images.
#' @param  red             Band for red color.
#' @param  green           Band for green color.
#' @param  blue            Band for blue color.
#' @param  tile            Tile to be plotted.
#' @param  dates           Dates to be plotted.
#' @param  roi             Spatial extent to plot in WGS 84 - named vector
#'                         with either (lon_min, lon_max, lat_min, lat_max) or
#'                         (xmin, xmax, ymin, ymax)
#' @param  palette         An RColorBrewer palette
#' @param  rev             Reverse the color order in the palette?
#' @param  scale           Scale to plot map (0.4 to 1.0)
#' @param  first_quantile  First quantile for stretching images
#' @param  last_quantile   Last quantile for stretching images
#' @param  max_cog_size    Maximum size of COG overviews (lines or columns)
#' @param  legend_position Where to place the legend (default = "inside")
#'
#' @return               A plot object with an RGB image
#'                       or a B/W image on a color scale for SAR cubes
#'
#' @note
#'       Use \code{scale} parameter for general output control.
#'       The \code{dates} parameter indicates the date allows plotting of different dates when
#'       a single band and three dates are provided, `sits` will plot a
#'       multi-temporal RGB image for a single band (useful in the case of
#'       SAR data). For RGB bands with multi-dates, multiple plots will be
#'       produced.
#'
#' @note The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.7)
#' \item \code{legend_title_size}: relative size of legend title (default = 0.7)
#' \item \code{legend_text_size}: relative size of legend text (default = 0.7)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.3)
#' }
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a SAR data cube from cloud services
#'     cube_s1_grd <-  sits_cube(
#'         source = "MPC",
#'         collection = "SENTINEL-1-GRD",
#'         bands = c("VV", "VH"),
#'         orbit = "descending",
#'         tiles = c("21LUJ"),
#'         start_date = "2021-08-01",
#'         end_date = "2021-09-30"
#'     )
#'     # plot VH band of the first date of the data cube
#'     plot(cube_s1_grd, band = "VH")
#' }
#' @export
plot.sar_cube <- function(x, ...,
                             band = NULL,
                             red = NULL,
                             green = NULL,
                             blue = NULL,
                             tile = x[["tile"]][[1]],
                             dates = NULL,
                             roi = NULL,
                             palette = "Greys",
                             rev = FALSE,
                             scale = 1.0,
                             first_quantile = 0.05,
                             last_quantile = 0.95,
                             max_cog_size = 1024,
                             legend_position = "inside") {

    plot.raster_cube(
        x, ...,
        band = band,
        red = red,
        green = green,
        blue = blue,
        tile = tile,
        dates = dates,
        roi = roi,
        palette = palette,
        rev = rev,
        scale = scale,
        first_quantile = first_quantile,
        last_quantile = last_quantile,
        max_cog_size = max_cog_size,
        legend_position = legend_position

    )
}

#' @title  Plot DEM cubes
#' @name plot.dem_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plot RGB raster cube
#'
#' @param  x             Object of class "dem_cube".
#' @param  ...           Further specifications for \link{plot}.
#' @param  band          Band for plotting grey images.
#' @param  tile          Tile to be plotted.
#' @param  roi           Spatial extent to plot in WGS 84 - named vector
#'                       with either (lon_min, lon_max, lat_min, lat_max) or
#'                       (xmin, xmax, ymin, ymax)
#' @param  palette       An RColorBrewer palette
#' @param  rev           Reverse the color order in the palette?
#' @param  scale         Scale to plot map (0.4 to 1.0)
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  legend_position Where to place the legend (default = "inside")
#' @return               A plot object with a DEM cube
#'                       or a B/W image on a color scale
#'
#' @note
#'       Use \code{scale} parameter for general output control.
#'
#' @note The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.7)
#' \item \code{legend_title_size}: relative size of legend title (default = 0.7)
#' \item \code{legend_text_size}: relative size of legend text (default = 0.7)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.3)
#' }
#'
#' @examples
#' if (sits_run_examples()) {
#' # obtain the DEM cube
#'     dem_cube_19HBA <- sits_cube(
#'         source = "MPC",
#'         collection = "COP-DEM-GLO-30",
#'         bands = "ELEVATION",
#'         tiles = "19HBA"
#'         )
#' # plot the DEM reversing the palette
#'     plot(dem_cube_19HBA, band = "ELEVATION")
#' }
#' @export
plot.dem_cube <- function(x, ...,
                          band = "ELEVATION",
                          tile = x[["tile"]][[1]],
                          roi = NULL,
                          palette = "Spectral",
                          rev = TRUE,
                          scale = 1.0,
                          max_cog_size = 1024,
                          legend_position = "inside") {
    # check caller
    .check_set_caller(".plot_dem_cube")
    # check roi
    if (.has(roi))
        .check_roi(roi)
    # retrieve dots
    dots <- list(...)
    # get tmap params from dots
    tmap_params <- .tmap_params_set(dots, legend_position)
    # is tile inside the cube?
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # verifies if tmap package is installed
    .check_require_packages("tmap")
    # check scale parameter
    .check_num_parameter(scale, min = 0.2)
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    # check band
    .check_that(band %in% .cube_bands(x))
    # crop using ROI
    if (.has(roi)) {
        tile <- tile |>
            .tile_filter_bands(bands = band) |>
            .crop(roi = roi,
                  output_dir = .rand_sub_tempdir(),
                  progress = FALSE)
    }
    # select the file to be plotted
    dem_file <- .tile_path(tile, band)
    # size of data to be read
    sizes <- .tile_overview_size(tile = tile, max_cog_size)
    # retrieve the overview if COG
    dem_file <- .gdal_warp_file(dem_file, sizes)
    # read SpatialRaster file
    rast <- .raster_open_rast(dem_file)
    # plot the DEM
    p <- .tmap_dem_map(r = rast,
                       band = band,
                       palette = palette,
                       rev = rev,
                       scale = scale,
                       tmap_params = tmap_params)
    return(p)
}
#' @title  Plot RGB vector data cubes
#' @name plot.vector_cube
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
#' @param  dates         Dates to be plotted.
#' @param  seg_color     Color to show the segment boundaries
#' @param  line_width    Line width to plot the segments boundary (in pixels)
#' @param  palette       An RColorBrewer palette
#' @param  rev           Reverse the color order in the palette?
#' @param  scale         Scale to plot map (0.4 to 1.5)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)
#' @param  legend_position Where to place the legend (default = "inside")
#' @return               A plot object with an RGB image
#'                       or a B/W image on a color
#'                       scale using the palette
#'
#' @note The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.7)
#' \item \code{legend_title_size}: relative size of legend title (default = 0.7)
#' \item \code{legend_text_size}: relative size of legend text (default = 0.7)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.3)
#' }
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # Segment the cube
#'     segments <- sits_segment(
#'         cube = cube,
#'         output_dir = tempdir(),
#'         multicores = 2,
#'         memsize = 4
#'     )
#'     # plot NDVI band of the second date date of the data cube
#'     plot(segments, band = "NDVI", date = sits_timeline(cube)[1])
#' }
#' @export
plot.vector_cube <- function(x, ...,
                             band = NULL,
                             red = NULL,
                             green = NULL,
                             blue = NULL,
                             tile = x[["tile"]][[1]],
                             dates = NULL,
                             seg_color = "yellow",
                             line_width = 0.3,
                             palette = "RdYlGn",
                             rev = FALSE,
                             scale = 1.0,
                             first_quantile = 0.02,
                             last_quantile = 0.98,
                             max_cog_size = 1024,
                             legend_position = "inside") {
    .check_set_caller(".plot_vector_cube")
    # retrieve dots
    dots <- list(...)
    # deal with wrong parameter "date"
    if ("date" %in% names(dots) && missing(dates)) {
        dates <- as.Date(dots[["date"]])
    }
    # get tmap params from dots
    tmap_params <- .tmap_params_set(dots, legend_position)
    # is tile inside the cube?
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    if (.has(dates)) {
        # is this a valid date?
        dates <- as.Date(dates)[[1]]
        .check_that(all(dates %in% .tile_timeline(tile)),
                    msg = .conf("messages", ".plot_raster_cube_date")
        )
    } else {
        dates <- .tile_timeline(tile)[[1]]
    }
    # retrieve the segments for this tile
    sf_seg <- .segments_read_vec(tile)
    # BW or color?
    .check_bw_rgb_bands(band, red, green, blue)
    .check_available_bands(x, band, red, green, blue)
    if (.has(band)) {
        # plot the band as false color
        p <- .plot_false_color(
            tile = tile,
            band = band,
            date = dates[[1]],
            roi = NULL,
            sf_seg    = sf_seg,
            seg_color = seg_color,
            line_width = line_width,
            palette = palette,
            rev = rev,
            scale = scale,
            first_quantile = first_quantile,
            last_quantile = last_quantile,
            max_cog_size = max_cog_size,
            tmap_params = tmap_params
        )
    } else {
        # plot RGB
        p <- .plot_rgb(
            tile = tile,
            red = red,
            green = green,
            blue = blue,
            date = dates[[1]],
            roi = NULL,
            sf_seg   = sf_seg,
            seg_color = seg_color,
            line_width = line_width,
            first_quantile = first_quantile,
            last_quantile = last_quantile,
            scale = scale,
            max_cog_size = max_cog_size,
            tmap_params = tmap_params
        )
    }
    return(p)
}
#' @title  Plot probability cubes
#' @name   plot.probs_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             Object of class "probs_cube".
#' @param  ...           Further specifications for \link{plot}.
#' @param tile           Tile to be plotted.
#' @param roi            Spatial extent to plot in WGS 84 - named vector
#'                        with either (lon_min, lon_max, lat_min, lat_max) or
#'                        (xmin, xmax, ymin, ymax)
#' @param labels         Labels to plot.
#' @param palette        RColorBrewer palette
#' @param rev            Reverse order of colors in palette?
#' @param quantile       Minimum quantile to plot
#' @param scale          Scale to plot map (0.4 to 1.0)
#' @param max_cog_size   Maximum size of COG overviews (lines or columns)
#' @param legend_position Where to place the legend (default = "outside")
#' @param legend_title    Title of legend (default = "probs")
#' @return               A plot containing probabilities associated
#'                       to each class for each pixel.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # plot the resulting probability cube
#'     plot(probs_cube)
#' }
#'
#' @export
#'
plot.probs_cube <- function(x, ...,
                            tile = x[["tile"]][[1]],
                            roi = NULL,
                            labels = NULL,
                            palette = "YlGn",
                            rev = FALSE,
                            quantile = NULL,
                            scale = 1.0,
                            max_cog_size = 512,
                            legend_position = "outside",
                            legend_title = "probs") {
    .check_set_caller(".plot_probs_cube")
    # precondition
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # check roi
    if (.has(roi))
        .check_roi(roi)
    # get tmap params from dots
    dots <- list(...)
    tmap_params <- .tmap_params_set(dots, legend_position, legend_title)
    # filter the cube
    tile <- .cube_filter_tiles(cube = x, tiles = tile)

    # plot the probs cube
    p <- .plot_probs(tile = tile,
                     roi = roi,
                     labels_plot = labels,
                     palette = palette,
                     rev = rev,
                     scale = scale,
                     quantile = quantile,
                     max_cog_size = max_cog_size,
                     tmap_params = tmap_params)

    return(p)
}
#' @title  Plot probability vector cubes
#' @name   plot.probs_vector_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             Object of class "probs_vector_cube".
#' @param  ...           Further specifications for \link{plot}.
#' @param tile           Tile to be plotted.
#' @param labels         Labels to plot
#' @param palette        RColorBrewer palette
#' @param rev            Reverse order of colors in palette?
#' @param scale          Scale to plot map (0.4 to 1.0)
#' @param legend_position Where to place the legend (default = "outside")
#' @return               A plot containing probabilities associated
#'                       to each class for each pixel.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         seg_fn = sits_slic(step = 5,
#'                            compactness = 1,
#'                            dist_fun = "euclidean",
#'                            avg_fun = "median",
#'                            iter = 20,
#'                            minarea = 10,
#'                            verbose = FALSE),
#'         output_dir = tempdir()
#'     )
#'     # classify a data cube
#'     probs_vector_cube <- sits_classify(
#'         data = segments,
#'         ml_model = rfor_model,
#'         output_dir = tempdir()
#'     )
#'     # plot the resulting probability cube
#'     plot(probs_vector_cube, labels = "Forest")
#' }
#'
#' @export
#'
plot.probs_vector_cube <- function(x, ...,
                                   tile = x[["tile"]][[1]],
                                   labels = NULL,
                                   palette = "YlGn",
                                   rev = FALSE,
                                   scale = 1.0,
                                   legend_position = "outside") {
    .check_set_caller(".plot_probs_vector")
    # precondition
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # retrieve dots
    dots <- list(...)
    # get tmap params from dots
    tmap_params <- .tmap_params_set(dots, legend_position)

    # filter the cube
    tile <- .cube_filter_tiles(cube = x, tiles = tile)

    # plot the probs vector cube
    p <- .plot_probs_vector(tile = tile,
                            labels_plot = labels,
                            palette = palette,
                            rev = rev,
                            scale = scale,
                            tmap_params = tmap_params)

    return(p)
}
#' @title  Plot variance cubes
#' @name   plot.variance_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             Object of class "variance_cube".
#' @param  ...           Further specifications for \link{plot}.
#' @param tile           Tile to be plotted.
#' @param  roi           Spatial extent to plot in WGS 84 - named vector
#'                        with either (lon_min, lon_max, lat_min, lat_max) or
#'                        (xmin, xmax, ymin, ymax)
#' @param labels         Labels to plot.
#' @param palette        RColorBrewer palette
#' @param rev            Reverse order of colors in palette?
#' @param type           Type of plot ("map" or "hist")
#' @param scale          Scale to plot map (0.4 to 1.0)
#' @param quantile       Minimum quantile to plot
#' @param  max_cog_size  Maximum size of COG overviews (lines or columns)

#' @param legend_position Where to place the legend (default = "inside")
#' @param legend_title    Title of legend (default = "probs")
#' @return                A plot containing local variances associated to the
#'                        logit probability for each pixel and each class.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # obtain a variance cube
#'     var_cube <- sits_variance(probs_cube, output_dir = tempdir())
#'     # plot the variance cube
#'     plot(var_cube)
#' }
#'
#' @export
#'
plot.variance_cube <- function(x, ...,
                               tile = x[["tile"]][[1]],
                               roi = NULL,
                               labels = NULL,
                               palette = "YlGnBu",
                               rev = FALSE,
                               type = "map",
                               quantile = 0.75,
                               scale = 1.0,
                               max_cog_size = 1024,
                               legend_position = "inside",
                               legend_title = "logvar") {
    .check_set_caller(".plot_variance_cube")
    # precondition
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # check roi
    if (.has(roi))
        .check_roi(roi)
    # retrieve dots
    dots <- list(...)
    # get tmap params from dots
    tmap_params <- .tmap_params_set(dots, legend_position, legend_title)
    # filter the cube
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    # check type
    .check_that(type %in% c("map", "hist"))
    # plot the variance cube
    if (type == "map") {
        p <- .plot_probs(tile = tile,
                         roi = roi,
                         labels_plot = labels,
                         palette = palette,
                         rev = rev,
                         scale = scale,
                         quantile = quantile,
                         max_cog_size = max_cog_size,
                         tmap_params = tmap_params)
    } else {
        p <- .plot_variance_hist(tile)
    }

    return(p)
}

#' @title  Plot uncertainty cubes
#' @name   plot.uncertainty_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x              Object of class "probs_image".
#' @param  ...            Further specifications for \link{plot}.
#' @param  tile           Tiles to be plotted.
#' @param  roi            Spatial extent to plot in WGS 84 - named vector
#'                        with either (lon_min, lon_max, lat_min, lat_max) or
#'                        (xmin, xmax, ymin, ymax)
#' @param  palette        An RColorBrewer palette
#' @param  rev            Reverse the color order in the palette?
#' @param  scale          Scale to plot map (0.4 to 1.0)
#' @param  first_quantile First quantile for stretching images
#' @param  last_quantile  Last quantile for stretching images
#' @param  max_cog_size   Maximum size of COG overviews (lines or columns)
#' @param legend_position Where to place the legend (default = "inside")
#'
#' @return               A plot object produced by the stars package
#'                       with a map showing the uncertainty associated
#'                       to each classified pixel.
#' @note The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.7)
#' \item \code{legend_title_size}: relative size of legend title (default = 1.0)
#' \item \code{legend_text_size}: relative size of legend text (default = 1.0)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.5)
#' }
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # calculate uncertainty
#'     uncert_cube <- sits_uncertainty(probs_cube, output_dir = tempdir())
#'     # plot the resulting uncertainty cube
#'     plot(uncert_cube)
#' }
#' @export
#'
plot.uncertainty_cube <- function(x, ...,
                                  tile = x[["tile"]][[1]],
                                  roi = NULL,
                                  palette = "RdYlGn",
                                  rev = TRUE,
                                  scale = 1.0,
                                  first_quantile = 0.02,
                                  last_quantile = 0.98,
                                  max_cog_size = 1024,
                                  legend_position = "inside") {
    .check_set_caller(".plot_uncertainty_cube")
    # check roi
    if (.has(roi))
        .check_roi(roi)
    # get tmap params from dots
    dots <- list(...)
    tmap_params <- .tmap_params_set(dots, legend_position)
    # precondition
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )

    # filter the cube
    tile <- .cube_filter_tiles(cube = x, tiles = tile[[1]])
    band <- .tile_bands(tile)
    # plot the data
    p <- .plot_false_color(
        tile = tile,
        band = band,
        date = NULL,
        roi = roi,
        sf_seg    = NULL,
        seg_color = NULL,
        line_width = NULL,
        palette = palette,
        rev = rev,
        scale = scale,
        first_quantile = first_quantile,
        last_quantile = last_quantile,
        max_cog_size = max_cog_size,
        tmap_params = tmap_params
    )
    return(p)
}
#' @title  Plot uncertainty vector cubes
#' @name   plot.uncertainty_vector_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             Object of class "probs_vector_cube".
#' @param  ...           Further specifications for \link{plot}.
#' @param tile           Tile to be plotted.
#' @param palette        RColorBrewer palette
#' @param rev            Reverse order of colors in palette?
#' @param scale          Scale to plot map (0.4 to 1.0)
#' @param legend_position Where to place the legend (default = "inside")
#' @return               A plot containing probabilities associated
#'                       to each class for each pixel.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         seg_fn = sits_slic(step = 5,
#'                            compactness = 1,
#'                            dist_fun = "euclidean",
#'                            avg_fun = "median",
#'                            iter = 20,
#'                            minarea = 10,
#'                            verbose = FALSE),
#'         output_dir = tempdir()
#'     )
#'     # classify a data cube
#'     probs_vector_cube <- sits_classify(
#'         data = segments,
#'         ml_model = rfor_model,
#'         output_dir = tempdir()
#'     )
#'     # measure uncertainty
#'     uncert_vector_cube <- sits_uncertainty(
#'         cube = probs_vector_cube,
#'         type = "margin",
#'         output_dir = tempdir()
#'     )
#'     # plot the resulting uncertainty cube
#'     plot(uncert_vector_cube)
#' }
#'
#' @export
#'
plot.uncertainty_vector_cube <- function(x, ...,
                                         tile = x[["tile"]][[1]],
                                         palette =  "RdYlGn",
                                         rev = TRUE,
                                         scale = 1.0,
                                         legend_position = "inside") {
    .check_set_caller(".plot_uncertainty_vector_cube")
    # precondition
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # check for color_palette parameter (sits 1.4.1)
    dots <- list(...)
    # get tmap params from dots
    tmap_params <- .tmap_params_set(dots, legend_position)

    # filter the cube
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    # set the title
    band <- .tile_bands(tile)
    # plot the probs vector cube
    p <- .plot_uncertainty_vector(tile = tile,
                                  palette = palette,
                                  rev = rev,
                                  scale = scale,
                                  tmap_params = tmap_params)

    return(p)
}
#' @title  Plot classified images
#' @name   plot.class_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified raster using ggplot.
#'
#' @param  x               Object of class "class_cube".
#' @param  y               Ignored.
#' @param  ...             Further specifications for \link{plot}.
#' @param  tile            Tile to be plotted.
#' @param  roi             Spatial extent to plot in WGS 84 - named vector
#'                         with either (lon_min, lon_max, lat_min, lat_max) or
#'                         (xmin, xmax, ymin, ymax)
#' @param  title           Title of the plot.
#' @param  legend          Named vector that associates labels to colors.
#' @param  palette         Alternative RColorBrewer palette
#' @param  scale           Relative scale (0.4 to 1.0) of plot text
#' @param  max_cog_size    Maximum size of COG overviews (lines or columns)
#' @param legend_position  Where to place the legend (default = "outside")
#'
#' @return                 A  color map, where each pixel has the color
#'                         associated to a label, as defined by the legend
#'                         parameter.
#' @note The following optional parameters are available to allow for detailed
#'       control over the plot output:
#' \itemize{
#' \item \code{graticules_labels_size}: size of coordinates labels (default = 0.8)
#' \item \code{legend_title_size}: relative size of legend title (default = 1.0)
#' \item \code{legend_text_size}: relative size of legend text (default = 1.0)
#' \item \code{legend_bg_color}: color of legend background (default = "white")
#' \item \code{legend_bg_alpha}: legend opacity (default = 0.5)
#' }
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # label cube with the most likely class
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     # plot the resulting classified image
#'     plot(label_cube)
#' }
#' @export
#'
plot.class_cube <- function(x, y, ...,
                            tile = x[["tile"]][[1]],
                            roi = NULL,
                            title = "Classified Image",
                            legend = NULL,
                            palette = "Spectral",
                            scale = 1.0,
                            max_cog_size = 1024,
                            legend_position = "outside") {
    stopifnot(missing(y))
    # set caller to show in errors
    .check_set_caller(".plot_class_cube")
    # check roi
    if (.has(roi))
        .check_roi(roi)
    # check for color_palette parameter (sits 1.4.1)
    dots <- list(...)
    # get tmap params from dots
    tmap_params <- .tmap_params_set(dots, legend_position)
    # precondition - cube must be a labelled cube
    cube <- x
    .check_is_class_cube(cube)

    # precondition
    if (.has(tile))
        .check_chr_contains(
            x = cube[["tile"]],
            contains = tile,
            case_sensitive = FALSE,
            discriminator = "all_of",
            can_repeat = FALSE,
            msg = .conf("messages", ".plot_raster_cube_tile")
        )

    # select only one tile
    tile <- .cube_filter_tiles(cube = cube, tiles = tile)

    # plot class cube
    .plot_class_image(
        tile = tile,
        roi = roi,
        legend = legend,
        palette = palette,
        scale = scale,
        max_cog_size = max_cog_size,
        tmap_params = tmap_params
    )
}
#' @title  Plot Segments
#' @name plot.class_vector_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plot vector classified cube
#'
#' @param  x               Object of class "segments".
#' @param  ...             Further specifications for \link{plot}.
#' @param  tile            Tile to be plotted.
#' @param  legend          Named vector that associates labels to colors.
#' @param  seg_color       Segment color.
#' @param  line_width      Segment line width.
#' @param  palette         Alternative RColorBrewer palette
#' @param  scale           Scale to plot map (0.4 to 1.0)
#' @param  legend_position Where to place the legend (default = "outside")
#'
#' @return               A plot object with an RGB image
#'                       or a B/W image on a color
#'                       scale using the chosen palette
#'
#' @note To see which color palettes are supported, please run
#' @examples
#' if (sits_run_examples()) {
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         output_dir = tempdir()
#'     )
#'     # create a classification model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify the segments
#'     probs_segs <- sits_classify(
#'         data = segments,
#'         ml_model = rfor_model,
#'         output_dir = tempdir()
#'     )
#'     #
#'     # Create a classified vector cube
#'     class_segs <- sits_label_classification(
#'         cube = probs_segs,
#'         output_dir = tempdir(),
#'         multicores = 2,
#'         memsize = 4
#'     )
#'     # plot the segments
#'     plot(class_segs)
#' }
#' @export
plot.class_vector_cube <- function(x, ...,
                                   tile = x[["tile"]][[1]],
                                   legend = NULL,
                                   seg_color = "black",
                                   line_width = 0.5,
                                   palette = "Spectral",
                                   scale = 1.0,
                                   legend_position = "outside") {
    # set caller to show in errors
    .check_set_caller(".plot_class_vector_cube")
    # check for color_palette parameter (sits 1.4.1)
    dots <- list(...)
    # get tmap params from dots
    tmap_params <- .tmap_params_set(dots, legend_position)
    # only one tile at a time
    .check_chr_parameter(tile)
    # is tile inside the cube?
    .check_chr_contains(
        x = x[["tile"]],
        contains = tile,
        case_sensitive = FALSE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".plot_raster_cube_tile")
    )
    # filter the tile to be processed
    tile <- .cube_filter_tiles(cube = x, tiles = tile)
    # plot class vector cube
    p <- .plot_class_vector(
        tile = tile,
        legend = legend,
        palette = palette,
        scale = scale,
        tmap_params = tmap_params
    )
    return(p)
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
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
#'     # plot the model
#'     plot(rf_model)
#' }
#' @export
#'
plot.rfor_model <- function(x, y, ...) {
    # verifies if randomForestExplainer package is installed
    .check_require_packages("randomForestExplainer")
    .check_is_sits_model(x)
    # retrieve the random forest object from the env iroment
    rf <- .ml_model(x)
    p <- randomForestExplainer::plot_min_depth_distribution(rf)
    return(p)
}

#'
#' @title  Plot confusion matrix
#' @name   plot.sits_accuracy
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @description Plot a bar graph with informations about the confusion matrix
#'
#' @param  x            Object of class "plot.sits_accuracy".
#' @param  y            Ignored.
#' @param  ...          Further specifications for \link{plot}.
#' @param  title        Title of plot.
#' @return              A plot object produced by the ggplot2 package
#'                      containing color bars showing the confusion
#'                      between classes.
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # show accuracy for a set of samples
#'     train_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
#'     test_data  <- sits_sample(samples_modis_ndvi, frac = 0.5)
#'     # compute a random forest model
#'     rfor_model <- sits_train(train_data, sits_rfor())
#'     # classify training points
#'     points_class <- sits_classify(
#'         data = test_data, ml_model = rfor_model
#'     )
#'     # calculate accuracy
#'     acc <- sits_accuracy(points_class)
#'     # plot accuracy
#'     plot(acc)
#' }
#' @export
#'
plot.sits_accuracy <- function(x, y, ..., title = "Confusion matrix") {
    stopifnot(missing(y))
    data <- x
    if (!inherits(data, "sits_accuracy")) {
        message(.conf("messages", ".plot_sits_accuracy"))
        return(invisible(NULL))
    }

    # configure plot colors
    # get labels from cluster table
    labels <- colnames(x[["table"]])
    colors <- .colors_get(
        labels = labels,
        legend = NULL,
        palette = "Set3",
        rev = TRUE
    )

    data <- tibble::as_tibble(t(prop.table(x[["table"]], margin = 2)))

    colnames(data) <- c("pred", "class", "conf_per")

    p <- ggplot2::ggplot() +
        ggplot2::geom_bar(
            ggplot2::aes(
                y = .data[["conf_per"]],
                x = .data[["pred"]],
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
        ggplot2::labs(x = "Class", y = "Agreement with reference") +
        ggplot2::scale_fill_manual(name = "Class", values = colors) +
        ggplot2::ggtitle(title)

    p <- graphics::plot(p)
    return(invisible(p))
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
#'     som_map <- sits_som_map(samples_modis_ndvi)
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
        message(.conf("messages", ".plot_som_evaluate_cluster"))
        return(invisible(NULL))
    }

    # Filter the cluster to plot
    if (!(is.null(name_cluster))) {
        data <- dplyr::filter(data, .data[["cluster"]] %in% name_cluster)
    }
    # configure plot colors
    # get labels from cluster table
    labels <- unique(data[["class"]])
    colors <- .colors_get(
        labels = labels,
        legend = NULL,
        palette = "Set3",
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
        ggplot2::labs(x = "Class", y = "Percentage of mixture") +
        ggplot2::scale_fill_manual(name = "Class label", values = colors) +
        ggplot2::ggtitle(title)

    p <- graphics::plot(p)
    return(invisible(p))
}
#' @title  Plot a SOM map
#' @name   plot.som_map
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a SOM map generated by "sits_som_map".
#' The plot function produces different plots based on the input data.
#' If type is "codes", plots the vector weight for in each neuron.
#' If type is "mapping", shows where samples are mapped.
#'
#' @param  x          Object of class "som_map".
#' @param  y          Ignored.
#' @param  ...        Further specifications for \link{plot}.
#' @param  type       Type of plot: "codes" for neuron weight (time series) and
#'                    "mapping" for the number of samples allocated in a neuron.
#' @param  band       What band will be plotted.
#'
#' @return            Called for side effects.
#'
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # create a SOM map
#'     som_map <- sits_som_map(samples_modis_ndvi)
#'     # plot the SOM map
#'     plot(som_map)
#' }
#' @export
#'
plot.som_map <- function(x, y, ..., type = "codes", band = 1) {
    stopifnot(missing(y))
    koh <- x
    if (!inherits(koh, "som_map")) {
        message(.conf("messages", ".plot_som_map"))
        return(invisible(NULL))
    }
    if (type == "mapping") {
        graphics::plot(koh[["som_properties"]],
            bgcol = koh[["som_properties"]][["paint_map"]],
            "mapping", whatmap = band,
            codeRendering = "lines"
        )
    } else if (type == "codes") {
        graphics::plot(koh[["som_properties"]],
            bgcol = koh[["som_properties"]][["paint_map"]],
            "codes", whatmap = band,
            codeRendering = "lines"
        )
    }

    # create a legend
    leg <- cbind(koh[["som_properties"]][["neuron_label"]],
                 koh[["som_properties"]][["paint_map"]])
    graphics::legend(
        "bottomright",
        legend = unique(leg[, 1]),
        col = unique(leg[, 2]),
        pch = 15,
        pt.cex = 2,
        cex = 1,
        text.col = "black",
        inset = c(0.0095, 0.05),
        xpd = TRUE,
        ncol = 1
    )
    return(invisible(x))
}
#' @title  Plot XGB model
#' @name   plot.xgb_model
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plots trees in an extreme gradient boosting model.
#'
#'
#' @param  x             Object of class "xgb_model".
#' @param  ...           Further specifications for \link{plot}.
#' @param  trees         Vector of trees to be plotted
#' @param  width         Width of the output window
#' @param  height        Height of the output window
#' @return               A plot
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train an extreme gradient boosting
#'     xgb_model <- sits_train(samples_modis_ndvi,
#'         ml_method = sits_xgboost()
#'     )
#'     plot(xgb_model)
#' }
#' @export
#'
plot.xgb_model <- function(x, ...,
                           trees = 0:4,
                           width = 1500,
                           height = 1900) {
    # verifies if DiagrammeR package is installed
    .check_require_packages("DiagrammeR")
    .check_is_sits_model(x)
    # retrieve the XGB object from the environment
    xgb <- .ml_model(x)
    # plot the trees
    gr <- xgboost::xgb.plot.tree(model = xgb, trees = trees, render = FALSE)
    p <-  DiagrammeR::render_graph(gr, width = width, height = height)
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
#'     ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_tempcnn)
#'     # plot the model
#'     plot(ml_model)
#' }
#' @export
#'
plot.torch_model <- function(x, y, ...) {
    stopifnot(missing(y))
    model <- x
    if (!inherits(model, "torch_model")) {
        message(.conf("messages", ".plot_torch_model"))
        return(invisible(NULL))
    }
    # set the model variables to be plotted
    model_vars <- c("records", "metrics")
    # retrieve the model variables from the environment
    metrics_lst <- environment(model)[["torch_model"]][[model_vars]]

    metrics_dfr <- .map_dfr(names(metrics_lst), function(name) {
        met <- metrics_lst[[name]]

        .map_dfr(met, tibble::as_tibble_row) |>
            dplyr::mutate(epoch = seq_len(dplyr::n()), data = name) |>
            tidyr::pivot_longer(cols = 1:2, names_to = "metric")
    })

    p <- ggplot2::ggplot(metrics_dfr, ggplot2::aes(
        x = .data[["epoch"]],
        y = .data[["value"]],
        color = .data[["data"]],
        fill = .data[["data"]]
    ))

    p <- p + ggplot2::geom_point(
        shape = 21, col = 1,
        na.rm = TRUE, size = 2
    ) +
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
#'     distances <- sits_geo_dist(samples_modis_ndvi, mt_sf)
#'     # plot sample-to-sample and sample-to-prediction distances
#'     plot(distances)
#' }
#' @export
#'
plot.geo_distances <- function(x, y, ...) {
    distances <- x
    if (!inherits(distances, "geo_distances")) {
        message(.conf("messages", ".plot_geo_distances"))
        return(invisible(NULL))
    }

    density_plot <-
        distances |>
        dplyr::mutate(distance = .data[["distance"]] / 1000) |>
        ggplot2::ggplot(ggplot2::aes(x = .data[["distance"]])) +
        ggplot2::geom_density(
            ggplot2::aes(
                color = .data[["type"]],
                fill = .data[["type"]]
            ),
            linewidth = 1, alpha = 0.25
        ) +
        ggplot2::scale_x_log10(labels = scales::label_number()) +
        ggplot2::xlab("Distance (km)") +
        ggplot2::ylab("") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::ggtitle("Distribution of Nearest Neighbor Distances")
    return(density_plot)
}

#' @title Plot a dendrogram cluster
#' @name plot.sits_cluster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Plot a dendrogram
#'
#' @param x             sits tibble with cluster indexes.
#' @param ...           Further specifications for \link{plot}.
#' @param cluster       cluster object produced by `sits_cluster` function.
#' @param cutree_height dashed horizontal line to be drawn
#'                      indicating the height of dendrogram cutting.
#' @param palette       HCL color palette.
#'
#' @return              The dendrogram object.
#'
#' @examples
#' if (sits_run_examples()) {
#'      samples <- sits_cluster_dendro(cerrado_2classes,
#'                 bands = c("NDVI", "EVI"))
#' }
#'
#' @export
plot.sits_cluster <- function(x, ...,
                              cluster,
                              cutree_height,
                              palette) {
    .check_set_caller(".plot_sits_cluster")
    # verifies if dendextend and methods packages is installed
    .check_require_packages(c("dendextend", "methods"))
    # ensures that a cluster object  exists
    .check_na_null_parameter(cluster)
    # get data labels
    data_labels <- x[["label"]]

    # extract the dendrogram object
    hclust_cl <- methods::S3Part(cluster, strictS3 = TRUE)
    dend <- hclust_cl |> stats::as.dendrogram()

    # colors vector
    colors <- .colors_get(
        labels = data_labels,
        legend = NULL,
        palette = palette,
        rev = TRUE
    )
    colors_leg <- colors[unique(data_labels)]

    # set the visualization params for dendrogram
    dend <- dend |>
        dendextend::set(
            what = "labels",
            value = character(length = length(data_labels))
        ) |>
        dendextend::set(
            what = "branches_k_color",
            value = colors,
            k = length(data_labels)
        )

    graphics::plot(dend,
        ylab = paste(
            tools::file_path_sans_ext(cluster@method),
            "linkage distance"
        )
    )
    # plot cutree line
    if (.has(cutree_height)) {
        graphics::abline(h = cutree_height, lty = 2)
    }

    # plot legend
    graphics::legend("topright",
        fill = colors_leg,
        legend = .samples_labels(x)
    )
    return(invisible(dend))
}
