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
#'  \item{classified time series: } {see \code{\link{plot.predicted}}}
#'  \item{raster  cube: }         {see \code{\link{plot.raster_cube}}}
#'  \item{classification probabilities: }{see \code{\link{plot.probs_cube}}}
#'  \item{classified image: }     {see \code{\link{plot.classified_image}}}
#'  \item{SOM evaluate cluster: } {see \code{\link{plot.som_evaluate_cluster}}}
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
#' @param  x            object of class "sits"
#' @param  y            ignored
#' @param ...           further specifications for \link{plot}.
#' @return              The plot itself.
#'
#' @examples
#' \dontrun{
#' # Read a set of samples with 2 classes ("Cerrado" and "Pasture")
#' # Plot all the samples together
#' plot(cerrado_2classes)
#' # Plot the first 20 samples (defaults to "allyears")
#' plot(cerrado_2classes[1:20, ])
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
#' @description Given a sits tibble with a set of patterns, plot them.
#'
#' @param  x             object of class "patterns"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @return               The plot itself.
#'
#' @examples
#' \dontrun{
#' # Read a set of samples with 2 classes ("Cerrado" and "Pasture")
#' # Plot the patterns
#' plot(sits_patterns(cerrado_2classes))
#' }
#'
#' @export
#'
plot.patterns <- function(x, y, ...) {
    stopifnot(missing(y))
    p <- .sits_plot_patterns(x)
    return(invisible(p))
}

#' @title  Plot time series predictions
#' @name   plot.predicted
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Given a sits tibble with a set of predictions, plot them
#'
#' @param  x             object of class "predicted"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param  bands         bands used for visualisation
#' @param  palette       hcl palette used for visualisation
#'                       (in case classes are not in the default sits palette)
#' @return               The plot itself.
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for Mato Grosso region (provided by EMBRAPA)
#' samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
#' # classify the point
#' model_svm <- sits_train(samples_mt_ndvi, ml_method = sits_svm())
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' class_ndvi.tb <- sits_classify(point_ndvi, model_svm)
#' # plot the classification
#' plot(class_ndvi.tb)
#' }
#'
#' @export
#'
plot.predicted <- function(x, y, ...,
                           bands = "NDVI",
                           palette = "Harmonic") {
    stopifnot(missing(y))
    p <- .sits_plot_predicted_ts(x, bands, palette)
    return(invisible(p))
}
#' @title  Plot RGB data cubes
#' @name plot.raster_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses mapview to visualize raster cube and classified images
#'
#' @param  x             object of class "raster_cube" or "classified image"
#' @param  ...           further specifications for \link{sits_view}.
#' @param  band          for plotting grey images
#' @param  red           band for red color.
#' @param  green         band for green color.
#' @param  blue          band for blue color.
#' @param  tile          tile number to be plotted
#' @param  time          temporal instances to be plotted.
#' @param  roi           sf object giving a region of interest.
#'
#' @return               plot object
#' @export
plot.raster_cube <- function(x, ...,
                                  band = NULL,
                                  red = NULL,
                                  green = NULL,
                                  blue = NULL,
                                  tile = 1,
                                  time = 1,
                                  roi = NULL) {

    # precondition
    .check_num(
        tile, min = 1, max = nrow(x), is_integer = TRUE,
        len_min = 1, len_max = 1,
        msg = "invalid tile parameter"
    )
    # select only one tile
    x <- x[tile, ]

    if (!purrr::is_null(band)) {
        red = band
        green = band
        blue = band
    }
    else
    {
        if (purrr::is_null(red) || purrr::is_null(green) || purrr::is_null(blue))
            stop("missing red, green, or blue bands")
    }

    # preconditions
    .check_that(
        x = all(c(red, green, blue) %in% sits_bands(x)),
        msg = "requested RGB bands are not available in data cube"
    )

    timeline <- sits_timeline(x)
    .check_that(
        x = time >= 1 & time <= length(timeline),
        msg = "time parameter out of bounds"
    )

    # verify sf package if roi is informed
    if (!purrr::is_null(roi)) {
        if (!requireNamespace("sf", quietly = TRUE)) {
            stop("Please install package sf.", call. = FALSE)
        }

        # filter only intersecting tiles
        intersects <- slider::slide(x, function(tile) {
            .sits_raster_sub_image_intersects(cube = tile,
                                              roi = roi)
        }) %>% unlist()

        # check if intersection is not empty
        .check_that(
            x = any(intersects),
            msg = "informed roi does not intersect cube"
        )

        x <- x[intersects, ]
    }

    # plot only the selected tile
    # select only the bands for the timeline
    bands_date <- .file_info(x) %>%
        dplyr::filter(date == as.Date(timeline[[time]]))

    # Are we plotting a grey image
    if (!purrr::is_null(band)) {
        rgb_stack <- dplyr::filter(bands_date, band == red)$path
    }
    else
    {
        # get RGB files for the requested timeline
        red_file <- dplyr::filter(bands_date, band == red)$path
        green_file <- dplyr::filter(bands_date, band == green)$path
        blue_file <- dplyr::filter(bands_date, band == blue)$path
        # put the band on a raster/terra stack
        rgb_stack <- c(red_file, green_file, blue_file)
    }
    # use the raster package to obtain a raster object from a stack
    r_obj <- .raster_open_stack.terra(rgb_stack)

    # extract region of interest
    if (!purrr::is_null(roi)) {
        sub_image <- .sits_raster_sub_image(tile = x, roi = roi)
        r_obj <- .raster_crop.terra(r_obj = r_obj, block = sub_image)
    }

    .check_that(
        x = .raster_ncols(r_obj) > 0 && .raster_nrows(r_obj) > 0,
        msg = "unable to retrieve raster data"
    )
    # view the RGB file
    if (!purrr::is_null(band)) {
        rgb <- suppressWarnings(
            terra::plotRGB(r_obj,
                           r = 1,
                           g = 1,
                           b = 1,
                           stretch = "hist")
        )
    }
    else {
        rgb <- suppressWarnings(
            terra::plotRGB(r_obj,
                           r = 1,
                           g = 2,
                           b = 3,
                           stretch = "hist")
        )
    }


    return(invisible(r_obj))
}
#' @title  Plot probability cubes
#' @name   plot.probs_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             object of class "probs_image"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param tile           tile number to be plotted
#' @param labels         labels to plot (optional)
#' @param n_colors       number of colors to plot
#' @param palette        hcl palette used for visualisation
#'
#' @return               The plot itself.
#'
#' @export
#'
plot.probs_cube <- function(x, y, ...,
                            tile = 1,
                            labels = NULL,
                            n_colors = 20,
                            palette = "Terrain") {
    stopifnot(missing(y))
    # verifies if stars package is installed
    if (!requireNamespace("stars", quietly = TRUE)) {
        stop("Please install package stars.", call. = FALSE)
    }
    breaks <- "pretty"
    n_breaks <- n_colors + 1
    # define the output color palette
    col <- grDevices::hcl.colors(n = n_colors,
                                 palette = palette,
                                 alpha = 1,
                                 rev = TRUE)
    # create a stars object
    st <- stars::read_stars(.file_info_path(x[tile,]))
    # get the labels
    labels_cube <- sits_labels(x)

    # verify if label is not NULL
    if (!purrr::is_null(labels)) {
        # label is not null, then plot only the label
        layers <- match(labels, labels_cube)
        p <- st %>%
            dplyr::slice(index = layers, along = "band") %>%
            plot(breaks = breaks,
                 nbreaks = n_breaks,
                 col = col,
                 main = labels) %>%
            suppressMessages()
    }
    else {
        p <- suppressMessages(plot(st,
                                   breaks = breaks,
                                   nbreaks = n_breaks,
                                   col = col,
                                   main = labels_cube)
        )
    }

    return(invisible(p))
}

#' @title  Plot uncertainty cubes
#' @name   plot.uncertainty_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             object of class "probs_image"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param tile           tile number to be plotted
#' @param n_breaks       number of breaks to plot
#' @param breaks         type of class intervals
#' @param palette        hcl palette used for visualisation
#'
#' @return               The plot itself.
#'
#' @note
#'
#' \itemize{Possible class intervals
#'  \item{"sd":} {intervals based on the average and standard deviation.}
#'  \item{"equal": } {divides the range of the variable into n parts.}
#'  \item{"pretty": } {number of breaks likely to be legible.}
#'  \item{"quantile": } {quantile breaks}
#'  \item{"kmeans" :} {uses kmeans to generate the breaks.}
#'  \item{"hclust" :} {breaks defined by hierarchical clustering.}
#'  \item{"bclust" :} {breaks defined by bagged clustering.}
#'  \item{"fisher" :} {method proposed by Fischer (1958).}
#'  \item{"jenks" :} {method proposed by Jenks.}
#'  \item{"dpih" :} {based on the bin width of a histogram.}
#'  \item{"headtails" :} {algorithm proposed by Bin Jiang (2013)}
#'  }
#'
#' @export
#'
plot.uncertainty_cube <- function(x, y, ...,
                                  tile = 1,
                                  n_breaks = 11,
                                  breaks = "pretty",
                                  palette = "Blues") {
    stopifnot(missing(y))
    # verifies if stars package is installed
    if (!requireNamespace("stars", quietly = TRUE)) {
        stop("Please install package stars.", call. = FALSE)
    }
    # check class interval
    .check_chr_within(
        x = breaks,
        within = .config_get("class_intervals"),
        discriminator = "any_of",
        msg = "invalid class interval"
    )
    n_colors <- n_breaks - 1
    # define the output color palette
    col <- grDevices::hcl.colors(n = n_colors, palette = palette,
                                 alpha = 1, rev = TRUE)
    # create a stars object
    st <- stars::read_stars(.file_info_path(x[tile,]))
    p <- suppressMessages(plot(st,
                               breaks = breaks,
                               nbreaks = n_breaks,
                               col = col,
                               main = "Uncertainty")
    )

    return(invisible(p))
}

#' @title  Plot classified images
#' @name   plot.classified_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified raster using ggplot.
#'
#' @param  x               object of class "classified_image"
#' @param  y               ignored
#' @param  ...             further specifications for \link{plot}.
#' @param  time            temporal reference for plot.
#' @param  title           title of the plot
#' @param  legend          named vector that associates labels to colors
#' @param  palette         alternative palette that uses grDevices::hcl.pals()
#' @param  rev             invert the order of hcl palette (TRUE/FALSE)
#'
#' @export
#'
plot.classified_image <- function(x, y, ...,
                                  time = 1,
                                  title = "Classified Image",
                                  legend = NULL,
                                  palette = "Spectral",
                                  rev = TRUE) {
    stopifnot(missing(y))

    p <- .sits_plot_classified_image(cube = x,
                                     time = time,
                                     title = title,
                                     legend = legend,
                                     palette = palette,
                                     rev = rev)

}

#' @title  Plot confusion between clusters
#' @name   plot.som_evaluate_cluster
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#'
#' @description Plot a bar graph with informations about each cluster.
#' The percentage of mixture between the clusters.
#'
#' @param  x            object of class "plot.som_evaluate_cluster"
#' @param  y            ignored
#' @param  ...          further specifications for \link{plot}.
#' @param  name_cluster Choose the cluster to plot
#' @param  title        title of plot. default is ""Confusion by cluster"".
#' @return              The plot itself.
#' @examples
#' \dontrun{
#' # Produce a cluster map
#'
#' samples_mt_2bands <- sits_select(samples_modis_4bands, bands = c("NDVI", "EVI"))
#' som_map <- sits_som_map(samples_mt_2bands)
#' # Evaluate the clusters
#' cluster_overall <- sits_som_evaluate_cluster(som_map)
#' # Plot confusion between the clusters
#' plot(cluster_overall)
#' }
#'
#' @export
#'
plot.som_evaluate_cluster <- function(x, y, ...,
                                      name_cluster = NULL,
                                      title = "Confusion by cluster") {
    stopifnot(missing(y))
    p <- .sits_plot_som_evaluate_cluster(x, name_cluster, title)
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
#' @param  x          Object of class "som_map"
#' @param  y          Ignored
#' @param  ...        Further specifications for \link{plot}.
#' @param  type       Type of plot: "codes" for neuron weight (time series) and
#'                    "mapping" for the number of samples allocated in a neuron.
#' @param  band       What band will be plotted.
#'
#' @return            The plot itself.
#'
#' @examples
#' \dontrun{
#' # Produce a cluster map
#' samples_mt_2bands <- sits_select(samples_modis_4bands, bands = c("NDVI", "EVI"))
#' som_map <- sits_som_map(samples_mt_2bands)
#' # Plot the clusters
#' plot(som_map, type = "codes")
#' # Plot kohonen map showing where the samples were allocated
#' plot(som_map, type = "mapping")
#' }
#'
#' @export
#'
plot.som_map <- function(x, y, ..., type = "codes", band = 1) {
    stopifnot(missing(y))
    .sits_plot_som_map(x, type, band)
}

#' @title  Plot Keras (deep learning) model
#' @name   plot.keras_model
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a deep learning model developed using keras
#'
#' @param  x             Object of class "keras_model"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @return               The plot itself.
#'
#' @examples
#' \dontrun{
#' # Get a set of samples
#' samples_ndvi_evi <- sits_select(samples_modis_4bands,
#'                                 bands = c("NDVI", "EVI"))
#'
#' # train a deep learning model
#' dl_model <- sits_train(samples_ndvi_evi, ml_method = sits_mlp())
#' plot(dl_model)
#' }
#'
#' @export
#'
plot.keras_model <- function(x, y, ...) {
    stopifnot(missing(y))
    plot(environment(x)$history)
}

#' @title Plot all intervals of one time series for the same lat/long together
#' @name .sits_plot_allyears
#' @keywords internal
#'
#' @description For each lat/long location in the data, join temporal
#' instances of the same place together for plotting.
#' @param data    One or more time series (stored in a sits tibble).
.sits_plot_allyears <- function(data) {
    locs <- dplyr::distinct(data, longitude, latitude)

    plots <- purrr::pmap(
        list(locs$longitude, locs$latitude),
        function(long, lat) {
            dplyr::filter(data, longitude == long, latitude == lat) %>%
                .sits_plot_ggplot_series() %>%
                graphics::plot()
        }
    )
    return(invisible(plots[[1]]))
}


#' @title Plot classification patterns
#' @name .sits_plot_patterns
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @description   Plots the patterns to be used for classification
#'                (code is reused from the dtwSat package by Victor Maus).
#' @param data    one or more time series containing patterns.
#'
#' @return        plot
#'
.sits_plot_patterns <- function(data) {
    # verifies if scales package is installed
    if (!requireNamespace("scales", quietly = TRUE)) {
        stop("Please install package scales.", call. = FALSE)
    }

    # put the time series in the data frame
    plot.df <- purrr::pmap_dfr(
        list(data$label, data$time_series),
        function(label, ts) {
            lb <- as.character(label)
            # extract the time series and convert
            df <- data.frame(Time = ts$Index, ts[-1], Pattern = lb)
        }
    )

    plot.df <- tidyr::pivot_longer(plot.df, cols = sits_bands(data))

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

#' @title Plot a set of time series for the same spatio-temporal reference
#'
#' @name .sits_plot_together
#' @keywords internal
#'
#' @description Plots all time series for the same label together.
#' This function is useful to find out the spread of the values of
#' the time series for a given label.
#'
#' @param    data    A sits tibble with the list of time series to be plotted.
#' @return           The plot itself.
.sits_plot_together <- function(data) {
    # create a data frame with the median, and 25% and 75% quantiles
    create_iqr <- function(dt, band) {
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
            data2 <- dplyr::filter(data, label == lb)
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
                    dt <- Reduce(function(...) merge(..., all = T), dt_lst)

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

#' @title Plot one timeSeries using ggplot
#'
#' @name .sits_plot_ggplot_series
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot. This function is used
#' for showing the same lat/long location in a series of time steps.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            The plot itself.
.sits_plot_ggplot_series <- function(row) {
    # Are there NAs in the data?
    if (any(is.na(row$time_series[[1]]))) {
        g <- .sits_plot_ggplot_series_na(row)
    } else {
        g <- .sits_plot_ggplot_series_no_na(row)
    }
    return(g)
}
#' @title Plot one timeSeries using ggplot (no NAs present)
#'
#' @name .sits_plot_ggplot_series_no_na
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot in the case the series
#'              has no NA values.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            The plot itself.
.sits_plot_ggplot_series_no_na <- function(row) {
    # create the plot title
    plot_title <- .sits_plot_title(row$latitude, row$longitude, row$label)
    #
    colors <- grDevices::hcl.colors(n = 20, palette = "Harmonic", alpha = 1, rev = TRUE)
    # extract the time series
    data_ts <- dplyr::bind_rows(row$time_series)
    # melt the data into long format
    melted_ts <- data_ts %>%
        tidyr::pivot_longer(cols = -Index, names_to = "variable") %>%
        as.data.frame()
    # plot the data with ggplot
    g <- ggplot2::ggplot(melted_ts, ggplot2::aes(
        x = Index,
        y = value,
        group = variable
    )) +
        ggplot2::geom_line(ggplot2::aes(color = variable)) +
        ggplot2::labs(title = plot_title) +
        ggplot2::scale_fill_manual(palette = colors)
    return(g)
}
#' @title Plot one timeSeries wih NAs using ggplot
#'
#' @name .sits_plot_ggplot_series_na
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot, showing where NAs are.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            The plot itself.
.sits_plot_ggplot_series_na <- function(row) {

    # verifies if tidyr package is installed
    if (!requireNamespace("tidyr", quietly = TRUE)) {
        stop("Please install package tidyr", call. = FALSE)
    }

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
        dplyr::transmute(cld = replace_na(X1)) %>%
        dplyr::bind_cols(data, .)

    # prepare tibble to ggplot (fortify)
    ts1 <- tidyr::pivot_longer(data, -Index)
    g <- ggplot2::ggplot(data = ts1 %>%
                             dplyr::filter(name != "cld")) +
        ggplot2::geom_col(ggplot2::aes(x = Index, y = value),
                          fill = "sienna",
                          alpha = 0.3,
                          data = ts1 %>%
                              dplyr::filter(name == "cld", !is.na(value))
        ) +
        ggplot2::geom_line(ggplot2::aes(x = Index, y = value, color = name)) +
        ggplot2::geom_point(ggplot2::aes(x = Index, y = value, color = name)) +
        ggplot2::labs(title = plot_title)

    return(g)
}

#' @title Plot many timeSeries together using ggplot
#'
#' @name .sits_plot_ggplot_together
#' @keywords internal
#'
#' @description Plots a set of  time series together.
#'
#' @param melted         tibble with the time series (already melted).
#' @param means          means and std deviations of the time series.
#' @param plot_title     title for the plot.
#' @return               The plot itself.
.sits_plot_ggplot_together <- function(melted, means, plot_title) {
    g <- ggplot2::ggplot(data = melted, ggplot2::aes(
        x = Index,
        y = value,
        group = variable
    )) +
        ggplot2::geom_line(colour = "#819BB1", alpha = 0.5) +
        ggplot2::labs(title = plot_title) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = Index, y = med),
            colour = "#B16240", size = 2, inherit.aes = FALSE
        ) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = Index, y = qt25),
            colour = "#B19540", size = 1, inherit.aes = FALSE
        ) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = Index, y = qt75),
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

#' @title Plot time series classification results
#' @name .sits_plot_predicted_ts
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @description        plots the classification results
#'                     (code reused from the dtwSat package by Victor Maus).
#' @param data         sits tibble with classified time series.
#' @param bands        band for plotting the classification.
#' @param palette      hcl palette used for visualisation
#'
#' @return             The plot itself.
.sits_plot_predicted_ts <- function(data, bands, palette) {

    # verifies if scales package is installed
    if (!requireNamespace("scales", quietly = TRUE)) {
        stop("Please install package scales.", call. = FALSE)
    }
    if (purrr::is_null(bands)) {
        bands <- sits_bands(data)
    }
    if (!all(bands %in% sits_bands(data)))
        bands <- sits_bands(data)
    # configure plot colors
    # get labels from predicted tibble
    labels <- unique(data$predicted[[1]]$class)
    colors <- .config_colors(labels = labels,
                             palette = palette,
                             rev = FALSE)

    # put the time series in the data frame
    g_lst <- purrr::pmap(
        list(
            data$latitude, data$longitude, data$label,
            data$time_series, data$predicted
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
            df_x <- tidyr::pivot_longer(df_x, cols = -c("Time", "Series"), names_to = "variable")
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
                ggplot2::scale_color_brewer(palette = "Set1")  +
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
    return(invisible(g_lst[[1]]))
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
#' @param palette       hcl color palette
#'
#' @return              The plot itself.
.sits_plot_dendrogram <- function(data,
                                  cluster,
                                  cutree_height,
                                  palette) {

    # set caller to show in errors
    .check_set_caller(".sits_plot_dendrogram")

    # verifies if dendextend package is installed
    if (!requireNamespace("dendextend", quietly = TRUE)) {
        stop("Please install package dendextend.", call. = FALSE)
    }
    # verifies if methods package is installed
    if (!requireNamespace("methods", quietly = TRUE)) {
        stop("Please install package methods.", call. = FALSE)
    }
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
    colors <- .config_colors(labels = data_labels,
                             palette = palette,
                             rev = TRUE)
    colors_clust <- colors[data_labels]

    # set the visualisation params for dendrogram
    dend <- dend %>%
        dendextend::set(
            what = "labels",
            value = character(length = length(data_labels))) %>%
        dendextend::set(
            what = "branches_k_color",
            value = colors_clust,
            k = length(data_labels))

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


#' @title  Plot the SOM grid with neurons labeled
#' @name   .sits_plot_som_map
#' @keywords internal
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#' @description Given a kohonen object with a set of time neurons, plot them.
#'
#' The plot function produces different plots based on the input data:
#' \itemize{
#'  \item{"codes": }{Plot the vector weight for each neuron.}
#'  \item{"mapping": }{Shows where samples are mapped.}
#' }
#'
#' @param  koh        SOM map produced by "sits_som_map" function
#' @param  type       Type of plot ("codes" or "mapping")
#' @param  band       What band will be plotted
#'
.sits_plot_som_map <- function(koh, type = "codes", band = 1) {
    # Sanity check
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
    }
    else if (type == "codes") {
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

#' @title  Plot information about confusion between clusters
#'
#' @name   .sits_plot_som_evaluate_cluster
#' @keywords internal
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#'
#' @description Plot a bar graph with information about each cluster.
#' The percentage of mixture between the clusters.
#'
#' @param data          Percentage of mixture between the clusters
#' @param cluster_name  Choose the cluster to plot
#' @param title         Title of plot.
#'
#' @return              ggplot2 object
.sits_plot_som_evaluate_cluster <- function(data,
                                            cluster_name = NULL,
                                            title = "Confusion by cluster") {
    if (!inherits(data, "som_evaluate_cluster")) {
        message("unable to plot - please run sits_som_evaluate_cluster")
        return(invisible(NULL))
    }

    # Filter the cluster to plot
    if (!(is.null(cluster_name))) {
        data <- dplyr::filter(data, cluster %in% cluster_name)
    }
    # configure plot colors
    # get labels from cluster table
    labels <- unique(data$class)
    colors <- .config_colors(labels = labels,
                             palette = "Spectral",
                             rev = TRUE)

    p <- ggplot2::ggplot() +
        ggplot2::geom_bar(
            ggplot2::aes(
                y = mixture_percentage,
                x = cluster,
                fill = class
            ),
            data = data,
            stat = "identity",
            position = ggplot2::position_dodge()
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x =
                           ggplot2::element_text(angle = 60, hjust = 1)) +
        ggplot2::labs(x = "Cluster", y = "Percentage of mixture") +
        ggplot2::scale_fill_manual(name = "Class label", values = colors) +
        ggplot2::ggtitle(title)

    p <- graphics::plot(p)
    return(invisible(p))
}
#' @title Plot a raster classified image
#'
#' @name .sits_plot_classified_image
#' @keywords internal
#'
#' @description plots a raster using ggplot. This function is used
#' for showing the same lat/long location in a series of time steps.
#'
#' @param cube             metadata for a labelled data cube.
#' @param time             temporal reference for plot.
#' @param title            title of the plot
#' @param legend           named vector that associates labels to colors.
#' @param palette          palette (one of grDevices::hcl.pals())
#' @param rev              revert the order of hcl palette (TRUE/FALSE)
.sits_plot_classified_image <- function(cube,
                                        time,
                                        title,
                                        legend,
                                        palette,
                                        rev) {


    # set caller to show in errors
    .check_set_caller(".sits_plot_classified_image")

    #precondition 1 - cube must be a labelled cube
    .check_chr_within(
        x = "classified_image",
        within = class(cube),
        discriminator = "any_of",
        msg = "cube must be a classified image")
    #precondition 2 - time must be a positive integer
    .check_that(x = time >= 1,
                msg = paste("time must be a positive integer")
    )

    # get the raster object
    r <- suppressWarnings(terra::rast(.file_info_path(cube)))

    # convert from raster to points
    df <- terra::as.data.frame(r, xy = TRUE)
    # define the column names for the data frame
    colnames(df) <- c("x", "y", "class")

    # get the labels and how many there are
    labels <- sits_labels(cube)
    nclasses <- length(labels)
    # create a mapping from classes to labels
    names(labels) <- as.character(c(1:nclasses))

    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .config_colors(labels = labels,
                                 palette = palette,
                                 rev = rev)
    }
    else {
        .check_chr_within(
            x = labels,
            within = names(legend),
            discriminator = "all_of",
            msg = "some labels are missing from the legend")

        colors <- unname(legend[labels])
    }
    # set the names of the color vector
    names(colors) <- as.character(c(1:nclasses))

    # plot the data with ggplot
    g <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_raster(ggplot2::aes(fill = factor(class))) +
        ggplot2::labs(title = title) +
        ggplot2::scale_fill_manual(values = colors,
                                   labels = labels,
                                   guide = ggplot2::guide_legend(
                                       title = "Classes")
        )

    graphics::plot(g)
    return(g)
}

#' @title Plot classification alignments using the dtwSat package
#' @name .sits_plot_twdtw_alignments
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description        Plots the alignments from TWDTW classification
#' @param matches      A list of dtwSat S4 match objects
#'                     (produced by sits_TWDTW_matches).
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
#' @param  matches      dtwSatS4 matches objects produced by sits_TWDTW_matches.
#' @param  start_date   Start date of the plot (used for classifications).
#' @param  end_date     End date of the plot (used for classifications).
#' @param  interval     Interval between classifications.
#' @param  overlap      Minimum overlapping between one match
#'                      and the interval of classification.
#'                      For details see dtwSat::twdtwApply help.
.sits_plot_twdtw_class <- function(matches,
                                   start_date = NULL,
                                   end_date = NULL,
                                   interval = "12 month",
                                   overlap = 0.5) {
    # verifies if dtwSat package is installed
    if (!requireNamespace("dtwSat", quietly = TRUE)) {
        stop("Please install package dtwSat", call. = FALSE)
    }

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
