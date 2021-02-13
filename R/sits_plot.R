#' @title  Generic interface for ploting time series
#' @method plot sits
#' @name plot
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This is a generic function. Parameters depend on the specific
#' type of input.  See each function description for the
#' required parameters:
#' \itemize{
#'  \item{sits tibble: }                 {see \code{\link{plot.sits}}}
#'  \item{patterns: }                    {see \code{\link{plot.patterns}}}
#'  \item{SOM map: }                     {see \code{\link{plot.som_map}}}
#'  \item{classified time series: }      {see \code{\link{plot.predicted}}}
#'  \item{brick  cube: }                 {see \code{\link{plot.brick_cube}}}
#'  \item{stack  cube: }                 {see \code{\link{plot.stack_cube}}}
#'  \item{classification probabilities: }{see \code{\link{plot.probs_cube}}}
#'  \item{classified image: }            {see \code{\link{plot.classified_image}}}
#'  \item{SOM evaluate cluster: }        {see \code{\link{plot.som_evaluate_cluster}}}
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
#' @param  colors       Color palette to be used (based on Color Brewer
#'                      - default is "Dark2").
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
#' @export
plot.sits <- function(x, y, ..., colors = "Dark2") {
    stopifnot(missing(y))

    # Are there more than 30 samples? Plot them together!
    if (nrow(x) > 30) {
          p <- .sits_plot_together(x, colors)
      } # If no conditions are met, take "allyears" as the default
    else {
          p <- .sits_plot_allyears(x, colors)
      }
    # return the plot
    return(invisible(p))
}

#' @title  Generic interface for ploting patterns
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
#' @export
plot.patterns <- function(x, y, ...) {
    stopifnot(missing(y))
    p <- .sits_plot_patterns(x)
    return(invisible(p))
}

#' @title  Generic interface for ploting time series predictions
#' @name   plot.predicted
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Given a sits tibble with a set of predictions, plot them
#'
#' @param  x             object of class "predicted"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param  bands         bands used for visualisation
#' @return               The plot itself.
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for Mato Grosso region (provided by EMBRAPA)
#' samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#' # classify the point
#' model_svm <- sits_train(samples_mt_ndvi, ml_method = sits_svm())
#' class_ndvi.tb <- sits_classify(point_ndvi, model_svm)
#' # plot the classification
#' plot(class_ndvi.tb)
#' }
#' @export
plot.predicted <- function(x, y, ..., bands = "NDVI") {
    stopifnot(missing(y))
    p <- .sits_plot_classification(x, bands)
    return(invisible(p))
}
#' @title  Generic interface for plotting brick cubes
#' @name   plot.brick_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a stack cube using terra
#'
#' @param  x             object of class "brick_cube"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param  red           band for red color.
#' @param  green         band for green color.
#' @param  blue          band for blue color.
#' @param  time          temporal instance to be plotted
#'
#' @return               mapview object
#'
#' @examples
#' \dontrun{
#' # retrieve two files with NDVI and EVI from MODIS
#' ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
#'     package = "sits"
#' ))
#' evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
#'     package = "sits"
#' ))
#' # retrieve the timeline
#' data("timeline_2013_2014")
#' # create a data cube
#' sinop_2014 <- sits_cube(
#'     type = "BRICK",
#'     name = "sinop-2014",
#'     timeline = timeline_2013_2014,
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     bands = c("NDVI", "EVI"),
#'     files = c(ndvi_file, evi_file)
#' )
#' # plot the data cube
#' plot(sinop_2014, red = "EVI", green = "EVI", blue = "EVI")
#' }
#'
#' @export
plot.brick_cube <- function(x, y, ..., red, green, blue, time = 1) {
    stopifnot(missing(y))
    # verifies if mapview package is installed
    if (!requireNamespace("mapview", quietly = TRUE)) {
        stop("Please install package mapview.", call. = FALSE)
    }
    # verifies if raster package is installed
    if (!requireNamespace("raster", quietly = TRUE)) {
        stop("Please install package raster.", call. = FALSE)
    }
    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "GeoportailFrance.orthos",
        "Esri.WorldImagery"
    ))

    # get information about bands and files
    file_info <- x[1,]$file_info[[1]]

    # is there a cloud band?
    # remove the cloud band from the file information
    bands <- .sits_config_bands_no_cloud(x[1,])
    file_info <- dplyr::filter(file_info, band %in% bands)


    # index to assign which bands to plot
    index <- .sits_plot_rgb_brick(
        bands = bands,
        timeline = sits_timeline(x),
        red = toupper(red),
        green = toupper(green),
        blue = toupper(blue),
        time = time
    )

    # is the data set a stack or a brick

    # use the raster package to obtain a "rast" object from a brick
    rast <- suppressWarnings(raster::stack(file_info$path))
    assertthat::assert_that(raster::ncol(rast) > 0 & raster::nrow(rast) > 1,
                      msg = "plot.brick_cube: unable to retrive raster data"
    )

    # plot the RGB file
    mv <- suppressWarnings(mapview::viewRGB(rast,
                                            r = index["red"],
                                            g = index["green"],
                                            b = index["blue"]
    ))
    return(mv)
}
#' @title  Generic interface for plotting stack cubes
#' @name   plot.stack_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a stack cube using terra
#'
#' @param  x             object of class "stack_cube"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param  red           band for red color.
#' @param  green         band for green color.
#' @param  blue          band for blue color.
#' @param  time          temporal instances to be plotted.
#' @param  roi           sf object giving a region of interest.
#'
#' @return               mapview object
#'
#' @examples
#' \dontrun{
#' data_dir <- system.file("extdata/raster/cbers", package = "sits")
#'
#' cbers_022024 <- sits_cube(
#'     type = "STACK",
#'     name = "cbers_022024",
#'     satellite = "CBERS-4",
#'     sensor = "AWFI",
#'     resolution = "64m",
#'     data_dir = data_dir,
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#' # plot the data cube
#' plot(cbers_022024, red = "B15", green = "B16", blue = "B13", time = 1)
#' }
#'
#' @export
plot.stack_cube <- function(x, y, ..., red, green, blue, time = 1, roi = NULL) {

    stopifnot(missing(y))

    # verifies if mapview package is installed
    if (!requireNamespace("mapview", quietly = TRUE)) {
        stop("Please install package mapview.", call. = FALSE)
    }
    # verifies if raster package is installed
    if (!requireNamespace("raster", quietly = TRUE)) {
        stop("Please install package raster.", call. = FALSE)
    }
    # verify sf package if roi is informed
    if (!purrr::is_null(roi)) {
        if (!requireNamespace("sf", quietly = TRUE)) {
            stop("Please install package sf.", call. = FALSE)
        }

        # filter only intersecting tiles
        intersects <- slider::slide(x, function(row) {

            .sits_raster_sub_image_intersects(row, roi)
        }) %>% unlist()

        if (!any(intersects)) {
            stop("Informed roi does not intersect cube.", call. = FALSE)
        }
        x <- x[intersects,]
    }

    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "GeoportailFrance.orthos",
        "Esri.WorldImagery"
    ))

    # plot only the first tile
    # get information about bands and files
    file_info <- x[1,]$file_info[[1]]

    # is there a cloud band?
    # remove the cloud band from the file information
    bands <- .sits_config_bands_no_cloud(x[1,])
    file_info <- dplyr::filter(file_info, band %in% bands)

    # index to assign which bands to plot
    index <- .sits_plot_rgb_stack(
        bands = bands,
        timeline = sits_timeline(x),
        red = toupper(red),
        green = toupper(green),
        blue = toupper(blue),
        time = time
    )

    # use the raster package to obtain a raster object from a stack
    rast <- suppressWarnings(raster::stack(file_info$path[index]))

    if (!purrr::is_null(roi)) {

        roi <- raster::extent(sf::st_bbox(
            sf::st_transform(roi, crs = raster::crs(rast))))

        rast <- suppressWarnings(raster::crop(rast, roi))

    }

    assertthat::assert_that(raster::ncol(rast) > 0 & raster::nrow(rast) > 1,
                    msg = "plot.stack_cube: unable to retrieve raster data"
    )

    # plot the RGB file
    mv <- suppressWarnings(mapview::viewRGB(
        rast, r = 1, g = 2, b = 3,
        layer.name = paste0("Time ", time)))

    return(mv)
}
#' @title  Generic interface for plotting probability cubes
#' @name   plot.probs_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a probability cube using stars
#'
#' @param  x             object of class "probs_image"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param time           temporal reference for plot.
#' @param breaks         type of breaks
#' @param title          string.
#' @param colors         color palette.
#' @param n_colors       number of colors.
#'
#' @return               The plot itself.
#'
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select the bands for classification
#' samples_ndvi_evi <- sits_select(samples_mt_4bands, bands = c("EVI", "NDVI"))
#' # build the classification model
#' rfor_model <- sits_train(samples_ndvi_evi, sits_rfor(num_trees = 2000))
#'
#' # select the bands "ndvi", "evi" provided by the SITS package
#' ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
#'     package = "sits"
#' ))
#' evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
#'     package = "sits"
#' ))
#' # select the timeline
#' data("timeline_2013_2014")
#' # build a data cube from files
#'
#' sinop_2014 <- sits_cube(
#'     type = "BRICK",
#'     name = "sinop-2014",
#'     timeline = timeline_2013_2014,
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     bands = c("ndvi", "evi"),
#'     files = c(ndvi_file, evi_file)
#' )
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop_2014,
#'     rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4,
#'     multicores = 2
#' )
#'
#' plot(sinop_probs)
#' }
#' @export
plot.probs_cube <- function(x, y, ..., time = 1,
                            title = "Probabilities for Classes",
                            breaks = "kmeans",
                            colors = "YlGnBu",
                            n_colors = 10) {
    stopifnot(missing(y))
    # verifies if stars package is installed
    if (!requireNamespace("stars", quietly = TRUE)) {
        stop("Please install package stars.", call. = FALSE)
    }
    # define the output color pallete
    col <- grDevices::hcl.colors(10, colors, rev = TRUE)
    # create a stars object
    st <- stars::read_stars(x$file_info[[1]]$path[[time]])

    p <- plot(st,
              breaks = breaks,
              nbreaks = 11,
              col = col,
              main = x$labels[[1]]
    )
    return(invisible(p))
}


#' @title  Generic interface for ploting classified images
#' @name   plot.classified_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a classified raster using ggplot.
#'
#' @param  x             object of class "classified_image"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @param  map           map to overlay (mapview object)
#' @param  time          temporal reference for plot.
#' @param  title         string.
#' @param  colors        color pallete.
#'
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select the bands for classification
#' samples_ndvi_evi <- sits_select(samples_mt_4bands, bands = c("EVI", "NDVI"))
#' # build the classification model
#' xgb_model <- sits_train(samples_ndvi_evi, ml_method = sits_xgboost())
#'
#' # select the bands "ndvi", "evi" provided by the SITS package
#' ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
#'     package = "sits"
#' ))
#' evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
#'     package = "sits"
#' ))
#' # select the timeline
#' data("timeline_2013_2014")
#' # build a data cube from files
#'
#' sinop_2014 <- sits_cube(
#'     type = "BRICK", name = "sinop-2014",
#'     timeline = timeline_2013_2014,
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     bands = c("ndvi", "evi"),
#'     files = c(ndvi_file, evi_file)
#' )
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop_2014, xgb_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#' # smooth the result with a bayesian filter
#'
#' sinop_bayes <- sits_smooth(sinop_probs,
#'     output_dir = tempdir())
#'
#' sinop_label <- sits_label_classification(sinop_bayes,
#'     output_dir = tempdir()
#' )
#'
#' # plot the smoothened image
#' plot(sinop_label, title = "Sinop-Bayes")
#' }
#' @export
plot.classified_image <- function(x, y, ..., map = NULL, time = 1,
                                  title = "Classified Image", colors = NULL) {
    stopifnot(missing(y))
    # verifies if mapview package is installed
    if (!requireNamespace("mapview", quietly = TRUE)) {
        stop("Please install package mapview.", call. = FALSE)
    }
    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "GeoportailFrance.orthos",
        "Esri.WorldImagery"
    ))

    # get the labels
    labels <- .sits_cube_labels(x)

    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(colors)) {
        colors <- .sits_config_colors(labels)
    }

    # obtain the raster
    rl <- suppressWarnings(raster::raster(x$file_info[[1]]$path[time]))
    assertthat::assert_that(raster::ncol(rl) > 0 & raster::nrow(rl) > 1,
        msg = "plot.classified_image: unable to retrive raster data"
    )
    # create a RAT
    rl <- raster::ratify(rl)
    rat <- raster::levels(rl)[[1]]
    # include labels in the RAT
    # be careful - some labels may not exist in the classified image
    rat$landcover <- labels[rat$ID]
    colors <- colors[rat$ID]
    # assign the RAT to the raster object
    levels(rl) <- rat

    # use mapview
    if (!purrr::is_null(map))
          mv <- suppressWarnings(
            mapview::mapview(rl,
                             map = map,
                             col.regions = colors)
            )
    else
          mv <- suppressWarnings(
            mapview::mapview(rl,
                             col.regions = colors)
            )

    return(mv)
}

#' @title  Plot information about confunsion between clusters
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
#' samples_mt_2bands <- sits_select(samples_mt_6bands, bands = c("NDVI", "EVI"))
#' som_map <- sits_som_map(samples_mt_2bands)
#' # Evaluate the clusters
#' cluster_overall <- sits_som_evaluate_cluster(som_map)
#' # Plot confusion between the clusters
#' plot(cluster_overall)
#' }
#' @export
plot.som_evaluate_cluster <- function(x, y, ..., name_cluster = NULL, title = "Confusion by cluster") {
  stopifnot(missing(y))
  p <- .sits_plot_som_evaluate_cluster(x, name_cluster, title)
  return(invisible(p))
}
#' @title  Generic interface for plotting a SOM map
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
#' @param  whatmap    What data layer will be plotted.
#'
#' @return            The plot itself.
#'
#' @examples
#' \dontrun{
#' # Produce a cluster map
#' samples_mt_2bands <- sits_select(samples_mt_6bands, bands = c("NDVI", "EVI"))
#' som_map <- sits_som_map(samples_mt_2bands)
#' # Plot the clusters
#' plot(som_map, type = "codes")
#' # Plot kohonen map showing where the samples were allocated
#' plot(som_map, type = "mapping")
#' }
#' @export
plot.som_map <- function(x, y, ..., type = "codes", whatmap = 1) {
    stopifnot(missing(y))
    .sits_plot_som_map(x, type, whatmap)
}

#' @title  Generic interface for plotting a Keras model
#' @name   plot.keras_model
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description plots a deep learning model
#'
#' @param  x             Object of class "keras_model"
#' @param  y             ignored
#' @param  ...           further specifications for \link{plot}.
#' @return               The plot itself.
#'
#' @examples
#' \donttest{
#' # Get a set of samples
#' samples_ndvi_evi <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
#'
#' # train a deep learning model
#' dl_model <- sits_train(samples_ndvi_evi, ml_method = sits_deeplearning(
#'     layers = c(512, 512, 512),
#'     activation = "relu",
#'     dropout_rates = c(0.50, 0.40, 0.35),
#'     epochs = 100,
#'     batch_size = 128,
#'     validation_split = 0.2
#' ))
#' plot(dl_model)
#' }
#' @export
plot.keras_model <- function(x, y, ...) {
    stopifnot(missing(y))
    p <- graphics::plot(environment(x)$history)
    return(invisible(p))
}

#' @title Plot all intervals of one time series for the same lat/long together
#' @name .sits_plot_allyears
#' @keywords internal
#'
#' @description For each lat/long location in the data, join temporal
#' instances of the same place together for plotting.
#' @param data    One or more time series (stored in a sits tibble).
#' @param colors  The color pallete to be used (default is "Set2").
.sits_plot_allyears <- function(data, colors) {
    locs <- dplyr::distinct(data, longitude, latitude)

    plots <- purrr::pmap(
        list(locs$longitude, locs$latitude),
        function(long, lat) {
            dplyr::filter(data, longitude == long, latitude == lat) %>%
                .sits_ggplot_series(colors) %>%
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
    # prepare a data frame for plotting
    plot.df <- data.frame()

    # put the time series in the data frame
    purrr::pmap(
        list(data$label, data$time_series),
        function(label, ts) {
            lb <- as.character(label)
            # extract the time series and convert
            df <- data.frame(Time = ts$Index, ts[-1], Pattern = lb)
            plot.df <<- rbind(plot.df, df)
        }
    )

    plot.df <- reshape2::melt(plot.df, id.vars = c("Time", "Pattern"))

    # Plot temporal patterns
    gp <- ggplot2::ggplot(plot.df, ggplot2::aes_string(
        x = "Time",
        y = "value",
        colour = "variable"
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
#' @param    colors  The color pallete to be used (default is "Set1").
#' @return           The plot itself.
.sits_plot_together <- function(data, colors) {
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
        g <- .sits_ggplot_together(dt_melted, dt_qts, title)
        p <- graphics::plot(g)
        return(p)
    }

    # how many different labels are there?
    labels <- sits_labels(data)$label

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
            ref_dates <- sits_time_series_dates(data2)
            # align all time series to the same dates
            data2 <- .sits_align_dates(data2, ref_dates)

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
#' @name .sits_ggplot_series
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot. This function is used
#' for showing the same lat/long location in a series of time steps.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @param colors      brewer colors to be used for plotting.
#' @return            The plot itself.
.sits_ggplot_series <- function(row, colors = "Dark2") {
    # Are there NAs in the data?
    if (any(is.na(row$time_series[[1]]))) {
          g <- .sits_ggplot_series_na(row, colors)
      } else {
          g <- .sits_ggplot_series_no_na(row, colors)
      }
    return(g)
}
#' @title Plot one timeSeries using ggplot (no NAs present)
#'
#' @name .sits_ggplot_series_no_na
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot in the case the series
#'              has no NA values.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @param colors      brewer colors to be used for plotting.
#' @return            The plot itself.
.sits_ggplot_series_no_na <- function(row, colors = "Dark2") {
    # create the plot title
    plot_title <- .sits_plot_title(row$latitude, row$longitude, row$label)
    # extract the time series
    data_ts <- row$time_series
    # melt the data into long format
    melted_ts <- data_ts %>%
        reshape2::melt(id.vars = "Index") %>%
        as.data.frame()
    # plot the data with ggplot
    g <- ggplot2::ggplot(melted_ts, ggplot2::aes(
        x = Index,
        y = value,
        group = variable
    )) +
        ggplot2::geom_line(ggplot2::aes(color = variable)) +
        ggplot2::labs(title = plot_title) +
        ggplot2::scale_color_brewer(palette = colors)
    return(g)
}
#' @title Plot one timeSeries wih NAs using ggplot
#'
#' @name .sits_ggplot_series_na
#' @keywords internal
#'
#' @description Plots a set of time series using ggplot, showing where NAs are.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @param colors      brewer colors to be used for plotting.
#' @return            The plot itself.
.sits_ggplot_series_na <- function(row, colors = "Dark2") {

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
#' @name .sits_ggplot_together
#' @keywords internal
#'
#' @description Plots a set of  time series together.
#'
#' @param melted         tibble with the time series (already melted).
#' @param means          means and std deviations of the time series.
#' @param plot_title     title for the plot.
#' @return               The plot itself.
.sits_ggplot_together <- function(melted, means, plot_title) {
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

#' @title Plot classification results
#' @name .sits_plot_classification
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @description        plots the classification results
#'                     (code reused from the dtwSat package by Victor Maus).
#' @param data         sits tibble with classified time series.
#' @param bands        band for plotting the classification.
#'
#' @return             The plot itself.
.sits_plot_classification <- function(data, bands = NULL) {
    if (purrr::is_null(bands)) {
          bands <- sits_bands(data)[1]
      }
    # bands in SITS are in uppercase
    bands <- toupper(bands)

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
            df_x <- reshape2::melt(df_x, id.vars = c("Time", "Series"))
            # define a nice set of breaks for value plotting
            y_labels <- scales::pretty_breaks()(range(df_x$value,
                na.rm = TRUE
            ))
            y_breaks <- y_labels

            # get the predicted values as a tibble
            df_pol <- data.frame()

            # create a data frame with values and intervals
            i <- 1
            purrr::pmap(
                list(
                    row_predicted$from, row_predicted$to,
                    row_predicted$class
                ),
                function(rp_from, rp_to, rp_class) {
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
                    i <<- i + 1
                    df_pol <<- rbind(df_pol, df_p)
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
                ggplot2::scale_fill_brewer(palette = "Set3") +
                ggplot2::geom_line(
                    data = df_x,
                    ggplot2::aes_string(
                        x = "Time",
                        y = "value",
                        colour = "variable"
                    )
                ) +
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
#' @param cluster_obj   cluster object produced by `sits_cluster` function.
#' @param cutree_height dashed horizontal line to be drawn
#'                      indicating the height of dendrogram cutting.
#' @param colors        color scheme as per `sits_color_name` function.
#'
#' @return              The plot itself.
.sits_plot_dendrogram <- function(data,
                                  cluster_obj,
                                  cutree_height = NULL,
                                  colors = "RdYlGn") {

    # verifies if dendextend package is installed
    if (!requireNamespace("dendextend", quietly = TRUE)) {
        stop("Please install package dendextend.", call. = FALSE)
    }
    # verifies if methods package is installed
    if (!requireNamespace("methods", quietly = TRUE)) {
        stop("Please install package methods.", call. = FALSE)
    }


    # ensures that a cluster object  exists
    assertthat::assert_that(!purrr::is_null(cluster_obj),
        msg = "plot_dendrogram: no valid cluster object available"
    )

    # get unique labels
    data_labels <- data$label
    u_lb <- base::unique(data_labels)

    # warns if the number of available colors is insufficient to all labels
    if (length(u_lb) > (
        length(.sits_brewer_rgb[[.sits_color_name(colors)]]) - 1)) {
          message("sits_plot_dendrogram: The number of labels
                is greater than the number of available colors.")
      }

    # extract the dendrogram object
    hclust_cl <- methods::S3Part(cluster_obj, strictS3 = TRUE)
    dend <- hclust_cl %>% stats::as.dendrogram()

    # prepare labels color vector
    cols <- character(length(data_labels))
    cols[] <- grDevices::rgb(0 / 255, 0 / 255, 0 / 255, 0 / 255)

    i <- 1
    seq(u_lb) %>%
        purrr::map(function(i) {
            cols[data_labels[cluster_obj$order] == u_lb[i]] <<-
              .sits_brewer_rgb[[.sits_color_name(colors)]][[length(u_lb)]][[i]]
            i <<- i + 1
        })

    # plot the dendrogram
    dend <- dendextend::set(
        dend, "labels",
        character(length = length(data_labels))
    )
    dend <- dendextend::set(dend, "branches_k_color",
        value = cols,
        k = length(data_labels)
    )

    p <- graphics::plot(dend,
        ylab = paste(
            tools::file_path_sans_ext(cluster_obj@method),
            "linkage distance"
        )
    )

    # plot cutree line
    if (!purrr::is_null(cutree_height)) {
          graphics::abline(h = cutree_height, lty = 2)
      }

    # plot legend
    graphics::legend("topright",
        fill = as.character(
            .sits_brewer_rgb[[.sits_color_name(colors)]][[length(u_lb)]]
        ),
        legend = u_lb
    )

    return(invisible(p))
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
#' @param  koh        SOM map produced by "sits_som_map" function
#' @param  type       Type of plot ("codes" or "mapping")
#' @param  whatmap    What data layer will be plotted.
.sits_plot_som_map <- function(koh, type = "codes", whatmap = 1) {
    # Sanity check
    if (!("som_map" %in% class(koh))) {
        message("wrong input data; please run sits_som_map first")
        return(invisible(NULL))
    }
    if (type == "mapping") {
        graphics::plot(koh$som_properties,
            bgcol = koh$som_properties$paint_map,
            "mapping", whatmap = whatmap
        )
    }
    else if (type == "codes") {
        graphics::plot(koh$som_properties,
            bgcol = koh$som_properties$paint_map,
            "codes", whatmap = whatmap
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
#' @name   .sits_plot_som_evaluate_cluster
#' @keywords internal
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#'
#' @description Plot a bar graph with information about each cluster.
#' The percentage of mixture between the clusters.
#'
#' @param data          Percentage of mixture between the clusters
#' @param  name_cluster Choose the cluster to plot
#' @param title         Title of plot.
#' @return              ggplot2 object
.sits_plot_som_evaluate_cluster <- function(data, cluster_name = NULL, title = "Confusion by cluster") {
    if (!("som_evaluate_cluster" %in% class(data))) {
        message("unable to plot - please run sits_som_evaluate_cluster")
        return(invisible(NULL))
    }

    # Filter the cluster to plot
    if (!(is.null(cluster_name))){
      data <- dplyr::filter(data, cluster %in% cluster_name)
    }
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
      ggplot2::scale_fill_discrete(name = "Class label") +
      ggplot2::ggtitle(title)

    p <- graphics::plot(p)
    return(invisible(p))
}
#' @title  Assign RGB channels to into image layers with many time instance
#' @name   .sits_plot_rgb_brick
#' @keywords internal
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @description Obtain a vector with the correct layer to be plotted for
#' an RGB assignment of a multi-temporal brick cube
#'
#' @param bands      bands of the data cube (excludes cloud band)
#' @param timeline   timeline of the data cube
#' @param red        Band to be assigned to R channel
#' @param green      Band to be assigned to G channel
#' @param blue       Band to be assigned to G channel
#' @param time       Temporal instance to be plotted
#' @return           Named vector with the correct layers for RGB
.sits_plot_rgb_brick <- function(bands, timeline,
                                 red, green, blue, time) {

    # check if the selected bands are correct
    all_bands <- paste0(bands, collapse = " ")
    assertthat::assert_that(red %in% bands,
        msg = paste0("R channel should be one of ", all_bands)
    )
    assertthat::assert_that(green %in% bands,
        msg = paste0("G channel should be one of ", all_bands)
    )
    assertthat::assert_that(blue %in% bands,
        msg = paste0("B channel should be one of ", all_bands)
    )
    # find out the number of instances
    n_instances <- length(timeline)
    # check if the selected temporal instance exists
    assertthat::assert_that(time <= n_instances, msg = "time out of bounds")

    # locate the instances
    instances_lst <- purrr::map(c(red, green, blue),
          function(b) {
              inst <- grep(b, bands)
              return(n_instances * (inst - 1) + time)
    })

    # create a named vector to store the RGB instances
    index <- unlist(instances_lst)
    names(index) <- c("red", "green", "blue")

    return(index)
}
#' @title  Assign RGB channels to for raster stack cubes
#' @name   .sits_plot_rgb_stack
#' @keywords internal
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @description Obtain a vector with the correct layer to be plotted for
#' an RGB assignment of a multi-temporal brick cube
#'
#' @param bands      bands of the data cube (excludes cloud band)
#' @param timeline   timeline of the data cube
#' @param red        Band to be assigned to R channel
#' @param green      Band to be assigned to G channel
#' @param blue       Band to be assigned to G channel
#' @param time       Temporal instance to be plotted
#' @return           Named vector with the correct layers for RGB
.sits_plot_rgb_stack <- function(bands, timeline,
                                 red, green, blue, time) {

    # check if the selected bands are correct
    all_bands <- paste0(bands, collapse = " ")
    assertthat::assert_that(red %in% bands,
                          msg = paste0("R channel should be one of ", all_bands)
    )
    assertthat::assert_that(green %in% bands,
                          msg = paste0("G channel should be one of ", all_bands)
    )
    assertthat::assert_that(blue %in% bands,
                          msg = paste0("B channel should be one of ", all_bands)
    )
    # find out the number of instances
    n_instances <- length(timeline)
    # check if the selected temporal instance exists
    assertthat::assert_that(time <= n_instances,
                            msg = sprintf("Time '%s' is out of bounds.", time))

    # locate the instances
    instances_lst <- purrr::map(c(red, green, blue),
          function(b) {
              inst <- grep(b, bands)
              return((time - 1) * length(bands) + inst)
    })

    # create a named vector to store the RGB instances
    index <- unlist(instances_lst)
    names(index) <- c("red", "green", "blue")

    return(index)
}
