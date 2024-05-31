#' @title  View data cubes and samples in leaflet
#' @name sits_view
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses leaflet to visualize time series, raster cube and
#' classified images
#'
#' @param  x             Object of class "sits", "data.frame", "som_map",
#'                       "raster_cube" or "classified image".
#' @param  ...           Further specifications for \link{sits_view}.
#' @param  band          For plotting grey images.
#' @param  red           Band for red color.
#' @param  green         Band for green color.
#' @param  blue          Band for blue color.
#' @param  dates         Dates to be plotted.
#' @param  tiles         Tiles to be plotted (in case of a multi-tile cube).
#' @param  class_cube    Classified cube to be overlayed on top on image.
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Color palette (if colors not in legend nor
#'                       in sits default colors)
#' @param  opacity       Opacity of segment fill or class cube
#' @param  seg_color     Color for segment boundaries
#' @param  line_width    Line width for segments (in pixels)
#' @param  id_neurons    Neurons from the SOM map to be shown.
#'
#' @return               A leaflet object containing either samples or
#'                       data cubes embedded in a global map that can
#'                       be visualized directly in an RStudio viewer.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # view samples
#'     sits_view(cerrado_2classes)
#'     # create a local data cube
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     # view the data cube
#'     sits_view(modis_cube,
#'         band = "NDVI"
#'     )
#'     # train a model
#'     rf_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify the cube
#'     modis_probs <- sits_classify(
#'         data = modis_cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir()
#'     )
#'     # generate a map
#'     modis_label <- sits_label_classification(
#'         modis_probs,
#'         output_dir = tempdir()
#'     )
#'     # view the classified map
#'     sits_view(modis_label)
#'     # view the classified map with the B/W image
#'     sits_view(modis_cube,
#'         band = "NDVI",
#'         class_cube = modis_label,
#'         dates = sits_timeline(modis_cube)[[1]]
#'     )
#'     # view the classified map with the RGB image
#'     sits_view(modis_cube,
#'         red = "NDVI", green = "NDVI", blue = "NDVI",
#'         class_cube = modis_label,
#'         dates = sits_timeline(modis_cube)[[1]]
#'     )
#'     # create an uncertainty cube
#'     modis_uncert <- sits_uncertainty(
#'         cube = modis_probs,
#'         output_dir = tempdir()
#'     )
#'     # view the uncertainty cube
#'     sits_view(modis_uncert)
#' }
#' @export
sits_view <- function(x, ...) {
    # set caller to show in errors
    .check_set_caller("sits_view")
    UseMethod("sits_view", x)
}
#' @rdname   sits_view
#'
#' @export
sits_view.sits <- function(x, ...,
                           legend = NULL,
                           palette = "Harmonic") {
    .check_set_caller("sits_view_sits")
    # precondition
    .check_require_packages("leaflet")

    # check samples contains the expected columns
    .check_that(all(c("longitude", "latitude", "label") %in% colnames(x)))
    # create a leaflet for samples
    leaf_map <- .view_samples(
        samples = x,
        legend = legend,
        palette = palette
    )
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.data.frame <- function(x, ...,
                                 legend = NULL,
                                 palette = "Harmonic") {
    leaf_map <- sits_view.sits(x, legend, palette)
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.som_map <- function(x, ...,
                              id_neurons,
                              legend = NULL,
                              palette = "Harmonic") {
    .check_set_caller("sits_view_som_map")
    # check id_neuron
    .check_int_parameter(
        id_neurons,
        min = 1,
        max = max(unique(x[["labelled_neurons"]][["id_neuron"]])),
        len_min = 1,
        len_max = length(unique(x[["labelled_neurons"]][["id_neuron"]]))
    )
    # first select unique locations
    samples <- dplyr::filter(
        x[["data"]], .data[["id_neuron"]] %in% !!id_neurons
    )
    leaf_map <- .view_samples(
        samples = samples,
        legend = legend,
        palette = palette
    )
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.raster_cube <- function(x, ...,
                                  band = NULL,
                                  red = NULL,
                                  green = NULL,
                                  blue = NULL,
                                  tiles = x[["tile"]][[1]],
                                  dates = NULL,
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "RdYlGn",
                                  opacity = 0.7,
                                  seg_color = "black",
                                  line_width = 1) {
    # preconditions
    # Probs cube not supported
    .check_that(!inherits(x, "probs_cube"))
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # pre-condition for bands
    # # no band? take a default
    if (!(.has(band) || (.has(red) && .has(green) && .has(blue))))
        band <- sits_bands(x)[[1]]
    .check_bw_rgb_bands(band, red, green, blue)
    .check_available_bands(x, band, red, green, blue)
    # retrieve dots
    dots <- list(...)
    # deal with wrong parameter "date"
    if ("date" %in% names(dots) && missing(dates)) {
        dates <- as.Date(dots[["date"]])
    }
    # create a leaflet and add providers
    leaf_map <- .view_add_base_maps()
    # create a vector to hold overlay groups
    overlay_groups <- vector()
    # convert tiles names to tile objects
    cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    # obtain dates vector
    dates <- .view_set_dates(x, dates)
    # create a new layer in the leaflet
    for (i in seq_len(nrow(cube))) {
        row <- cube[i, ]
        for (date in dates) {
            # add group
            group <- .view_add_overlay_group(row, as.Date(date), band)
            overlay_groups <- append(overlay_groups, group)
            # view image raster
            leaf_map <- leaf_map |>
                .view_image_raster(
                    group = group,
                    tile = row,
                    date = as.Date(date),
                    band = band,
                    red = red,
                    green = green,
                    blue = blue,
                    legend = legend,
                    palette = palette,
                    opacity = opacity
            )
        }
        # include segments and class cube if available
        leaf_map <- leaf_map |>
        # include segments
            .view_segments(
                tile = row,
                seg_color = seg_color,
                line_width = line_width,
                opacity  = opacity,
                legend = legend,
                palette = palette
        ) |>
            .view_class_cube(
                class_cube = class_cube,
                tile = row,
                legend = legend,
                palette = palette,
                opacity = opacity
            )
    }
    # add overlay groups for segments and class cube (if available)
    overlay_groups <- .view_add_overlay_grps(
        overlay_groups = overlay_groups,
        cube = x,
        class_cube = class_cube
    )
    # add layers control to leafmap
    leaf_map <- leaf_map |>
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance",
                           "Sentinel-2-2020", "OSM"),
            overlayGroups = overlay_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        # add legend to leaf_map
        .view_add_legend(
            cube = x,
            legend = legend,
            palette = palette
        )
    return(leaf_map)
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.class_cube <- function(x, ...,
                                 tiles = x[["tile"]],
                                 legend = NULL,
                                 palette = "Spectral",
                                 opacity = 0.8) {
    # preconditions
    .check_require_packages("leaflet")
    # deal with tiles
    # filter the tiles to be processed
    cube <- .view_filter_tiles(x, tiles)
    # create a leaflet and add providers
    leaf_map <- .view_add_base_maps()
    # go through the tiles
    for (row in nrow(cube)) {
        tile <- cube[row, ]
        # add a leafmap for class cube
        leaf_map <- leaf_map |>
            .view_class_cube(
                class_cube = cube,
                tile = tile,
                legend = legend,
                palette = palette,
                opacity = opacity
            )
    }

    # add overlay groups
    overlay_groups <- "classification"
    # add layers control
    leaf_map <- leaf_map |>
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance",
                           "Sentinel-2-2020", "OSM"),
            overlayGroups = overlay_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        # add legend
        .view_add_legend(
            cube = cube,
            legend = legend,
            palette = palette
        )

    return(leaf_map)
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.probs_cube <- function(x, ...,
                                 tiles = x[["tile"]],
                                 class_cube = NULL,
                                 legend = NULL,
                                 opacity = 0.7,
                                 palette = "YlGnBu") {
    stop(.conf("messages", "sits_view"))
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.default <- function(x, ...) {
    stop(.conf("messages", "sits_view_default"))
}
