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
                                  tiles = x[["tile"]],
                                  dates = NULL,
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "RdYlGn",
                                  opacity = 0.7) {
    # preconditions
    # Probs cube not supported
    .check_that(!inherits(x, "probs_cube"))
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # pre-condition for bands
    .check_bw_rgb_bands(band, red, green, blue)
    .check_available_bands(x, band, red, green, blue)
    # retrieve dots
    dots <- list(...)
    # deal with wrong parameter "date"
    if ("date" %in% names(dots) && missing(dates)) {
        dates <- as.Date(dots[["date"]])
    }
    # view image raster
    leaf_map <- .view_image_raster(
        cube = x,
        class_cube = class_cube,
        tiles = tiles,
        dates = dates,
        band = band,
        red = red,
        green = green,
        blue = blue,
        legend = legend,
        palette = palette,
        opacity = opacity
    )
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.vector_cube <- function(x, ...,
                                  band = NULL,
                                  red = NULL,
                                  green = NULL,
                                  blue = NULL,
                                  tiles = x[["tile"]],
                                  dates = NULL,
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "RdYlGn",
                                  opacity = 0.7,
                                  seg_color = "black",
                                  line_width = 1) {
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # Probs cube not supported
    .check_that(!inherits(x, "probs_cube"))
    # pre-condition for bands
    .check_bw_rgb_bands(band, red, green, blue)
    .check_available_bands(x, band, red, green, blue)
    # retrieve dots
    dots <- list(...)
    # deal with wrong parameter "date"
    if ("date" %in% names(dots) && missing(dates)) {
        dates <- as.Date(dots[["date"]])
    }
    # view vector cube
    leaf_map <- .view_image_vector(
        cube = x,
        tiles = tiles,
        dates = dates,
        band = band,
        red = red,
        green = green,
        blue = blue,
        class_cube = class_cube,
        legend = legend,
        palette = palette,
        opacity = opacity,
        seg_color = seg_color,
        line_width = line_width
    )
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.uncertainty_cube <- function(x, ...,
                                       tiles = x[["tile"]],
                                       class_cube = NULL,
                                       legend = NULL,
                                       palette = "Blues",
                                       opacity = 0.7) {
    # preconditions
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # plot as grayscale
    band <- .cube_bands(x)
    # filter the tiles to be processed
    cube <- .view_filter_tiles(x, tiles)
    # more than one tile? needs regular cube
    if (nrow(cube) > 1) {
        .check_that(.cube_is_regular(cube))
    }
    # view image raster
    leaf_map <- .view_image_raster(
        cube = x,
        class_cube = class_cube,
        tiles = tiles,
        dates = NULL,
        band = band,
        red = NULL,
        green = NULL,
        blue = NULL,
        legend = legend,
        palette = palette,
        opacity = opacity
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
    # add a leafmap for class cube
    leaf_map <- leaf_map |>
        .view_class_cube(
            class_cube = cube,
            tiles = tiles,
            legend = legend,
            palette = palette,
            opacity = opacity
        )
    # add overlay groups
    overlay_groups <- .view_add_overlay_grps(
        cube = cube
    )
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
