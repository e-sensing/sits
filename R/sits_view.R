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
#' @param  segments      Segment list produced by \link{sits_segment}
#' @param  view_max_mb   Maximum size of leaflet to be visualized
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
#'     # view the probs
#'     sits_view(modis_probs)
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
#'
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = modis_cube,
#'         tile = "012010",
#'         bands = "NDVI",
#'         date = sits_timeline(modis_cube)[1],
#'         seg_fn = sits_supercells(step = 20)
#'     )
#'     # view image and segments
#'     sits_view(
#'         modis_cube,
#'         band = "NDVI",
#'         segments = segments
#'     )
#'     # view image, classified image and segments
#'     sits_view(
#'         modis_cube,
#'         red = "NDVI",
#'         green = "NDVI",
#'         blue = "NDVI",
#'         class_cube = modis_label,
#'         segments = segments
#'     )
#'     # view B/W image, classified image and segments
#'     sits_view(
#'         modis_cube,
#'         band = "NDVI",
#'         class_cube = modis_label,
#'         segments = segments
#'     )
#'     # get the average value per segment
#'     samples_seg <- sits_get_data(
#'         cube = modis_cube,
#'         samples = segments
#'     )
#'     # train a model
#'     tcnn_model <- sits_train(samples_modis_ndvi, sits_tempcnn())
#'     # classify the segments
#'     seg_class <- sits_classify(
#'         data = samples_seg,
#'         ml_model = tcnn_model
#'     )
#'
#'     # add a column to the segments by class
#'     segments <- sits_join_segments(
#'         data = seg_class,
#'         segments = segments
#'     )
#'     # view image and classified segments
#'     sits_view(
#'         modis_cube,
#'         band = "NDVI",
#'         segments = segments
#'     )
#'     # view image, classified image and segments
#'     sits_view(
#'         modis_cube,
#'         red = "NDVI",
#'         green = "NDVI",
#'         blue = "NDVI",
#'         class_cube = modis_label,
#'         segments = segments
#'     )
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
    # precondition
    .check_require_packages("leaflet")

    # check samples contains the expected columns
    .check_chr_contains(
        colnames(x),
        contains = c("longitude", "latitude", "label"),
        discriminator = "all_of",
        msg = "Missing lat/long and label - please correct"
    )
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
    # check id_neuron
    .check_int_parameter(
        id_neurons,
        min = 1,
        max = max(unique(x$labelled_neurons$id_neuron)),
        len_min = 1,
        len_max = length(unique(x$labelled_neurons$id_neuron))
    )
    # first select unique locations
    samples <- dplyr::filter(
        x$data, .data[["id_neuron"]] %in% !!id_neurons
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
                                  tiles = x$tile,
                                  dates = NULL,
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "RdYlGn",
                                  segments = NULL,
                                  view_max_mb = NULL) {
    # preconditions
    # Probs cube not supported
    .check_that(!inherits(x, "probs_cube"),
        local_msg = paste0("sits_view not available for probability cube")
    )
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # pre-condition for bands
    .check_view_bands(x, band, red, green, blue)
    # grayscale or RGB?
    leaf_map <- .view_image(
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
        segments = segments,
        view_max_mb = view_max_mb
    )
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.uncertainty_cube <- function(x, ...,
                                       tiles = x$tile,
                                       class_cube = NULL,
                                       legend = NULL,
                                       palette = "Blues",
                                       segments = NULL,
                                       view_max_mb = NULL) {
    # preconditions
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # plot as grayscale
    band <- .cube_bands(x)
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        x$tile,
        msg = "requested tiles are not part of cube"
    )
    # filter the tiles to be processed
    cube <- .cube_filter_tiles(x, tiles)
    # more than one tile? needs regular cube
    if (nrow(cube) > 1) {
        .check_is_regular(cube)
    }
    # check the view_max_mb parameter
    view_max_mb <- .view_set_max_mb(view_max_mb)
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = 1,
        view_max_mb = view_max_mb
    )
    # create a leaflet and add providers
    leaf_map <- .view_add_basic_maps()
    # get names of basic maps
    base_maps <- .view_get_base_maps(leaf_map)
    # obtain the raster objects for the dates chosen
    for (row in seq_len(nrow(cube))) {
        # get tile
        tile <- cube[row, ]
        band_file <- .tile_path(tile, band)
        leaf_map <- .view_add_stars_image(
            leaf_map = leaf_map,
            band_file = band_file,
            tile = tile,
            band = .cube_bands(cube),
            palette = palette,
            output_size = output_size
        )
    }
    # include class cube, if available
    leaf_map <- .view_class_cube(
        leaf_map = leaf_map,
        class_cube = class_cube,
        tiles = tiles,
        legend = legend,
        palette = palette,
        output_size = output_size
    )
    # add overlay groups
    overlay_groups <- .view_add_overlay_grps(
        cube = cube,
        class_cube = class_cube
    )
    # add layers control to leafmap
    leaf_map <- leaf_map |>
        leaflet::addLayersControl(
            baseGroups = base_maps,
            overlayGroups = overlay_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        # add legend to leaf_map
        .view_add_legend(
            class_cube = class_cube,
            segments = segments,
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
                                 tiles = NULL,
                                 legend = NULL,
                                 palette = "Spectral",
                                 segments = NULL,
                                 view_max_mb = NULL) {
    # preconditions
    .check_require_packages("leaflet")
    # deal with tiles
    if (!purrr::is_null(tiles)) {
        # try to find tiles in the list of tiles of the cube
        .check_chr_within(
            tiles,
            x$tile,
            msg = "requested tiles are not part of cube"
        )
        # select the tiles that will be shown
        cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    } else {
        cube <- x
    }
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = 1,
        view_max_mb = view_max_mb
    )
    # create a leaflet and add providers
    leaf_map <- .view_add_basic_maps()
    # get names of basic maps
    base_maps <- .view_get_base_maps(leaf_map)
    # add a leafmap for class cube
    leaf_map <- leaf_map |>
        .view_class_cube(
            class_cube = cube,
            tiles = tiles,
            legend = legend,
            palette = palette,
            output_size = output_size
        ) |>
        # add segments
        .view_segments(
            segments = segments,
            legend = legend,
            palette = palette
        ) |>
        # add legend
        .view_add_legend(
            class_cube = cube,
            legend = legend,
            palette = palette,
            segments = segments
        )

    # add overlay groups
    overlay_groups <- .view_add_overlay_grps(
        class_cube = cube,
        segments = segments
    )
    # add layers control
    leaf_map <- leaf_map |>
        leaflet::addLayersControl(
            baseGroups = base_maps,
            overlayGroups = overlay_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    return(leaf_map)
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.probs_cube <- function(x, ...,
                                 tiles = x$tile,
                                 class_cube = NULL,
                                 legend = NULL,
                                 view_max_mb = NULL,
                                 palette = "YlGnBu") {
    # preconditions
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # get band and labels
    band <- .cube_bands(x)
    labels <- .cube_labels(x)
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        x$tile,
        msg = "requested tiles are not part of cube"
    )
    # filter the tiles to be processed
    cube <- .cube_filter_tiles(x, tiles)

    # more than one tile? needs regular cube
    if (nrow(cube) > 1) {
        .check_is_regular(cube)
    }
    # check the view_max_mb parameter
    view_max_mb <- .view_set_max_mb(view_max_mb)
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = length(labels),
        view_max_mb = view_max_mb
    )
    # add base maps to leaflets
    leaf_map <- .view_add_basic_maps()
    # get names of basic maps
    base_maps <- .view_get_base_maps(leaf_map)
    # obtain the raster objects for the dates chosen
    for (row in seq_len(nrow(cube))) {
        # get tile
        tile <- cube[row, ]
        probs_file <- .tile_path(tile, band)
        st_obj <- stars::read_stars(
            probs_file,
            along = "band",
            RasterIO = list(
                "nBufXSize" = output_size[["xsize"]],
                "nBufYSize" = output_size[["ysize"]]
            ),
            proxy = FALSE
        )
        # resample and warp the image
        st_obj_new <- stars::st_warp(
            src = st_obj,
            crs = sf::st_crs("EPSG:3857")
        )
        for (ind in seq_len(length(labels))) {
            # add stars to leaflet
            leaf_map <- leafem::addStarsImage(
                leaf_map,
                x = st_obj_new,
                band = ind,
                colors = palette,
                project = FALSE,
                group = paste("probs", labels[[ind]]),
                maxBytes = output_size["leaflet_maxbytes"]
            )
        }
    }
    # should we overlay a classified image?
    leaf_map <- leaf_map |>
        .view_class_cube(
            class_cube = class_cube,
            tiles = tiles,
            legend = legend,
            palette = palette,
            output_size = output_size
        ) |>
        # add legend
        .view_add_legend(class_cube = cube)

    # set overlay groups
    overlay_groups <- paste("probs", labels)
    if (!purrr::is_null(class_cube)) {
        overlay_groups <- c(overlay_groups, "classification")
    }
    # add layers control to leafmap
    leaf_map <- leaf_map |>
        leaflet::addLayersControl(
            baseGroups = base_maps,
            overlayGroups = overlay_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    return(leaf_map)
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.default <- function(x, ...) {
    stop(paste0("sits_view not available for object of class ", class(x)[1]))
}
