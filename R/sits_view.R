#' @title  View data cubes and samples in leaflet
#' @name sits_view
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses leaflet to visualize time series, raster cube and
#' classified images
#'
#' @param  x             Object of class "sits",
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
#' @param  palette       Palette provided in the configuration file.
#' @param  label         Label from the SOM map to be shown.
#' @param  prob_max      Maximum a posteriori probability for SOM neuron
#'                       samples to be shown
#' @param  prob_min      Minimum a posteriori probability for SOM neuron
#'                       samples to be shown
#'
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
#'     sits_view(cerrado_2classes)
#'
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
#'     modis_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # get the timeline
#'     timeline <- sits_timeline(modis_cube)
#'     # view the data cube
#'     sits_view(modis_cube,
#'         band = "NDVI",
#'         dates = timeline[[1]]
#'     )
#'     # train a model
#'     rf_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'
#'     modis_probs <- sits_classify(
#'         data = modis_cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir(),
#'         memsize = 4,
#'         multicores = 1
#'     )
#'     modis_label <- sits_label_classification(modis_probs,
#'         output_dir = tempdir()
#'     )
#'
#'     sits_view(modis_label)
#'
#'     sits_view(modis_cube,
#'         band = "NDVI",
#'         class_cube = modis_label,
#'         dates = sits_timeline(modis_cube)[[1]]
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

    # first select unique locations
    x <- dplyr::distinct(
        x,
        .data[["longitude"]],
        .data[["latitude"]],
        .data[["label"]]
    )
    # convert tibble to sf
    samples <- sf::st_as_sf(
        x[c("longitude", "latitude", "label")],
        coords = c("longitude", "latitude"),
        crs = 4326
    )
    # get the bounding box
    samples_bbox <- sf::st_bbox(samples)
    # get the labels
    labels <- sits_labels(x)

    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .colors_get(
            labels = labels,
            palette = palette,
            rev = TRUE
        )
    } else {
        .check_chr_within(
            x = labels,
            within = names(legend),
            msg = "some labels are missing from the legend"
        )
        colors <- unname(legend[labels])
    }
    # create a pallete of colors
    factpal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )
    # create an interative map
    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) %>%
        leafem::addMouseCoordinates(map = .) %>%
        leaflet::flyToBounds(
            map = .,
            lng1 = samples_bbox[["xmin"]],
            lat1 = samples_bbox[["ymin"]],
            lng2 = samples_bbox[["xmax"]],
            lat2 = samples_bbox[["ymax"]]
        ) %>%
        leaflet::addCircleMarkers(
            map = .,
            data = samples,
            color = ~ factpal(label),
            radius = 4,
            stroke = FALSE,
            fillOpacity = 1,
            group = "Samples"
        ) %>%
        leaflet::addLayersControl(
            map = .,
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = c("Samples"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend("topright",
            pal     = factpal,
            values  = samples$label,
            title   = "Training Samples",
            opacity = 1
        )
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
#'
sits_view.som_map <- function(x, ...,
                              label,
                              prob_max = 1.0,
                              prob_min = 0.7,
                              legend = NULL,
                              palette = "Harmonic") {

    # view the samples
    # first select unique locations
    samples <- dplyr::distinct(
        x$data,
        .data[["longitude"]],
        .data[["latitude"]],
        .data[["label"]]
    )
    # convert tibble to sf
    samples <- sf::st_as_sf(
        samples[c("longitude", "latitude", "label")],
        coords = c("longitude", "latitude"),
        crs = 4326
    )
    # get the bounding box
    samples_bbox <- sf::st_bbox(samples)
    # get the labels
    labels <- sits_labels(samples)

    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .colors_get(
            labels = labels,
            palette = palette,
            rev = TRUE
        )
    } else {
        .check_chr_within(
            x = labels,
            within = names(legend),
            msg = "some labels are missing from the legend"
        )
        colors <- unname(legend[labels])
    }
    # create a pallete of colors
    factpal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )

    # use the neuron tibble with id neuron and samples per neuron
    # group neurons by neuron_id, best classes
    # filter by label, prior and posterior probability
    neurons_best <- x$labelled_neurons %>%
        dplyr::group_by(.data[["id_neuron"]]) %>%
        dplyr::slice_max(.data[["count"]], with_ties = FALSE) %>%
        dplyr::filter(.data[["label_samples"]] == label,
                      .data[["post_prob"]] <= prob_max,
                      .data[["post_prob"]] >= prob_min)

    # use the sits tibble with time series and cols: id_sample and id_neuron
    # filter by id_neuron
    samples_label <-  x$data %>%
        dplyr::inner_join(neurons_best, by = c("id_neuron" = "id_neuron"))

    # create an interative map
    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) %>%
        leafem::addMouseCoordinates(map = .) %>%
        leaflet::flyToBounds(
            map = .,
            lng1 = samples_bbox[["xmin"]],
            lat1 = samples_bbox[["ymin"]],
            lng2 = samples_bbox[["xmax"]],
            lat2 = samples_bbox[["ymax"]]
        ) %>%
        leaflet::addCircleMarkers(
            map = .,
            data = samples_label,
            popup = ~as.character(post_prob),
            color = ~factpal(label),
            radius = 4,
            stroke = FALSE,
            fillOpacity = 1,
            group = "Samples"
        ) %>%
        leaflet::addLayersControl(
            map = .,
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = c("Samples"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend("topright",
                           pal     = factpal,
                           values  = samples$label,
                           title   = "Training Samples",
                           opacity = 1
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
                                  tiles = NULL,
                                  dates = NULL,
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "default") {
    # preconditions
    # get other parameters
    dots <- list(...)
    # Probs cube not supported
    .check_that(!inherits(x, "probs_cube"),
        local_msg = paste0("sits_view not available for probability cube")
    )
    # # Remote files not working in Windows (bug in stars)
    # .check_that(
    #     !(.Platform$OS.type == "windows" &&
    #         grepl("^/vsi", .file_info_path(x[1, ]))),
    #     msg = "sits_view not working in Windows OS for remote files"
    # )

    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # pre-condition for band
    .check_that(
        !(purrr::is_null(band)) ||
            (!(purrr::is_null(red)) &&
                !(purrr::is_null(green)) &&
                !(purrr::is_null(blue))
            ),
        local_msg = paste0(
            "either 'band' parameter or 'red', 'green', and",
            "'blue' parameters should be informed"
        )
    )

    # check if rgb bands were informed
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
    # deal with parameter "date"
    if ("date" %in% names(dots) && missing(dates)) {
        message("please use dates instead of date as parameter")
        dates <- as.Date(dots[["date"]])
    }
    # deal with wrong parameter "tile"
    if ("tile" %in% names(dots) && missing(tiles)) {
        message("please use tiles instead of tile as parameter")
        tiles <- dots[["tile"]]
    }
    # if tiles are not informed, show all
    if (!purrr::is_null(tiles)) {
        # try to find tiles in the list of tiles of the cube
        .check_chr_within(
            tiles,
            x$tile,
            msg = "requested tiles are not part of cube"
        )
        # filter the tiles to be processed
        cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    }
    else
        cube <- x

    # if dates are not informed, show the first possible date
    if (purrr::is_null(dates))
        dates <- sits_timeline(cube[1,])[1]
    # check dates exist
    .check_that(
        x = all(as.Date(dates) %in% sits_timeline(cube[1,])),
        local_msg = "date is not in cube timeline",
        msg = "invalid dates parameter"
    )

    nrows_merge <- sum(slider::slide_dbl(cube, function(tile) {
        # retrieve the file info for the tile
        fi <- .fi(tile)
        return(max(fi[["nrows"]]))
    }))
    ncols_merge <- sum(slider::slide_dbl(cube, function(tile) {
        # retrieve the file info for the tile
        fi <- .fi(tile)
        return(max(fi[["ncols"]]))
    }))

    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        nrows = nrows_merge,
        ncols = ncols_merge,
        ndates = length(dates),
        ntiles = nrow(cube)
    )
    # create a leaflet and add providers
    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) %>%
        leaflet::addWMSTiles(
            map = .,
            baseUrl = "https://tiles.maps.eox.at/wms/",
            layers = c("s2cloudless-2020_3857_512"),
            group = "Sentinel-2-2020"
        ) %>%
        leafem::addMouseCoordinates(map = .)

    # obtain the raster objects for the dates chosen
    for (i in seq_along(dates)) {
        date <- as.Date(dates[[i]])
        for (row in seq_len(nrow(cube))) {
            # get tile
            tile <- cube[row,]
            # check if date is inside the timeline
            tile_dates <- sits_timeline(tile)
            if (!date %in% tile_dates) {
                idx_date <- which.min(abs(date - tile_dates))
                date <- tile_dates[idx_date]
            }
            # filter by date and band
            # if there is only one band, RGB files will be the same
            red_file   <- .tile_path(tile, red, date)
            green_file <- .tile_path(tile, green, date)
            blue_file  <- .tile_path(tile, blue, date)
            rgb_files <- c(r = red_file, g = green_file, b = blue_file)
            st_obj <- stars::read_stars(
                rgb_files,
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

            # add raster RGB to leaflet
            leaf_map <- leafem::addRasterRGB(
                leaf_map,
                x = st_obj_new,
                r = r_index,
                g = g_index,
                b = b_index,
                quantiles = c(0.1, 0.9),
                project = FALSE,
                group = paste(tile[["tile"]], date),
                maxBytes = output_size["leaflet_maxbytes"]
            )
        }
    }

    overlay_grps <- unlist(purrr::map(cube[["tile"]], function(tile) {
        paste(tile, dates)
    }))

    # should we overlay a classified image?
    if (!purrr::is_null(class_cube)) {
        # check that class_cube is valid
        .check_that(
            x = inherits(class_cube, c("class_cube")),
            msg = "classified cube to be overlayed is invalid"
        )
        # define overlay groups
        overlay_grps <- c(paste0(dates), "classification")
        # get the labels
        labels <- sits_labels(class_cube)
        names(labels) <- seq_along(labels)
        # obtain the colors
        colors <- .view_get_colors(
            labels = labels,
            legend = legend,
            palette = palette
        )
        if (!purrr::is_null(tiles))
            # select the tiles that will be shown
            class_cube <- dplyr::filter(class_cube, .data[["tile"]] %in% tiles)

        # create the stars objects that correspond to the tiles
        st_objs <- slider::slide(class_cube, function(tile) {
            # obtain the raster stars object
            st_obj <- stars::read_stars(
                .tile_path(tile),
                RAT = labels,
                RasterIO = list(
                    "nBufXSize" = output_size[["xsize"]],
                    "nBufYSize" = output_size[["ysize"]]
                ),
                proxy = FALSE
            )
        })

        # keep the first object
        st_merge <- st_objs[[1]]

        # if there is more than one stars object, merge them
        if (length(st_objs) > 1) {
            st_merge <- stars::st_mosaic(
                st_objs[[1]],
                st_objs[[2:length(st_objs)]]
            )
        }
        # resample and warp the image
        st_obj_new <- stars::st_warp(
            src = st_merge,
            crs = sf::st_crs("EPSG:3857")
        )
        # create a palette of colors
        fact_pal <- leaflet::colorFactor(
            palette = colors,
            domain = labels
        )
        # add the classified image object
        leaf_map <- leafem::addStarsImage(
            leaf_map,
            x = st_obj_new,
            colors = colors,
            method = "ngb",
            group = "classification",
            project = FALSE,
            maxBytes = output_size["leaflet_maxbytes"]
        ) %>%
            leaflet::addLegend(
                "topright",
                pal     = fact_pal,
                values  = labels,
                title   = "Classes",
                opacity = 1
            )
    }

    # add layers control to leafmap
    leaf_map <- leaf_map %>%
        leaflet::addLayersControl(
            map = .,
            baseGroups = c("GeoPortalFrance", "ESRI", "OSM", "Sentinel-2-2020"),
            overlayGroups = overlay_grps,
            options = leaflet::layersControlOptions(collapsed = FALSE)
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
                                       palette = "default") {
    # preconditions
    .check_require_packages("leaflet")

    # deal with tiles
    # check if tile exists
    if (!purrr::is_null(tiles)) {
        # try to find tiles in the list of tiles of the cube
        .check_chr_within(
            tiles,
            x$tile,
            msg = "requested tiles are not part of cube"
        )
        # select the tiles that will be shown
        cube <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    }
    else
        cube <- x

    # get the labels
    labels <- sits_labels(cube)
    names(labels) <- seq_along(labels)
    # obtain the colors
    colors <- .view_get_colors(
        labels = labels,
        legend = legend,
        palette = palette
    )
    # find size of image to be merged
    nrows_merge <- sum(slider::slide_dbl(cube, function(tile) {
        # retrieve the file info for the tile
        fi <- .fi(tile)
        return(max(fi[["nrows"]]))
    }))
    ncols_merge <- sum(slider::slide_dbl(cube, function(tile) {
        # retrieve the file info for the tile
        fi <- .fi(tile)
        return(max(fi[["ncols"]]))
    }))
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        nrows = nrows_merge,
        ncols = ncols_merge,
        ndates = 1,
        ntiles = nrow(cube)
    )
    # create the stars objects that correspond to the tiles
    st_objs <- slider::slide(cube, function(tile) {
        # obtain the raster stars object
        st_obj <- stars::read_stars(
            .tile_path(tile),
            RAT = labels,
            RasterIO = list(
                "nBufXSize" = output_size[["xsize"]],
                "nBufYSize" = output_size[["ysize"]]
            ),
            proxy = FALSE
        )
    })

    # keep the first object
    st_merge <- st_objs[[1]]

    # if there is more than one stars object, merge them
    if (length(st_objs) > 1) {
        st_merge <- stars::st_mosaic(
            st_objs[[1]],
            st_objs[[2:length(st_objs)]]
        )
    }
    # resample and warp the image
    st_obj_new <- stars::st_warp(
        src = st_merge,
        crs = sf::st_crs("EPSG:3857")
    )
    # create a palette of colors
    fact_pal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )
    # create the leaflet map
    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) %>%
        leaflet::addProviderTiles(
            map = .,
            provider = leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) %>%
        leafem::addMouseCoordinates(map = .) %>%
        leafem::addStarsImage(
            x = st_obj_new,
            colors = colors,
            method = "ngb",
            group = "classification",
            project = FALSE,
            maxBytes = output_size["leaflet_maxbytes"]
        ) %>%
        # add the the layers control
        leaflet::addLayersControl(
            map = .,
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = "classification",
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend(
            "topright",
            pal     = fact_pal,
            values  = labels,
            title   = "Classes",
            opacity = 1
        )
    return(leaf_map)
}
#' @rdname sits_view
#'
#' @export
#'
sits_view.probs_cube <- function(x, ...) {
    stop("sits_view not available for object of class probs_cube")
}

#' @rdname sits_view
#'
#' @export
#'
sits_view.default <- function(x, ...) {
    stop(paste0("sits_view not available for object of class ", class(x)[1]))
}
#' @title  Return the colors associated to the classified image
#' @name .view_get_colors
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  labels        Labels of the classified cube.
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file.
#' @return               Colors for legend of classified image.
#'
#'
.view_get_colors <- function(labels, legend, palette) {
    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .colors_get(
            labels = labels,
            palette = palette,
            rev = TRUE
        )
    } else {
        .check_chr_within(
            x = labels,
            within = names(legend),
            msg = "some labels are missing from the legend"
        )
        colors <- unname(legend[labels])
        names(colors) <- seq_along(labels)
    }
    return(colors)
}
#' @title  Return the cell size for the image to be resamples
#' @name .view_resample_size
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  nrows         Number of rows in the input image.
#' @param  ncols         Number of cols in the input image.
#' @param  ndates        Number of dates to show
#' @param  ntiles        Number of tiles in the input image.
#' @return               Cell size for x and y coordinates.
#'
#'
.view_resample_size <- function(nrows, ncols, ndates, ntiles) {

    # get the maximum number of bytes to be displayed (total)
    max_megabytes <- .conf("leaflet_max_megabytes")
    # get the compression factor
    comp <- .conf("leaflet_comp_factor")

    # calculate the total size of all input images in bytes
    # note that leaflet considers 4 bytes per pixel
    in_size_mbytes <- 4 * nrows * ncols * ndates * ntiles * comp
    # do we need to compress?
    ratio <- max((in_size_mbytes / (max_megabytes * 1024 * 1024)), 1)
    # only create local files if required
    if (ratio > 1) {
        new_nrows <- round(nrows / sqrt(ratio))
        new_ncols <- round(ncols * (new_nrows / nrows))
    } else {
        new_nrows <- round(nrows)
        new_ncols <- round(ncols)
    }
    leaflet_maxbytes <- 4 * new_nrows * new_ncols
    return(c(
        "xsize" = new_ncols, "ysize" = new_nrows,
        "leaflet_maxbytes" = leaflet_maxbytes
    ))
}
