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
#' @param  palette       Palette provided in the configuration file.
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
#'     sits_view(cerrado_2classes)
#'
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
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
#'
#'     modis_probs <- sits_classify(
#'         data = modis_cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir()
#'     )
#'     modis_label <- sits_label_classification(
#'         modis_probs,
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
#'     modis_uncert <- sits_uncertainty(
#'         cube = modis_probs,
#'         output_dir = tempdir()
#'     )
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

    # precondition
    .check_require_packages("leaflet")

    # check samples contains the expected columns
    .check_chr_contains(
        colnames(x),
        contains = c("longitude", "latitude", "label"),
        discriminator = "all_of",
        msg = "Missing lat/long and label - please correct")

    leaf_map <- .view_samples(
        samples = x,
        legend = legend,
        palette = palette)

    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.data.frame <- function(x, ...,
                           legend = NULL,
                           palette = "Harmonic") {

    # precondition
    .check_require_packages("leaflet")

    # check samples contains the expected columns
    .check_chr_contains(
        colnames(x),
        contains = c("longitude", "latitude", "label"),
        discriminator = "all_of",
        msg = "Missing lat/long and label - please correct")

    leaf_map <- .view_samples(
        samples = x,
        legend = legend,
        palette = palette)

    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
#'
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
        palette = palette)

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
                                  segments = NULL,
                                  view_max_mb = NULL,
                                  palette = "RdYlGn") {
    # preconditions
    # Probs cube not supported
    .check_that(!inherits(x, "probs_cube"),
        local_msg = paste0("sits_view not available for probability cube")
    )
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # pre-condition for bands
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
    # grayscale or RGB?
    if (!purrr::is_null(band)) {
        leaf_map <- .view_false_color(
            cube = x,
            class_cube = class_cube,
            tiles = tiles,
            dates = dates,
            band = band,
            legend = legend,
            palette = palette,
            view_max_mb = view_max_mb
        )
    } else {
        bands <- c(red, green, blue)
        leaf_map <- .view_rgb_image(
            cube = x,
            class_cube = class_cube,
            tiles = tiles,
            dates = dates,
            bands = bands,
            legend = legend,
            palette = palette,
            view_max_mb = view_max_mb
        )
    }
    return(leaf_map)
}
.view_false_color <- function(cube,
                              class_cube,
                              tiles,
                              dates,
                              band,
                              legend,
                              palette,
                              view_max_mb){
    # check bands are available
    .check_chr_within(
        band,
        within = .cube_bands(cube),
        discriminator = "any_of",
        msg = "invalid band"
    )
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        cube$tile,
        msg = "requested tiles are not part of cube"
    )
    # filter the tiles to be processed
    cube <- .cube_filter_tiles(cube, tiles)

    # more than one tile? needs regular cube
    if (nrow(cube) > 1)
        .check_is_regular(cube)

    # get the timeline
    timeline <- .cube_timeline(cube)[[1]]

    if (purrr::is_null(dates))
        dates <- timeline[1]

    # check dates exist
    .check_that(
        x = all(as.Date(dates) %in% timeline),
        local_msg = "date is not in cube timeline",
        msg = "invalid dates parameter"
    )
    # check the view_max_mb parameter
    if (!purrr::is_null(view_max_mb)) {
        .check_num(view_max_mb,
                   is_integer = TRUE,
                   min = 16,
                   max = 512,
                   msg = "view_max_mb should be btw 16MB and 512 MB")
    }
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = length(dates),
        view_max_mb = view_max_mb
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
            tile <- cube[row, ]
            # check if date is inside the timeline
            tile_dates <- sits_timeline(tile)
            if (!date %in% tile_dates) {
                idx_date <- which.min(abs(date - tile_dates))
                date <- tile_dates[idx_date]
            }
            # filter by date and band
            # if there is only one band, RGB files will be the same
            band_file   <- .tile_path(tile, band, date)
            st_obj <- stars::read_stars(
                band_file,
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
            # add stars to leaflet
            leaf_map <- leafem::addStarsImage(
                leaf_map,
                x = st_obj_new,
                band = 1,
                colors = palette,
                project = FALSE,
                group = paste(tile[["tile"]], band, date),
                maxBytes = output_size["leaflet_maxbytes"]
            )
        }
    }

    overlay_grps <- unlist(purrr::map(cube[["tile"]], function(tile) {
        paste(tile, band, dates)
    }))

    # should we overlay a classified image?
    if (!purrr::is_null(class_cube)) {
        # check that class_cube is valid
        .check_that(
            x = inherits(class_cube, c("class_cube")),
            msg = "classified cube to be overlayed is invalid"
        )
        # get the labels
        labels <- sits_labels(class_cube)
        names(labels) <- seq_along(labels)
        # obtain the colors
        colors <- .colors_get(
            labels = labels,
            legend = legend,
            palette = palette
        )

        # select the tiles that will be shown
        if (!purrr::is_null(tiles))
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
            return(st_obj)
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
        # define overlay groups
        overlay_grps <- c(overlay_grps, "classification")
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

.view_rgb_image <- function(cube,
                            class_cube,
                            tiles,
                            dates,
                            bands,
                            legend,
                            palette,
                            view_max_mb){
    # check bands are available
    .check_chr_within(
        bands,
        within = .cube_bands(cube),
        discriminator = "any_of",
        msg = "invalid band"
    )
    # get rgb
    red   <- bands[[1]]
    green <- bands[[2]]
    blue  <- bands[[3]]
    r_index <- 1
    g_index <- 2
    b_index <- 3
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        cube$tile,
        msg = "requested tiles are not part of cube"
    )
    # filter the tiles to be processed
    cube <- .cube_filter_tiles(cube, tiles)

    # more than one tile? needs regular cube
    if (nrow(cube) > 1)
        .check_is_regular(cube)

    # get the timeline
    timeline <- .cube_timeline(cube)[[1]]

    if (purrr::is_null(dates))
        dates <- timeline[1]

    # check dates exist
    .check_that(
        x = all(as.Date(dates) %in% timeline),
        local_msg = "date is not in cube timeline",
        msg = "invalid dates parameter"
    )
    # check the view_max_mb parameter
    if (!purrr::is_null(view_max_mb)) {
        .check_num(view_max_mb,
                   is_integer = TRUE,
                   min = 16,
                   max = 512,
                   msg = "view_max_mb should be btw 16MB and 512 MB")
    }
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = length(dates),
        view_max_mb = view_max_mb
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
            tile <- cube[row, ]
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
        # get the labels
        labels <- sits_labels(class_cube)
        names(labels) <- seq_along(labels)
        # obtain the colors
        colors <- .colors_get(
            labels = labels,
            legend = legend,
            palette = palette
        )

        # select the tiles that will be shown
        if (!purrr::is_null(tiles))
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
            return(st_obj)
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
        # define overlay groups
        overlay_grps <- c(overlay_grps, "classification")
    }
    if (!purrr::is_null(segments)) {
        # check that segments are valid
        .check_that(
            x = inherits(segments, c("segments")),
            msg = "segments to be overlayed are invalid"
        )
        # how many tiles are there in the segments
        tile_names <- names(segments)
        for (tile_name in tile_names) {
            # retrieve the segments for this tile
            sf_seg <- segments[[tile_name]]
            # transform the segments
            sf_seg <- sf::st_transform(sf_seg,
                crs = sf::st_crs("EPSG:4326")
            )
            # have the segments been classified?
            if ("class" %in% colnames(sf_seg)) {
                # get the labels
                lab_seg <- sf_seg %>%
                    sf::st_drop_geometry() %>%
                    dplyr::select("class") %>%
                    dplyr::distinct() %>%
                    dplyr::pull()
                names(lab_seg) <- seq_along(lab_seg)
                # obtain the colors
                col_seg <- .colors_get(
                    labels = lab_seg,
                    legend = legend,
                    palette = palette
                )
                # create a color palette
                fact_pal <- leaflet::colorFactor(
                    palette = col_seg,
                    domain = lab_seg
                )
                # add a new leafmap to show polygons of segments
                leafmap <- leafem::addFeatures(
                    leaf_map,
                    data = sf_seg,
                    fill = TRUE,
                    fillColor = colors,
                    fillOpacity = 0.7
                ) %>%
                    leaflet::addLegend(
                        "topright",
                        pal     = fact_pal,
                        values  = lab_seg,
                        title   = "Classes",
                        opacity = 1
                    )
                # define overlay groups
                overlay_grps <- c(overlay_grps, "class segments")
            }
            else {
                leaf_map <- leafem::addFeatures(
                    leaf_map,
                    data = sf_seg,
                    fill = TRUE,
                    color = "lightgoldenrod",
                    weight = 5
                )
                # define overlay groups
                overlay_grps <- c(overlay_grps, "segments")
            }
        }
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
#' @rdname   sits_view
#'
#' @export
sits_view.uncertainty_cube <- function(x, ...,
                                  tiles = x$tile,
                                  class_cube = NULL,
                                  legend = NULL,
                                  view_max_mb = NULL,
                                  palette = "Blues") {
    # preconditions
    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # plot as grayscale
    band   <- .cube_bands(x)
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        x$tile,
        msg = "requested tiles are not part of cube"
    )
    # filter the tiles to be processed
    cube <- .cube_filter_tiles(x, tiles)

    # more than one tile? needs regular cube
    if (nrow(cube) > 1)
        .check_is_regular(cube)

    # check the view_max_mb parameter
    if (!purrr::is_null(view_max_mb)) {
        .check_num(view_max_mb,
                   is_integer = TRUE,
                   min = 16,
                   max = 512,
                   msg = "view_max_mb should be btw 16MB and 512 MB")
    }
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = 1,
        view_max_mb = view_max_mb
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
    for (row in seq_len(nrow(cube))) {
        # get tile
        tile <- cube[row, ]
        band_file <- .tile_path(tile, band)
        st_obj <- stars::read_stars(
            band_file,
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

        # add stars to leaflet
        leaf_map <- leafem::addStarsImage(
            leaf_map,
            x = st_obj_new,
            band = 1,
            colors = palette,
            project = FALSE,
            group = paste(tile[["tile"]], .cube_bands(cube)),
            maxBytes = output_size["leaflet_maxbytes"]
        )
    }

    overlay_grps <- unlist(purrr::map(cube[["tile"]], function(tile) {
        paste(tile, .cube_bands(cube))
    }))

    # should we overlay a classified image?
    if (!purrr::is_null(class_cube)) {
        # check that class_cube is valid
        .check_that(
            x = inherits(class_cube, c("class_cube")),
            msg = "classified cube to be overlayed is invalid"
        )
        # get the labels
        labels <- sits_labels(class_cube)
        names(labels) <- seq_along(labels)
        # obtain the colors
        colors <- .colors_get(
            labels = labels,
            legend = legend,
            palette = palette
        )

        # select the tiles that will be shown
        if (!purrr::is_null(tiles))
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
            return(st_obj)
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
        # define overlay groups
        overlay_grps <- c(overlay_grps, "classification")
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
                                 palette = "default",
                                 view_max_mb = NULL) {
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
    } else {
        cube <- x
    }

    # get the labels
    labels <- sits_labels(cube)
    names(labels) <- seq_along(labels)
    # obtain the colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette
    )
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = 1,
        view_max_mb = view_max_mb
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
        return(st_obj)
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
    labels  <- .cube_labels(x)
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        x$tile,
        msg = "requested tiles are not part of cube"
    )
    # filter the tiles to be processed
    cube <- .cube_filter_tiles(x, tiles)

    # more than one tile? needs regular cube
    if (nrow(cube) > 1)
        .check_is_regular(cube)

    # check the view_max_mb parameter
    if (!purrr::is_null(view_max_mb)) {
        .check_num(view_max_mb,
                   is_integer = TRUE,
                   min = 16,
                   max = 512,
                   msg = "view_max_mb should be btw 16MB and 512 MB")
    }
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        cube = cube,
        ndates = length(labels),
        view_max_mb = view_max_mb
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

    # set overlay grps
    overlay_grps <- paste("probs", labels)

    # should we overlay a classified image?
    if (!purrr::is_null(class_cube)) {
        # check that class_cube is valid
        .check_that(
            x = inherits(class_cube, c("class_cube")),
            msg = "classified cube to be overlayed is invalid"
        )
        # get the labels
        labels <- sits_labels(class_cube)
        names(labels) <- seq_along(labels)
        # obtain the colors
        colors <- .colors_get(
            labels = labels,
            legend = legend,
            palette = palette
        )

        # select the tiles that will be shown
        if (!purrr::is_null(tiles))
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
            return(st_obj)
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
        # define overlay groups
        overlay_grps <- c(overlay_grps, "classification")
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
sits_view.default <- function(x, ...) {
    stop(paste0("sits_view not available for object of class ", class(x)[1]))
}
#' @title  Return the size of the imaged to be resamples for visulization
#' @name .view_resample_size
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube          Cube with tiles to be merged.
#' @param  ndates        Number of dates to be viewed.
#' @param  view_max_mb   Maximum size of leaflet to be visualized
#' @return               Number of rows and cols to be visualized.
#'
#'
.view_resample_size <- function(cube, ndates, view_max_mb) {

    # number of tiles to be merged
    ntiles <- nrow(cube)
    # estimate nrows and ncols to be merged
    nrows <- sum(slider::slide_dbl(cube, function(tile) {
        # retrieve the file info for the tile
        fi <- .fi(tile)
        return(max(fi[["nrows"]]))
    }))
    ncols <- sum(slider::slide_dbl(cube, function(tile) {
        # retrieve the file info for the tile
        fi <- .fi(tile)
        return(max(fi[["ncols"]]))
    }))

    # get the maximum number of bytes to be displayed (total)
    if (purrr::is_null(view_max_mb))
        max_megabytes <- .conf("leaflet_max_megabytes")
    else
        max_megabytes <- view_max_mb

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
#' @title  Visualize a set of samples
#' @name .view_samples
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  samples       Data.frame with columns "longitude", "latitude"
#'                       and "label"
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file.
#' @return               A leaflet object containing either samples or
#'                       data cubes embedded in a global map that can
#'                       be visualized directly in an RStudio viewer.
.view_samples <- function(samples, legend, palette){

    # first select unique locations
    samples <- dplyr::distinct(
        samples,
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
    labels <- sort(unique(samples$label))

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

