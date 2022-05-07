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
#'
#' @return               Leaflet object.
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
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # view the data cube
#'     sits_view(modis_cube,
#'         band = "NDVI",
#'         dates = timeline[[1]]
#'     )
#'
#'     samples_ndvi <- sits_select(samples_modis_4bands,
#'         bands = c("NDVI")
#'     )
#'     rf_model <- sits_train(samples_ndvi, sits_rfor())
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
    .check_that(
        x = inherits(x, c("sits", "raster_cube", "classified_image")),
        msg = "only works with time series, raster cubes and classified images"
    )
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
        colors <- .config_colors(
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
            popup = ~label,
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
sits_view.raster_cube <- function(x, ...,
                                  band = NULL,
                                  red = NULL,
                                  green = NULL,
                                  blue = NULL,
                                  tiles = x$tile[[1]],
                                  dates = sits_timeline(x)[1],
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "default") {
    # preconditions
    # Probs cube not supported
    .check_that(!inherits(x, "probs_cube"),
        local_msg = paste0("sits_view not available for probability cube")
    )
    # Remote files not working in Windows (bug in stars)
    .check_that(
        !(.Platform$OS.type == "windows" &&
            grepl("^/vsi", .file_info_path(x[1, ]))),
        msg = "sits_view not working in Windows OS for remote files"
    )

    # verifies if leafem and leaflet packages are installed
    .check_require_packages(c("leafem", "leaflet"))
    # check that dates are valid
    timeline <- sits_timeline(x)
    .check_that(
        all(as.Date(dates) %in% timeline),
        msg = "requested dates are not part of the cube timeline"
    )
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        x$tile,
        msg = "requested tiles are not part of cube"
    )
    # check that classified map is a proper cube
    if (!purrr::is_null(class_cube)) {
        .check_that(
            x = inherits(class_cube, c("classified_image")),
            msg = "classified cube to be overlayed is invalid"
        )
    }
    # pre-condition 2
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

    # filter the tiles to be processed
    cube_tiles <- dplyr::filter(x, .data[["tile"]] %in% tiles)

    # verifies if cube has a single timeline
    timeline <- sits_timeline(cube_tiles)
    .check_that(!is.list(timeline),
        local_msg = "more than one timeline per cube",
        msg = "cannot visualize cube"
    )


    nrows_merge <- sum(slider::slide_dbl(cube_tiles, function(tile) {
        # retrieve the file info for the tile
        fi <- .file_info(tile)
        return(max(fi[["nrows"]]))
    }))
    ncols_merge <- sum(slider::slide_dbl(cube_tiles, function(tile) {
        # retrieve the file info for the tile
        fi <- .file_info(tile)
        return(max(fi[["ncols"]]))
    }))

    # find out if resampling is required (for big images)
    size <- .view_resample_size(
        nrows = nrows_merge,
        ncols = ncols_merge,
        ntiles = nrow(cube_tiles)
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
        leafem::addMouseCoordinates(map = .)

    # obtain the raster objects for the dates chosen
    for (i in seq_along(dates)) {
        date <- as.Date(dates[[i]])
        st_objs <- slider::slide(cube_tiles, function(tile) {
            # retrieve the file info for the tile
            fi <- .file_info(tile)
            # filter by date
            images_date <- dplyr::filter(fi, as.Date(.data[["date"]]) == !!date)
            # if there is only one band, RGB files will be the same
            red_file <- dplyr::filter(
                images_date,
                .data[["band"]] == red
            )$path[[1]]
            green_file <- dplyr::filter(
                images_date,
                .data[["band"]] == green
            )$path[[1]]
            blue_file <- dplyr::filter(
                images_date,
                .data[["band"]] == blue
            )$path[[1]]
            rgb_files <- c(r = red_file, g = green_file, b = blue_file)
            st_obj <- stars::read_stars(
                rgb_files,
                along = "band",
                RasterIO = list(
                    "nBufXSize" = size["xsize"],
                    "nBufYSize" = size["ysize"]
                ),
                proxy = FALSE
            )
            return(st_obj)
        })

        # keep the first object
        st_merge <- st_objs[[1]]

        # mosaic the data
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
        # add raster RGB to leaflet
        leaf_map <- leafem::addRasterRGB(
            leaf_map,
            x = st_obj_new,
            r = r_index,
            g = g_index,
            b = b_index,
            quantiles = c(0.1, 0.9),
            project = FALSE,
            group = paste0(date),
            maxBytes = size["leaflet_maxbytes"]
        )
    }

    overlay_grps <- paste0(dates)

    # should we overlay a classified image?
    if (!purrr::is_null(class_cube)) {
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
        # select the tiles that will be shown
        cube_tiles <- dplyr::filter(class_cube, .data[["tile"]] %in% tiles)

        # create the stars objects that correspond to the tiles
        st_objs <- slider::slide(cube_tiles, function(tile) {
            # obtain the raster stars object
            st_obj <- stars::read_stars(
                .file_info_path(tile),
                RAT = labels,
                RasterIO = list(
                    "nBufXSize" = size["xsize"],
                    "nBufYSize" = size["ysize"]
                )
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
            maxBytes = size["leaflet_maxbytes"]
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
            baseGroups = c("GeoPortalFrance", "ESRI", "OSM"),
            overlayGroups = overlay_grps,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        )

    return(leaf_map)
}

#' @rdname sits_view
#'
#' @export
#'
sits_view.classified_image <- function(x, ...,
                                       tiles = NULL,
                                       legend = NULL,
                                       palette = "default") {
    # preconditions
    .check_require_packages("leaflet")

    # deal with tiles
    # check if tile exists
    if (purrr::is_null(tiles)) {
        tiles <- x$tile[[1]]
    }

    if (is.numeric(tiles)) {
        tiles <- x$tile[[tiles]]
    }
    # try to find tiles in the list of tiles of the cube
    .check_chr_within(
        tiles,
        x$tile,
        msg = "requested tiles are not part of cube"
    )
    # get the labels
    labels <- sits_labels(x)
    names(labels) <- seq_along(labels)
    # obtain the colors
    colors <- .view_get_colors(
        labels = labels,
        legend = legend,
        palette = palette
    )
    # select the tiles that will be shown
    cube_tiles <- dplyr::filter(x, .data[["tile"]] %in% tiles)
    # find size of image to be merged
    nrows_merge <- sum(slider::slide_dbl(cube_tiles, function(tile) {
        # retrieve the file info for the tile
        fi <- .file_info(tile)
        return(max(fi[["nrows"]]))
    }))
    ncols_merge <- sum(slider::slide_dbl(cube_tiles, function(tile) {
        # retrieve the file info for the tile
        fi <- .file_info(tile)
        return(max(fi[["ncols"]]))
    }))
    # find out if resampling is required (for big images)
    size <- .view_resample_size(
        nrows = nrows_merge,
        ncols = ncols_merge,
        ntiles = nrow(cube_tiles)
    )
    # create the stars objects that correspond to the tiles
    st_objs <- slider::slide(cube_tiles, function(tile) {
        # obtain the raster stars object
        st_obj <- stars::read_stars(
            .file_info_path(tile),
            RAT = labels,
            RasterIO = list(
                "nBufXSize" = size["xsize"],
                "nBufYSize" = size["ysize"]
            )
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
            maxBytes = size["leaflet_maxbytes"]
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

#' @title  Return the colors associated to the classified image
#' @name .view_get_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  labels        Labels of the classified cube.
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file.
#' @return               Colors for legend of classified image.
#' @keywords internal
#'
#'
.view_get_colors <- function(labels, legend, palette) {
    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .config_colors(
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
    return(colors)
}
#' @title  Return the cell size for the image to be resamples
#' @name .view_resample_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  nrows         Number of rows in the input image.
#' @param  ncols         Number of cols in the input image.
#' @param  ntiles        Number of tiles in the input image.
#' @return               Cell size for x and y coordinates.
#' @keywords internal
#'
#'
.view_resample_size <- function(nrows, ncols, ntiles) {

    # get the maximum number of bytes to be displayed per tile
    max_megabytes <- .config_get(key = "leaflet_max_megabytes")
    # get the compression factor
    comp <- .config_get(key = "leaflet_comp_factor")

    # calculate the size of the input image in bytes
    # note that leaflet considers 4 bytes per pixel
    in_size_mbytes <- 4 * nrows * ncols * comp * ntiles
    # do we need to compress?
    ratio <- max((in_size_mbytes / (max_megabytes * ntiles * 1024 * 1024)), 1)
    # only create local files if required
    if (ratio > 1) {
        new_nrows <- round(nrows / sqrt(ratio))
        new_ncols <- round(ncols * (new_nrows / nrows))
    } else {
        new_nrows <- round(nrows)
        new_ncols <- round(ncols)
    }
    leaflet_maxbytes <- 4 * new_nrows * new_ncols * ntiles
    return(c(
        "xsize" = new_ncols, "ysize" = new_nrows,
        "leaflet_maxbytes" = leaflet_maxbytes
    ))
}
