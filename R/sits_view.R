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
#' @examples
#' \donttest{
#' # view a collection of time series
#' sits_view(samples_modis_4bands)
#'
#' # view a temporal instance of a cube
#'
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
#' modis_cube <- sits_cube(
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   bands = c("NDVI", "EVI"),
#'   data_dir = data_dir,
#'   parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # plot the data cube
#' sits_view(modis_cube, red = "EVI", green = "NDVI", blue = "EVI", times = 1)
#' }
#'
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
    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "Please install package leaflet"
    )
    # first select unique locations
    x <- dplyr::distinct(x,
                         .data[["longitude"]],
                         .data[["latitude"]],
                         .data[["label"]])
    # convert tibble to sf
    samples <- sf::st_as_sf(
        x[c("longitude", "latitude", "label")],
        coords = c("longitude", "latitude"),
        crs = 4326
    )
    # get the bounding box
    samples_bbox <- sf::st_bbox(samples)
    dist_x <- (samples_bbox[["xmax"]] - samples_bbox[["xmin"]])
    dist_y <- (samples_bbox[["ymax"]] - samples_bbox[["ymin"]])
    lng_center <- samples_bbox[["xmin"]] + dist_x / 2.0
    lat_center <- samples_bbox[["ymin"]] + dist_y / 2.0
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
            leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) %>%
        leafem::addMouseCoordinates() %>%
        leaflet::flyToBounds(
            lng1 = samples_bbox[["xmin"]],
            lat1 = samples_bbox[["ymin"]],
            lng2 = samples_bbox[["xmax"]],
            lat2 = samples_bbox[["ymax"]]
        ) %>%
        leaflet::addCircleMarkers(
            data = samples,
            popup = ~label,
            color = ~ factpal(label),
            radius = 4,
            stroke = FALSE,
            fillOpacity = 1,
            group = "Samples"
        ) %>%
        leaflet::addLayersControl(
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
                                  dates = sits_timeline(x)[1],
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "default") {
    dots <- list(...)
    # preconditions
    # Remote files not working in Windows (bug in stars)
    if (.Platform$OS.type == "windows") {
        path <- .file_info_path(x[1,])
        if (grepl("^/vsi", path)) {
            stop("sits_view not working in Windows OS for remote files",
                 call. = FALSE)
        }
    }
    # verifies if leafem and leaflet packages are installed
    .check_that(
        requireNamespace("leafem", quietly = TRUE),
        msg = "Plase install package 'leafem'"
    )
    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "Please install package 'leaflet'"
    )
    # deal with parameter "time"
    if ("time" %in% names(dots)) {
        warning("time parameter is deprecated, please use dates")
        dates <- sits_timeline(x)[as.integer(dots[["time"]])]
    }
    # deal with parameter "times"
    if ("times" %in% names(dots)) {
        warning("times parameter is deprecated, please use dates")
        dates <- sits_timeline(x)[as.numeric(dots[["times"]])]
    }
    # deal with parameter "date"
    if ("date" %in% names(dots)) {
        warning("use dates instead of date")
        dates <- as.Date(dots[["date"]])
    }
    # deal with wrong parameter "tile"
    if ("tile" %in% names(dots) && missing(tiles)) {
        message("please use tiles instead of tile as parameter")
        tiles <- dots[["tile"]]
    }
    # deal with tiles
    # check if tile exists
    if (purrr::is_null(tiles)) {
        tiles <- x$tile[[1]]
    } else {
        if (is.numeric(tiles)) {
            tiles <- x$tile[[tiles]]
        }
    }
    # try to find tiles in the list of tiles of the cube
    .check_chr_contains(
        x$tile,
        tiles,
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
        purrr::is_null(band) ||
            (purrr::is_null(red) &&
                 purrr::is_null(green) &&
                 purrr::is_null(blue)),
        local_msg = paste0("either 'band' parameter or 'red', 'green', and",
                           "'blue' parameters should be informed")
    )

    # check if rgb bands were informed
    if (!purrr::is_null(red) ||
        !purrr::is_null(green) ||
        !purrr::is_null(blue)) {

        # check if all RGB bands is not null
        .check_that(
            !purrr::is_null(red) &&
                !purrr::is_null(green) &&
                !purrr::is_null(blue),
            local_msg = "missing red, green, or blue bands",
            msg = "invalid RGB bands"
        )
    } else {

        # get default band (try first non-cloud band...)
        if (purrr::is_null(band)) {
            band <- .cube_bands(x, add_cloud = FALSE)
            if (length(band) > 0) {
                band <- band[[1]]
            } else {
                # ...else get cloud band
                band <- .cube_bands(x)[[1]]
            }
        }

        # plot as grayscale
        red <- band
        green <- band
        blue <- band
    }

    # filter the tiles to be processed
    cube_tiles <- dplyr::filter(x, .data[["tile"]] %in% tiles)

    # verifies if cube has a single timeline
    timeline <- sits_timeline(cube_tiles)
    .check_that(!is.list(timeline),
                local_msg = "more than one timeline per cube",
                msg = "cannot visualize cube"
    )

    # check that times are valid
    .check_that(all(as.Date(dates) %in% timeline),
                msg = paste0("requested dates are not part of the cube timeline")
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
            leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) %>%
        leafem::addMouseCoordinates()

    # obtain the raster objects for the dates chosen
    for (date in dates) {
        st_objs <- slider::slide(cube_tiles, function(tile) {
            # retrieve the file info for the tile
            fi <- .file_info(tile)
            date <- lubridate::as_date(date)
            # filter by date
            images_date <- dplyr::filter(fi, as.Date(.data[["date"]]) == !!date)
            if (purrr::is_null(band)) {
                red_file <- dplyr::filter(images_date,
                                          .data[["band"]] == red)$path[[1]]
                green_file <- dplyr::filter(images_date,
                                            .data[["band"]] == green)$path[[1]]
                blue_file <- dplyr::filter(images_date,
                                           .data[["band"]]  == blue)$path[[1]]
                rgb_files <- c(r = red_file, g = green_file, b = blue_file)
            } else {
                band_file <- dplyr::filter(images_date,
                                           .data[["band"]] == !!band)$path[[1]]
                rgb_files <- c(r = band_file, g = band_file, b = band_file)
            }
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
        # mosaic the data
        # if there is more than one stars object, merge them
        if (length(st_objs) > 1) {
            st_merge <- stars::st_mosaic(
                st_objs[[1]],
                st_objs[[2:length(st_objs)]]
            )
        } else {
            # keep the first object
            st_merge <- st_objs[[1]]
        }
        # resample and warp the image
        st_obj_new <- stars::st_warp(
            src = st_merge,
            crs = sf::st_crs("EPSG:3857")
        )
        if (purrr::is_null(band)) {
            leaf_map <- leafem::addRasterRGB(
                leaf_map,
                x = st_obj_new,
                r = 1,
                g = 2,
                b = 3,
                quantiles = c(0.1, 0.9),
                project = FALSE,
                group = paste0(date),
                maxBytes = size["leaflet_maxBytes"]
            )
        } else {
            leaf_map <- leafem::addRasterRGB(
                leaf_map,
                x = st_obj_new,
                r = 1,
                g = 1,
                b = 1,
                quantiles = c(0.1, 0.9),
                project = FALSE,
                group = paste0(date),
                maxBytes = size["leaflet_maxBytes"]
            )
        }
    }
    # should we overlay a classified image?
    if (!purrr::is_null(class_cube)) {
        # get the labels
        labels <- sits_labels(class_cube)
        names(labels) <- c(1:length(labels))
        # obtain the colors
        colors <- .view_get_colors(
            labels = labels,
            legend = legend,
            palette = palette
        )
        # select the tiles that will be shown
        cube_tiles <- dplyr::filter(class_cube, .data[["tile"]] %in% tiles)

        # create the stars objects that correspond to the tiles
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
        # if there is more than one stars object, merge them
        if (length(st_objs) > 1) {
            st_merge <- stars::st_mosaic(
                st_objs[[1]],
                st_objs[[2:length(st_objs)]]
            )
        } else {
            # keep the first object
            st_merge <- st_objs[[1]]
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
            maxBytes = size["leaflet_maxBytes"]
        )
    }
    # define overlay groups
    if (!purrr::is_null(class_cube)) {
        overlay_grps <- c(paste0(dates), "classification")
    } else {
        overlay_grps <- paste0(dates)
    }
    # add layers control to leafmap
    leaf_map <- leaf_map %>%
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = overlay_grps,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    # add class cube
    if (!purrr::is_null(class_cube)) {
        leaf_map <- leaf_map %>%
            leaflet::addLegend(
                "topright",
                pal     = fact_pal,
                values  = labels,
                title   = "Classes",
                opacity = 1
            )
    }
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
    dots <- list(...)
    # preconditions
    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "Please install package 'leaflet'"
    )
    # deal with wrong parameter "tile"
    if ("tile" %in% names(dots) && missing(tiles)) {
        message("please use tiles instead of tile as parameter")
        tiles <- dots[["tile"]]
    }
    # deal with tiles
    # check if tile exists
    if (purrr::is_null(tiles)) {
        tiles <- x$tile[[1]]
    } else {
        if (is.numeric(tiles)) {
            tiles <- x$tile[[tiles]]
        }
    }
    # try to find tiles in the list of tiles of the cube
    .check_chr_contains(x$tile, tiles,
                        msg = "requested tiles are not part of cube"
    )
    # get the labels
    labels <- sits_labels(x)
    names(labels) <- c(1:length(labels))
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
    # if there is more than one stars object, merge them
    if (length(st_objs) > 1) {
        st_merge <- stars::st_mosaic(
            st_objs[[1]],
            st_objs[[2:length(st_objs)]]
        )
    } else {
        # keep the first object
        st_merge <- st_objs[[1]]
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
            leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) %>%
        leafem::addMouseCoordinates() %>%
        leafem::addStarsImage(
            x = st_obj_new,
            colors = colors,
            method = "ngb",
            group = "classification",
            project = FALSE,
            maxBytes = size["leaflet_maxBytes"]
        ) %>%
        # add the the layers control
        leaflet::addLayersControl(
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
#' @return               Colors for legand of classified image.
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
    max_Mbytes <- .config_get(key = "leaflet_max_Mbytes")
    # get the compression factor
    comp <- .config_get(key = "leaflet_comp_factor")

    # calculate the size of the input image in bytes
    # note that leaflet considers 4 bytes per pixel
    in_size_Mbytes <- 4 * nrows * ncols * comp * ntiles
    # do we need to compress?
    ratio <- max((in_size_Mbytes / (max_Mbytes * ntiles * 1024 * 1024)), 1)
    # only create local files if required
    if (ratio > 1) {
        new_nrows <- round(nrows / sqrt(ratio))
        new_ncols <- round(ncols * (new_nrows / nrows))
    } else {
        new_nrows <- round(nrows)
        new_ncols <- round(ncols)
    }
    leaflet_maxBytes <- 4 * new_nrows * new_ncols * ntiles
    return(c(
        "xsize" = new_ncols, "ysize" = new_nrows,
        "leaflet_maxBytes" = leaflet_maxBytes
    ))
}
