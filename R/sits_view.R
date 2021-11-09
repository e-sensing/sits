#' @title  View data cubes and samples in leaflet
#' @name sits_view
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses leaflet to visualize time series, raster cube and classified images
#'
#' @param  x             object of class "sits", "raster_cube" or "classified image"
#' @param  ...           further specifications for \link{sits_view}.
#' @param  red           band for red color.
#' @param  green         band for green color.
#' @param  blue          band for blue color.
#' @param  times         temporal instances to be plotted.
#' @param  tile          tile to be plotted (in case of a multi-tile cube).
#' @param  class_cube    classified cube to be overlayed on top on image
#' @param  legend        named vector that associates labels to colors
#' @param  palette       palette provided in the configuration file
#'
#' @return               leaflet object
#'
#' @examples
#' \donttest{
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
#' modis_cube <- sits_cube(
#'     source = "BDC",
#'     collection = "MOD13Q1-6",
#'     band = "NDVI",
#'     data_dir = data_dir,
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # plot the data cube
#' sits_view(modis_cube, red = "EVI", green = "NDVI", blue = "EVI", time = 1)
#' }
#'
#' @export
sits_view <- function(x, ...){

    # set caller to show in errors
    .check_set_caller("sits_view")

    .check_that(
        x = inherits(x, c("sits", "raster_cube", "classified_image")),
        msg = "only works with time series, raster cubes and classified images")

    UseMethod("sits_view", x)
}
#' @rdname   sits_view
#'
#' @export
sits_view.sits <- function(x,
                           ...,
                           legend = NULL,
                           palette = "default") {

    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "Please install package leaflet"
    )
    # first select unique locations
    x <- dplyr::distinct(x, longitude, latitude, label)
    # convert tibble to sf
    samples <- sf::st_as_sf(x[c("longitude", "latitude", "label")],
                         coords = c("longitude", "latitude"),
                         crs = 4326)
    # get the bounding box
    samples_bbox <- sf::st_bbox(samples)
    dist_x <- (samples_bbox[["xmax"]] - samples_bbox[["xmin"]])
    dist_y <- (samples_bbox[["ymax"]] - samples_bbox[["ymin"]])
    lng_center <- samples_bbox[["xmin"]]  + dist_x/2.0
    lat_center <- samples_bbox[["ymin"]] + dist_y/2.0
    # get the labels
    labels <- sits_labels(x)

    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .config_palette_colors(labels, palette = palette)
    }
    else {
        .check_chr_within(
            x = labels,
            within = names(legend),
            msg = "some labels are missing from the legend")
        colors <- unname(legend[labels])

    }
    #
    # create a pallete of colors
    #
    factpal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )
    #
    # create an interative map
    #
    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI") %>%
        leaflet::addProviderTiles(leaflet::providers$GeoportailFrance.orthos, group = "GeoPortalFrance") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group = "OSM") %>%
        leaflet::flyToBounds(lng1 = samples_bbox[["xmin"]],
                             lat1 = samples_bbox[["ymin"]],
                             lng2 = samples_bbox[["xmax"]],
                             lat2 = samples_bbox[["ymax"]]) %>%
        leaflet::addCircleMarkers(data   = samples,
                                  popup  = ~label,
                                  color  = ~factpal(label),
                                  radius = 4,
                                  stroke = FALSE,
                                  fillOpacity = 1,
                                  group = "Samples") %>%
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = c("Samples"),
            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::addLegend("topright",
                           pal     = factpal,
                           values  = samples$label,
                           title   = "Training Samples",
                           opacity = 1)
    return(leaf_map)
}
#' @rdname   sits_view
#'
#' @export
sits_view.raster_cube <- function(x, ...,
                                  red,
                                  green,
                                  blue,
                                  tile  = 1,
                                  times = c(1),
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "default") {

    # preconditions
    # pick only the selected tile
    x <- x[tile,]
    # verifies if leafem and leaflet packages are installed
    .check_that(
        requireNamespace("leafem", quietly = TRUE),
        msg = "Plase install package 'leafem'"
    )
    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "Please install package 'leaflet'"
    )
    # verifies if raster package is installed
    .check_that(
        x = requireNamespace("raster", quietly = TRUE),
        msg = "Please install package 'raster'"
    )
    # check that the RGB bands are available in the cube
    .check_that(
        x = all(c(red, green, blue) %in% sits_bands(x)),
        msg = "requested RGB bands are not available in data cube"
    )
    # check that classified map is a proper cube
    if (!purrr::is_null(class_cube))
        .check_that(
            x = inherits(class_cube, c("classified_image")),
            msg = "classified cube to be overlayed is invalid")
    # check that times are valid
    timeline <- sits_timeline(x)
    .check_that(
        x = times >= 1 & times <= length(timeline),
        msg = paste0("time parameter out of bounds: should be between 1 and ",
                     length(timeline))
    )
    # check that requested tile exists
    .check_that(
        x = tile >= 1 & tile <= nrow(x),
        msg = paste0("tile parameter out of bounds: should be between 1 and ",
                     nrow(x))
    )

    # get the maximum number of bytes to be displayed
    max_Mbytes <- .config_get(key = "leaflet_max_Mbytes")

    # filter the cube for the bands to be displayed
    cube_bands <- sits_select(x, bands = c(red, green, blue))

    # plot only the selected tiles
    # select only the bands for the times chosen
    r_objs <- purrr::map(times, function(t) {
        bands_date <- x$file_info[[1]] %>%
            dplyr::filter(date == as.Date(timeline[[t]]))

        # get RGB files for the requested timeline
        red_file <- dplyr::filter(bands_date, band == red)$path
        green_file <- dplyr::filter(bands_date, band == green)$path
        blue_file <- dplyr::filter(bands_date, band == blue)$path

        rgb_files <- c(r = red_file, g = green_file, b = blue_file)
        # compress and reshape the image
        r_obj <- .view_reshape_image(cube = cube_bands,
                                     rgb_files = rgb_files,
                                     max_Mbytes = max_Mbytes)

        return(r_obj)
    })
    # create a leaflet and add providers
    leaf_mapRGB <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI") %>%
        leaflet::addProviderTiles(leaflet::providers$GeoportailFrance.orthos, group = "GeoPortalFrance") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM")

    # include raster RGB maps
    for (t in seq_along(times)) {
        leaf_mapRGB <- suppressWarnings(
            leafem::addRasterRGB(leaf_mapRGB,
                                 x = r_objs[[t]],
                                 r = 1,
                                 g = 2,
                                 b = 3,
                                 quantiles = c(0.1, 0.9),
                                 method = "ngb",
                                 group = paste0(timeline[times[t]]),
                                 maxBytes = max_Mbytes*1024*1024))
    }
    # should we overlay a classified image?
    if (!purrr::is_null(class_cube)) {

        # get the labels
        labels <- sits_labels(class_cube)
        # obtain the colors
        colors <- .view_get_colors(labels  = labels,
                                   legend  = legend,
                                   palette = palette)

        # retrieve the classified object (which is RATified)
        r_obj_class <- .view_class_cube(class_cube = class_cube, tile = tile)

        # retrieve the colors of the r_obj
        # some labels may not be present in the final image
        rat <- raster::levels(r_obj_class)[[1]]
        colors <- unname(colors[rat$landcover])
        labels <- labels[rat$ID]
        .check_that(length(colors) == length(labels),
                    msg = "mismatch btw labels and colors in classified image")
        #
        # create a palette of colors
        #
        fact_pal <- leaflet::colorFactor(
            palette = colors,
            domain = labels
        )

        # add the classified image object
        leaf_mapRGB <- suppressWarnings(
            leaflet::addRasterImage(leaf_mapRGB,
                                    x = r_obj_class,
                                    colors = colors,
                                    method = "ngb",
                                    group = "classification",
                                    maxBytes = max_Mbytes*1024*1024)) %>%
            leaflet::addLegend("topright",
                               pal     = fact_pal,
                               values  = labels,
                               title   = "Classes",
                               opacity = 1)

        overlay_grps = c(paste0(timeline[times]), "classification")
    }
    else
        overlay_grps = paste0(timeline[times])

    leaf_mapRGB <- leaf_mapRGB %>%
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = overlay_grps,
            options = leaflet::layersControlOptions(collapsed = FALSE))

    return(leaf_mapRGB)
}

#' @rdname sits_view
#'
#' @export
#'
sits_view.classified_image <- function(x,...,
                                       tile,
                                       legend = NULL,
                                       palette = "default") {

    # get the labels
    labels <- sits_labels(x)
    # obtain the colors
    colors <- .view_get_colors(labels = labels, legend = legend, palette = palette)

    # retrieve the classified object (which is RATified)
    r_obj <- .view_class_cube(x, tile)

    # retrieve the colors of the r_obj
    # some labels may not be present in the final image
    rat <- raster::levels(r_obj)[[1]]
    colors <- unname(colors[rat$landcover])
    labels <- labels[rat$ID]
    .check_that(length(colors) == length(labels),
                msg = "mismatch btw labels and colors in classified image")
    #
    # create a palette of colors
    #
    fact_pal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )

    # get the maximum number of bytes
    max_Mbytes <- .config_get(key = "leaflet_max_Mbytes")

    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI") %>%
        leaflet::addProviderTiles(leaflet::providers$GeoportailFrance.orthos, group = "GeoPortalFrance") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM")

    # add the classified image object
    leaf_map <- suppressWarnings(
        leaflet::addRasterImage(leaf_map,
                                x = r_obj,
                                colors = colors,
                                method = "ngb",
                                group = "class",
                                maxBytes = max_Mbytes*1024*1024
        )
    )

    # add the the layers control
    leaf_map <- leaf_map %>%
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = "class",
            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::addLegend("topright",
                           pal     = fact_pal,
                           values  = labels,
                           title   = "Classes",
                           opacity = 1)

    return(leaf_map)
}

#' @title  Reduce the cube size for visualisation and load files in tempdir
#' @name .view_reshape_image
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  x             object of "raster_cube" or "classified image"
#' @param  rgb_files     vector with RGB files.
#' @param  max_Mbytes    maximum number of megabytes to be shown in leaflet
#' @keywords internal

.view_reshape_image <- function(cube,
                          rgb_files,
                          max_Mbytes) {

    # open raster object
    r_obj <- suppressWarnings(raster::stack(rgb_files))
    # get number of rows and cols
    ncols <- raster::ncol(r_obj)
    nrows <- raster::nrow(r_obj)

    # retrieve the compression ratio
    comp <- .config_get("leaflet_comp_factor")
    # calculate the size of the input image in bytes
    # note that leaflet considers 4 bytes per pixel
    # but compresses the image
    in_size_Mbytes <- (4 * nrows * ncols * comp)/(1000 * 1000)
    # do we need to compress?
    ratio <- max((in_size_Mbytes/max_Mbytes), 1)

    # only create local files if required
    if (ratio > 1) {
        message("Please wait...resampling images")
        new_nrows <- round(nrows/sqrt(ratio))
        new_ncols <- round(ncols*(new_nrows/nrows))

        temp_files <- purrr::map2_chr(rgb_files, c("r", "g", "b"),
                                      function(f, c) {
            # destination file is in tempdir
            dest_file <- paste0(tempdir(),"/", basename(rgb_files[[c]]))
            # use gdal_translate to obtain the temp file
            suppressWarnings(
                gdalUtilities::gdalwarp(
                    srcfile     = rgb_files[[c]],
                    dstfile     = dest_file,
                    t_srs       = "EPSG:3857",
                    ts          = c(new_ncols, new_nrows),
                    co          = .config_get("gdal_creation_options")
                )
            )
            return(dest_file)
        })
        # if temp_files are created use them as sources for r_obj
        r_obj <- suppressWarnings(raster::stack(temp_files))
    }
    return(r_obj)
}

#' @title  Return the r_object associated to classified cube (RATified)
#' @name .view_class_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  class_cube    object of class "classified image"
#' @param  tile          tile to be plotted (in case of a multi-tile cube)
#' @keywords internal
#'
#'
.view_class_cube <- function(class_cube, tile){

    # get the labels
    labels <- sits_labels(class_cube)

    # obtain the raster
    r_obj <- suppressWarnings(raster::raster(class_cube[tile,]$file_info[[1]]$path[[1]]))
    # did we get the data?
    .check_that(
        x = raster::ncol(r_obj) > 0 &&
            raster::nrow(r_obj) > 0,
        msg = "unable to retrieve raster data"
    )
    # create a RAT
    r_obj <- raster::ratify(r_obj)
    rat <- raster::levels(r_obj)[[1]]

    # include labels in the RAT
    # be careful - some labels may not exist in the classified image
    rat$landcover <- labels[rat$ID]
    # assign the RAT to the raster object
    levels(r_obj) <- rat

    return(r_obj)
}

#' @title  Return the colors associated to the classified image
#' @name .view_get_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  labels        labels of the classified cube
#' @param  legend        named vector that associates labels to colors
#' @param  palette       palette provided in the configuration file
#' @keywords internal
#'
#'
.view_get_colors <- function(labels, legend, palette){
    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .config_palette_colors(labels, palette = palette)
    }
    else {
        .check_chr_within(
            x = labels,
            within = names(legend),
            msg = "some labels are missing from the legend")
        colors <- unname(legend[labels])

    }
    return(colors)
}
