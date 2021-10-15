#' @title  Generic interface for visualization of data cube
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
#' @param  roi           sf object giving a region of interest.
#' @param  map           map to overlay (mapview object)
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
#' sits_view(modis_cube, red = "EVI", green = "NDVI", blue = "EVI", times = 1)
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
        msg = "this function depends on 'leaflet' package"
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
                                  map = NULL,
                                  red,
                                  green,
                                  blue,
                                  times = c(1),
                                  roi = NULL) {

    # preconditions
    # verifies if leafem and leaflet packages are installed
    .check_that(
        requireNamespace("leafem", quietly = TRUE),
        msg = "this function depends on 'leafem' package"
    )
    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "this function depends on 'leaflet' package"
    )
    # verifies if raster package is installed
    .check_that(
        x = requireNamespace("raster", quietly = TRUE),
        msg = "this function depends on 'raster' package"
    )
    # check that the RGB bands are available in the cube
    .check_that(
        x = all(c(red, green, blue) %in% sits_bands(x)),
        msg = "requested RGB bands are not available in data cube"
    )

    timeline <- sits_timeline(x)
    .check_that(
        x = length(times) >= 1 & length(times) <= length(timeline),
        msg = "time parameter out of bounds"
    )

    # verify sf package if roi is informed
    if (!purrr::is_null(roi)) {
        if (!requireNamespace("sf", quietly = TRUE)) {
            stop("Please install package sf.", call. = FALSE)
        }

        # filter only intersecting tiles
        intersects <- slider::slide(x, function(row) {
            .sits_raster_sub_image_intersects(row, roi)
        }) %>% unlist()

        # check if intersection is not empty
        .check_that(
            x = any(intersects),
            msg = "informed roi does not intersect cube"
        )
        x <- x[intersects, ]
    }
    # filter the cube for the bands to be displayed
    cube_bands <- sits_select(x, bands = c(red, green, blue))

    max_Mbytes <- .config_get(key = "leaflet_max_Mbytes")
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

        r_obj <- .view_reshape_image(cube = cube_bands,
                                     rgb_files = rgb_files,
                                     roi = roi,
                                     max_Mbytes = max_Mbytes)

        return(r_obj)
    })

    leaf_mapRGB <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI") %>%
        leaflet::addProviderTiles(leaflet::providers$GeoportailFrance.orthos, group = "GeoPortalFrance") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM")

    for (t in seq_along(times)) {
        leaf_mapRGB <- suppressWarnings(leafem::addRasterRGB(leaf_mapRGB,
                             x = r_objs[[t]],
                             r = 1,
                             g = 2,
                             b = 3,
                             quantiles = c(0.1, 0.9),
                             # maxpixels = maxpixels,
                             # na.color = na.color,
                             method = "ngb",
                             group = paste0(timeline[times[t]]),
                             maxBytes = max_Mbytes*1024*1024))
    }
    leaf_mapRGB <- leaf_mapRGB %>%
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = paste0(timeline[times]),
            options = leaflet::layersControlOptions(collapsed = FALSE))

    return(leaf_mapRGB)
}

#' @rdname sits_view
#'
#' @export
#'
sits_view.classified_image <- function(x,...,
                                       map = NULL,
                                       legend = NULL) {

    # get the labels
    labels <- sits_labels(x)

    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        legend <- .config_palette_colors(labels)
        names(legend) <- labels
    }
    else {
        .check_chr_within(
            x =  labels,
            within = names(legend),
            msg = "some labels are missing from the legend"
        )
    }

    #
    # create a pallete of colors
    #
    factpal <- leaflet::colorFactor(
        palette = colors,
        domain = labels
    )

    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI") %>%
        leaflet::addProviderTiles(leaflet::providers$GeoportailFrance.orthos, group = "GeoPortalFrance") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM")

    for (r in seq_along(nrow(x))) {
        # obtain the raster
        r_obj <- suppressWarnings(raster::stack(x[r,]$file_info[[1]]$path[[1]]))
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
        colors <- unname(legend[rat$landcover])

        leaf_map <- leaflet::addRasterImage(leaf_map,
                                            x = r_obj,
                                            colors = colors,
                                            method = "ngb",
                                            group = "class",
                                            maxBytes = max_Mbytes*1024*1024
        )
    }
    leaf_map <- leaf_map %>%
        leaflet::addLayersControl(
            baseGroups = c("ESRI", "GeoPortalFrance", "OSM"),
            overlayGroups = "class",
            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::addLegend("topright",
                           pal     = factpal,
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
#' @param  roi           region of interest
#' @param  max_Mbytes    maximum number of megabytes to be shown in leaflet
#' @keywords internal

.view_reshape_image <- function(cube,
                          rgb_files,
                          roi = NULL,
                          max_Mbytes) {

    # use the raster package to obtain a raster object from a stack
    r_obj <- suppressWarnings(raster::stack(rgb_files))
    .check_that(
        x = raster::ncol(r_obj) > 0 && raster::nrow(r_obj) > 0,
        msg = "unable to retrieve raster data"
    )
    # extract bbox
    if (!purrr::is_null(roi)) {
        sub_image <- .sits_raster_sub_image(cube = cube, roi = roi)
        r_obj <- .raster_crop.raster(r_obj = r_obj, block = sub_image)
    }
    else
        sub_image <- .sits_raster_sub_image_default(cube = cube)

    # get number of rows and cols
    ncols <- raster::ncol(r_obj)
    nrows <- raster::nrow(r_obj)

    # retrieve the compression ratio
    comp <- .config_get("leaflet_comp_factor")
    # calculate the size of the input image in bytes
    # note that leaflet considers 4 bytes per pixel
    # but compresses the image
    in_size_Mbytes <- ceiling((4 * nrows * ncols * comp)/(1000 * 1000))
    # do we need to compress?
    ratio <- max((in_size_Mbytes/max_Mbytes), 1)

    # only create local files if required
    if (ratio > 1) {
        message("Please wait...resampling images")
        new_nrows <- floor(nrows/sqrt(ratio))
        new_ncols <- floor(ncols/sqrt(ratio))

        temp_files <- purrr::map2_chr(rgb_files, c("r", "g", "b"),
                                      function(f, c) {
            # destination file is in tempdir
            dest_file <- paste0(tempdir(),"/", basename(rgb_files[[c]]))
            # use gdal_translate to obtain the temp file
            suppressWarnings(
                gdalUtilities::gdal_translate(
                    src_dataset = rgb_files[[c]],
                    dst_dataset = dest_file,
                    srcwin     = c(sub_image[["first_col"]] - 1,
                                   sub_image[["first_row"]] - 1,
                                   sub_image[["ncols"]],
                                   sub_image[["nrows"]]),
                    outsize     = c(new_ncols, new_nrows),
                    co = c("COMPRESS=LZW")
                )
            )
            return(dest_file)
        })
        # if temp_files are created use them as sources for r_obj
        r_obj <- suppressWarnings(raster::stack(temp_files))
    }
    return(r_obj)
}
