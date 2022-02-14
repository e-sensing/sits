#' @title  View data cubes and samples in leaflet
#' @name sits_view
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses leaflet to visualize time series, raster cube and
#'  classified images
#'
#' @param  x             object of class "sits", "raster_cube" or
#'  "classified image"
#' @param  ...           further specifications for \link{sits_view}.
#' @param  band          for plotting grey images
#' @param  red           band for red color.
#' @param  green         band for green color.
#' @param  blue          band for blue color.
#' @param  dates         dates to be plotted
#' @param  tiles         tiles to be plotted (in case of a multi-tile cube).
#' @param  class_cube    classified cube to be overlayed on top on image
#' @param  legend        named vector that associates labels to colors
#' @param  palette       palette provided in the configuration file
#'
#' @return               leaflet object
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
#'     source = "BDC",
#'     collection = "MOD13Q1-6",
#'     bands = c("NDVI", "EVI"),
#'     data_dir = data_dir,
#'     parse_info = c("X1", "X2", "tile", "band", "date"),
#'     multicores = 1
#' )
#'
#' # plot the data cube
#' sits_view(modis_cube, red = "EVI", green = "NDVI", blue = "EVI", dates = "2013-09-14")
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
sits_view.sits <- function(x, ...,
                           legend = NULL,
                           palette = "Harmonic") {

    # precondition
    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "Please install package leaflet")

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
        colors <- .config_colors(labels = labels,
                                 palette = palette,
                                 rev = TRUE)
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
        domain = labels)
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
                                  band = NULL,
                                  red = NULL,
                                  green = NULL,
                                  blue = NULL,
                                  tiles  = NULL,
                                  dates = sits_timeline(x)[1],
                                  class_cube = NULL,
                                  legend = NULL,
                                  palette = "default") {


    dots <- list(...)
    # preconditions
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
        if (is.numeric(tiles))
            tiles <- x$tile[[tiles]]
    }
    # try to find tiles in the list of tiles of the cube
    .check_chr_contains(x$tile, tiles,
                        msg = "requested tiles are not part of cube")

    # check that classified map is a proper cube
    if (!purrr::is_null(class_cube))
        .check_that(
            x = inherits(class_cube, c("classified_image")),
            msg = "classified cube to be overlayed is invalid")

    # get the maximum number of bytes to be displayed
    max_Mbytes <- .config_get(key = "leaflet_max_Mbytes")

    # for plotting grey images
    if (purrr::is_null(band)) {
        # check that the RGB bands are available in the cube
        .check_that(
            x = all(c(red, green, blue) %in% sits_bands(x)),
            msg = "requested RGB bands are not available in data cube"
        )
    } else
        .check_that(band %in% sits_bands(x),
                    msg = "requested RGB bands are not available in data cube"
        )


    # filter the cube for the bands to be displayed
    # cube_bands <- sits_select(x, bands = c(red, green, blue))

    # filter the tiles to be processed
    cube_tiles <- dplyr::filter(x, tile %in% tiles)

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


    # plot only the selected tiles
    t_objs <- slider::slide(cube_tiles, function(tile){
        # retrieve the file info for the tile
        fi <- .file_info(tile)
        # obtain the raster objects for the dates chosen
        r_objs <- purrr::map(dates, function(d) {
            # filter by date
            images_date <- dplyr::filter(fi, date == as.Date(d))
            # deal with RGB images
            if (purrr::is_null(band)) {
                # get RGB files for the requested timeline
                red_file   <- dplyr::filter(images_date, band == red)$path[[1]]
                green_file <- dplyr::filter(images_date, band == green)$path[[1]]
                blue_file  <- dplyr::filter(images_date, band == blue)$path[[1]]

                rgb_files <- c(r = red_file, g = green_file, b = blue_file)
                # compress and reshape the image
                r_obj <- .view_reshape_rgb(rgb_files = rgb_files,
                                           date = as.Date(d),
                                           max_Mbytes = max_Mbytes)
            } else {
                # deal with single band images
                band_file   <- dplyr::filter(images_date, band == !!band)$path[[1]]
                r_obj <- .view_reshape_band(band_file = band_file,
                                            date = as.Date(d),
                                            max_Mbytes = max_Mbytes)
            }
            return(r_obj)
        })
        return(r_objs)
    })

    # create a leaflet and add providers
    leaf_mapRGB <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI") %>%
        leaflet::addProviderTiles(leaflet::providers$GeoportailFrance.orthos, group = "GeoPortalFrance") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
        leafem::addMouseCoordinates()

    # include raster RGB maps
    for (t_ind in seq_along(tiles)) {
        r_objs <- t_objs[[t_ind]]
        for (d_ind in seq_along(dates)) {
            if (purrr::is_null(band)) {
                leaf_mapRGB <- suppressWarnings(
                    leafem::addRasterRGB(leaf_mapRGB,
                                         x = r_objs[[d_ind]],
                                         r = 1,
                                         g = 2,
                                         b = 3,
                                         quantiles = c(0.1, 0.9),
                                         method = "ngb",
                                         group = paste0(dates[d_ind]),
                                         maxBytes = max_Mbytes*1024*1024))
            } else
                leaf_mapRGB <- suppressWarnings(
                    leafem::addRasterRGB(leaf_mapRGB,
                                         x = r_objs[[d_ind]],
                                         r = 1,
                                         g = 1,
                                         b = 1,
                                         quantiles = c(0.1, 0.9),
                                         method = "ngb",
                                         group = paste0(dates[d_ind]),
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
            r_obj_class <- .view_class_cube(class_cube = class_cube, tile = x$tile[[t_ind]])

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


        }
    }
    # define overlay groups
    if (!purrr::is_null(class_cube)) {
        overlay_grps = c(paste0(dates), "classification")
    }
    else
        overlay_grps = paste0(dates)

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
sits_view.classified_image <- function(x,...,
                                       tiles = NULL,
                                       legend = NULL,
                                       palette = "default") {

    dots <- list(...)
    # preconditions
    .check_that(
        requireNamespace("leaflet", quietly = TRUE),
        msg = "Please install package 'leaflet'"
    )
    # precondition - check tiles
    if (purrr::is_null(tiles))
        tiles <- x$tile
    .check_chr_contains(x$tile, tiles, msg = "tiles not available in the cube")

    # create lealet map
    leaf_map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI") %>%
        leaflet::addProviderTiles(leaflet::providers$GeoportailFrance.orthos, group = "GeoPortalFrance") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM")

    # get the labels
    labels <- sits_labels(x)
    # obtain the colors
    colors <- .view_get_colors(labels = labels, legend = legend, palette = palette)

    for (tile in tiles) {
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
    }

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

#' @title  Reduce an RGB image for visualisation and load files in tempdir
#' @name .view_reshape_rgb
#'
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  rgb_files     vector with RGB files.
#' @param  date          date reference for the file
#' @param  max_Mbytes    maximum number of megabytes to be shown in leaflet
#'
#' @return               Raster Stack with RGB object
.view_reshape_rgb <- function(rgb_files, date, max_Mbytes) {


    nrows <- purrr::map_int(c("r", "g", "b"), function(color) {
        # open raster object
        r_obj <- suppressWarnings(raster::stack(rgb_files[[color]]))
        # get number of rows
        return(raster::nrow(r_obj))
    })
    nrows_max <- max(nrows)

    ncols <- purrr::map_int(c("r", "g", "b"), function(color) {
        # open raster object
        r_obj <- suppressWarnings(raster::stack(rgb_files[[color]]))
        # get number of cols
        return(raster::ncol(r_obj))
    })
    ncols_max <- max(ncols)

    # retrieve the compression ratio
    comp <- .config_get("leaflet_comp_factor")
    # calculate the size of the input image in bytes
    # note that leaflet considers 4 bytes per pixel
    # but compresses the image
    in_size_Mbytes <- (4 * nrows_max * ncols_max * comp)/(1000 * 1000)
    # do we need to compress?
    ratio <- max((in_size_Mbytes/max_Mbytes), 1)

    # only create local files if required
    message("Please wait...resampling images")
    if (ratio > 1) {
        new_nrows <- round(nrows_max/sqrt(ratio))
        new_ncols <- round(ncols_max*(new_nrows/nrows_max))
    } else {
        new_nrows <- nrows_max
        new_ncols <- ncols_max
    }
    r_obj_green <- suppressWarnings(raster::stack(rgb_files[["g"]]))
    t_extent  <- c(raster::xmin(r_obj_green), raster::ymin(r_obj_green),
                   raster::xmax(r_obj_green), raster::ymax(r_obj_green))
    t_srs     <- suppressWarnings(paste0(raster::crs(r_obj_green)))

    temp_files <- purrr::map_chr(c("r", "g", "b"), function(color) {
        # destination file is in tempdir
        b_name <- basename(tools::file_path_sans_ext(rgb_files[[color]]))
        dest_file <- paste0(tempdir(),"/", b_name,  "_",date,".tif")
        # use gdal_translate to obtain the temp file
        suppressWarnings(
            gdalUtilities::gdalwarp(
                srcfile     = rgb_files[[color]],
                dstfile     = dest_file,
                t_srs       = "EPSG:3857",
                ts          = c(new_ncols, new_nrows),
                te          = t_extent,
                te_srs      = t_srs,
                co          = .config_get("gdal_creation_options")
            )
        )
        return(dest_file)
    })
    # if temp_files are created use them as sources for r_obj
    r_obj <- suppressWarnings(raster::stack(temp_files))

    return(r_obj)
}

#' @title  Reduce B/W image for visualisation and load files in tempdir
#' @name .view_reshape_band
#'
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  band_file     file for B/W band.
#' @param  date          date reference for the file
#' @param  max_Mbytes    maximum number of megabytes to be shown in leaflet
#' @return               Raster Stack with RGB object
.view_reshape_band <- function(band_file, date, max_Mbytes) {

    # open raster object
    r_obj <- suppressWarnings(raster::raster(band_file))
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

        # destination file is in tempdir
        b_name <- basename(tools::file_path_sans_ext(band_file))
        dest_file <- paste0(tempdir(),"/", b_name,  "_", date, ".tif")
        # use gdal_translate to obtain the temp file
        suppressWarnings(
            gdalUtilities::gdalwarp(
                srcfile     = band_file,
                dstfile     = dest_file,
                t_srs       = "EPSG:3857",
                ts          = c(new_ncols, new_nrows),
                co          = .config_get("gdal_creation_options")
            )
        )

        # if temp_files are created use them as sources for r_obj
        r_obj <- suppressWarnings(raster::stack(dest_file))
    }
    return(r_obj)
}

#' @title  Return the r_object associated to classified cube (RATified)
#' @name .view_class_cube
#'
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  class_cube    object of class "classified image"
#' @param  tile          tile to be plotted (in case of a multi-tile cube)
#'
#' @return a \code{raster} object
.view_class_cube <- function(class_cube, tile){

    # get the labels
    labels <- sits_labels(class_cube)

    class_cube <- dplyr::filter(class_cube, tile == !!tile)
    # obtain the raster
    r_obj <- suppressWarnings(
        raster::raster(.file_info_path(class_cube))
    )
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
#'
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  labels        labels of the classified cube
#' @param  legend        named vector that associates labels to colors
#' @param  palette       palette provided in the configuration file
#'
#' @return a \code{character} vector with colors palette.
.view_get_colors <- function(labels, legend, palette){
    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        colors <- .config_colors(labels = labels,
                                 palette = palette,
                                 rev = TRUE)
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
