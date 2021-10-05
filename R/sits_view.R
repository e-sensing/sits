#' @title  Generic interface for visualization of data cube
#' @name sits_view
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses mapview to visualize time series, raster cube and classified images
#'
#' @param  x             object of class "sits", "raster_cube" or "classified image"
#' @param  ...           further specifications for \link{sits_view}.
#' @param  red           band for red color.
#' @param  green         band for green color.
#' @param  blue          band for blue color.
#' @param  time          temporal instances to be plotted.
#' @param  roi           sf object giving a region of interest.
#' @param  map           map to overlay (mapview object)
#' @param  legend        named vector that associates labels to colors
#' @param  palette       palette provided in the configuration file
#'
#' @return               mapview object
#'
#' @examples
#' \donttest{
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
#' modis_cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "modis_sinop",
#'     origin = "BDC",
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

    # verifies if raster package is installed
    .check_that(
        x = requireNamespace("raster", quietly = TRUE),
        msg = "this function depends on 'raster' package"
    )

    # verifies if mapview package is installed
    .check_that(
        requireNamespace("mapview", quietly = TRUE),
        msg = "this function depends on 'mapview' package"
    )
    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "Esri.WorldImagery",
        "GeoportailFrance.orthos",
        "OpenStreetMap.Mapnik"
    ))

    UseMethod("sits_view", x)
}
#' @rdname   sits_view
#'
#' @export
sits_view.sits <- function(x,
                           ...,
                           legend = NULL,
                           palette = "default") {


    # first select unique locations
    x <- dplyr::distinct(x, longitude, latitude, label)
    # convert tibble to sf
    samples <- sf::st_as_sf(x[c("longitude", "latitude", "label")],
                         coords = c("longitude", "latitude"),
                         crs = 4326)

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

    # show samples in map
    mv <- suppressWarnings(
        mapview::mapview(samples,
                     zcol = c("label"),
                     legend = TRUE,
                     col.regions = colors
                     )
    )
    return(mv)
}
#' @rdname   sits_view
#'
#' @export
sits_view.raster_cube <- function(x, ...,
                                  map = NULL,
                                  red,
                                  green,
                                  blue,
                                  time = 1,
                                  roi = NULL) {

    # preconditions
    .check_that(
        x = all(c(red, green, blue) %in% sits_bands(x)),
        msg = "requested RGB bands are not available in data cube"
    )

    timeline <- sits_timeline(x)
    .check_that(
        x = time >= 1 & time <= length(timeline),
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

    # plot only the selected tile
    # select only the bands for the timeline
    bands_date <- x$file_info[[1]] %>%
        dplyr::filter(date == as.Date(timeline[[time]]))

    # get RGB files for the requested timeline
    red_file <- dplyr::filter(bands_date, band == red)$path
    green_file <- dplyr::filter(bands_date, band == green)$path
    blue_file <- dplyr::filter(bands_date, band == blue)$path

    rgb_files <- c(red_file, green_file, blue_file)

    # use the raster package to obtain a raster object from a stack
    r_obj <- .raster_open_stack.raster(rgb_files)

    # extract region of interest
    if (!purrr::is_null(roi)) {

        roi <- .sits_raster_sub_image(cube = x, roi = roi)

        r_obj <- .raster_crop.raster(r_obj = r_obj, block = roi)
    }

    .check_that(
        x = .raster_ncols(r_obj) > 0 && .raster_nrows(r_obj) > 0,
        msg = "unable to retrieve raster data"
    )

    # view the RGB file
    if (!purrr::is_null(map)) {
        mv <- suppressWarnings(
            mapview::viewRGB(r_obj,
                             map = map,
                             r = 1,
                             g = 2,
                             b = 3,
                             layer.name = paste0("Time ", time))
        )
    }
    else {
        mv <- suppressWarnings(
            mapview::viewRGB(r_obj,
                             r = 1,
                             g = 2,
                             b = 3,
                             layer.name = paste0("Time ", time))
        )
    }
    return(mv)
}

#' @rdname sits_view
#'
#' @export
#'
sits_view.classified_image <- function(x,...,
                                       map = NULL,
                                       time = 1,
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

    # obtain the raster
    r_obj <- .raster_open_rast.raster(
        file = x$file_info[[1]]$path[[time]]
    )[[1]]

    .check_that(
        x = .raster_ncols.raster(r_obj) > 0 &&
            .raster_nrows.raster(r_obj) > 0,
        msg = "unable to retrive raster data"
    )

    # create a RAT
    r_obj <- raster::ratify(r_obj)
    rat <- raster::levels(r_obj)[[1]]

    # include labels in the RAT
    # be careful - some labels may not exist in the classified image
    rat$landcover <- labels[rat$ID]
    colors <- unname(legend[rat$landcover])

    # assign the RAT to the raster object
    levels(r_obj) <- rat

    # use mapview
    if (!purrr::is_null(map))
        mv <- suppressWarnings(
            mapview::mapview(x = r_obj,
                             map = map,
                             col.regions = colors)
        )
    else
        mv <- suppressWarnings(
            mapview::mapview(x = r_obj,
                             col.regions = colors)
        )

    return(mv)
}
