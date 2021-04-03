#' @title  Generic interface for visualization of data cube
#' @name sits_view
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Uses mapview to visualize raster cube and classified images
#'
#' @param  x             object of class "raster_cube" or "classified image"
#' @param  ...           further specifications for \link{sits_view}.
#' @param  red           band for red color.
#' @param  green         band for green color.
#' @param  blue          band for blue color.
#' @param  time          temporal instances to be plotted.
#' @param  roi           sf object giving a region of interest.
#' @param  map           map to overlay (mapview object)
#' @param  legend        named vector that associates labels to colors
#'
#' @return               mapview object
#'
#' @examples
#' \donttest{
#' data_dir <- system.file("extdata/raster/cbers", package = "sits")
#'
#' cbers_022024 <- sits_cube(
#'     source = "LOCAL",
#'     name = "cbers_022024",
#'     satellite = "CBERS-4",
#'     sensor = "AWFI",
#'     resolution = 64,
#'     data_dir = data_dir,
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#' # plot the data cube
#' sits_view(cbers_022024, red = "B15", green = "B16", blue = "B13", time = 1)
#' }
#'
#' @export
sits_view <- function(x, ...){

    assertthat::assert_that(
        class(x)[1] %in% c("raster_cube", "classified_image"),
        msg = "sits_view only works with raster cube and classified images")

    UseMethod("sits_view", x)
}
#' @rdname   sits_view
#'
#' @export
sits_view.raster_cube <- function(x, ...,
                             red,
                             green,
                             blue,
                             time = 1,
                             roi = NULL) {

    # verifies if mapview package is installed
    if (!requireNamespace("mapview", quietly = TRUE)) {
        stop("Please install package mapview.", call. = FALSE)
    }
    # verifies if raster package is installed
    if (!requireNamespace("raster", quietly = TRUE)) {
        stop("Please install package raster.", call. = FALSE)
    }
    # preconditions
    assertthat::assert_that(all(c(red, green, blue) %in% sits_bands(x)),
                    msg = "requested RGB bands not available in data cube")

    timeline <- sits_timeline(x)
    assertthat::assert_that(time >= 1 & time <= length(timeline),
                            msg = "invalid time")

    # verify sf package if roi is informed
    if (!purrr::is_null(roi)) {
        if (!requireNamespace("sf", quietly = TRUE)) {
            stop("Please install package sf.", call. = FALSE)
        }

        # filter only intersecting tiles
        intersects <- slider::slide(x, function(row) {

            .sits_raster_sub_image_intersects(row, roi)
        }) %>% unlist()

        if (!any(intersects)) {
            stop("Informed roi does not intersect cube.", call. = FALSE)
        }
        x <- x[intersects, ]
    }
    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "GeoportailFrance.orthos",
        "Esri.WorldImagery"
    ))

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
    rast <- suppressWarnings(raster::stack(rgb_files))

    # extract region of interest
    if (!purrr::is_null(roi)) {
        roi <- raster::extent(sf::st_bbox(
            sf::st_transform(roi, crs = raster::crs(rast))))
        rast <- suppressWarnings(raster::crop(rast, roi))
    }

    assertthat::assert_that(
        .sits_raster_api_ncols(rast) > 0 && .sits_raster_api_nrows(rast) > 0,
        msg = "view.raster_cube: unable to retrieve raster data"
    )

    # view the RGB file
    mv <- suppressWarnings(mapview::viewRGB(
        rast, r = 1, g = 2, b = 3,
        layer.name = paste0("Time ", time)))

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

    # verifies if mapview package is installed
    if (!requireNamespace("mapview", quietly = TRUE)) {
        stop("Please install package mapview.", call. = FALSE)
    }
    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "GeoportailFrance.orthos",
        "Esri.WorldImagery"
    ))

    # get the labels
    labels <- sits_labels(x)
    # if colors are not specified, get them from the configuration file
    if (purrr::is_null(legend)) {
        legend <- .sits_config_colors(labels)
        names(legend) <- labels
    }
    else {
        assertthat::assert_that(all(labels %in% names(legend)),
                msg = "sits_view: some labels are missing from the legend")
    }

    # obtain the raster
    rl <- suppressWarnings(raster::raster(x$file_info[[1]]$path[time]))
    assertthat::assert_that(
        .sits_raster_api_ncols(rl) > 0 && .sits_raster_api_nrows(rl) > 0,
        msg = "plot.classified_image: unable to retrive raster data"
    )
    # create a RAT
    rl <- raster::ratify(rl)
    rat <- raster::levels(rl)[[1]]
    # include labels in the RAT
    # be careful - some labels may not exist in the classified image
    rat$landcover <- labels[rat$ID]
    colors <- unname(legend[rat$landcover])
    # assign the RAT to the raster object
    levels(rl) <- rat

    # use mapview
    if (!purrr::is_null(map))
        mv <- suppressWarnings(
            mapview::mapview(rl,
                             map = map,
                             col.regions = colors)
        )
    else
        mv <- suppressWarnings(
            mapview::mapview(rl,
                             col.regions = colors)
        )

    return(mv)
}
