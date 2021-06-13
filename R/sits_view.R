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
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#' # plot the data cube
#' sits_view(cbers_022024, red = "B15", green = "B16", blue = "B13", time = 1)
#' }
#'
#' @export
sits_view <- function(x, ...){

    assertthat::assert_that(
        inherits(x, c("raster_cube", "classified_image")),
        msg = "sits_view only works with raster cube and classified images")

    # verifies if raster package is installed
    assertthat::assert_that(
        requireNamespace("raster", quietly = TRUE),
        msg = "sits_view: this function depends on 'raster' package"
    )

    # verifies if mapview package is installed
    assertthat::assert_that(
        requireNamespace("mapview", quietly = TRUE),
        msg = "sits_view: this function depends on 'mapview' package"
    )

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

    # preconditions
    assertthat::assert_that(
        all(c(red, green, blue) %in% sits_bands(x)),
        msg = "sits_view: requested RGB bands are not available in data cube"
    )

    timeline <- sits_timeline(x)
    assertthat::assert_that(
        time >= 1 & time <= length(timeline),
        msg = "sits_view: time parameter out of bounds"
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
        assertthat::assert_that(
            any(intersects),
            msg = "sits_view: informed roi does not intersect cube"
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
    r_obj <- .sits_raster_api_open_stack.raster(rgb_files)

    # extract region of interest
    if (!purrr::is_null(roi)) {

        roi <- .sits_raster_sub_image(cube = x, roi = roi)

        r_obj <- .sits_raster_api_crop.raster(r_obj = r_obj, block = roi)
    }

    assertthat::assert_that(
        .sits_raster_api_ncols(r_obj) > 0 && .sits_raster_api_nrows(r_obj) > 0,
        msg = "view.raster_cube: unable to retrieve raster data"
    )

    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "GeoportailFrance.orthos",
        "Esri.WorldImagery"
    ))

    # view the RGB file
    mv <- suppressWarnings(
        mapview::viewRGB(r_obj,
                         r = 1,
                         g = 2,
                         b = 3,
                         layer.name = paste0("Time ", time))
    )

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
        legend <- .sits_config_colors(labels)
        names(legend) <- labels
    }
    else {
        assertthat::assert_that(
            all(labels %in% names(legend)),
            msg = "sits_view: some labels are missing from the legend"
        )
    }

    # obtain the raster
    r_obj <- .sits_raster_api_open_rast.raster(
        file = x$file_info[[1]]$path[[time]]
    )[[1]]

    assertthat::assert_that(
        .sits_raster_api_ncols.raster(r_obj) > 0 &&
            .sits_raster_api_nrows.raster(r_obj) > 0,
        msg = "plot: unable to retrive raster data"
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

    # set mapview options
    mapview::mapviewOptions(basemaps = c(
        "GeoportailFrance.orthos",
        "Esri.WorldImagery"
    ))

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
