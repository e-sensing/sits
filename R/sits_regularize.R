#' @title Build a regular data cube from an irregular one
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @name sits_regularize
#'
#' @description Produces regular data cubes for analysis-ready data (ARD)
#' image collections. Analysis-ready data (ARD) collections available in
#' AWS, MPC, USGS and DEAfrica are not regular in space and time.
#' Bands may have different resolutions,
#' images may not cover the entire time, and time intervals are not regular.
#' For this reason, subsets of these collection need to be converted to
#' regular data cubes before further processing and data analysis.
#' This function requires users to include the cloud band in their ARD-based
#' data cubes. This function uses the \code{gdalcubes} package.
#'
#' @references Appel, Marius; Pebesma, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092.
#'
#' @param cube        \code{raster_cube} object whose observation
#'                    period and/or spatial resolution is not constant.
#' @param ...         Additional parameters.
#' @param period      ISO8601-compliant time period for regular
#'                    data cubes, with number and unit, where
#'                    "D", "M" and "Y" stand for days, month and year;
#'                     e.g., "P16D" for 16 days.
#' @param res         Spatial resolution of regularized images (in meters).
#' @param output_dir  Valid directory for storing regularized images.
#' @param timeline    User-defined timeline for regularized cube.
#' @param roi         Region of interest (see notes below).
#' @param tiles       Tiles to be produced.
#' @param grid_system Grid system to be used for the output images.
#' @param multicores  Number of cores used for regularization;
#'                    used for parallel processing of input (integer)
#' @param progress    show progress bar?
#'
#'
#' @note
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies an ARD image collection
#'          from a cloud provider to a local directory for faster processing.}
#'      \item{\code{\link[sits]{sits_regularize}}: create a regular data cube
#'          from an ARD image collection.}
#'      \item{\code{\link[sits]{sits_apply}}: create new indices by combining
#'          bands of a  regular data cube (optional).}
#'      \item{\code{\link[sits]{sits_get_data}}: extract time series
#'          from a regular data cube based on user-provided labelled samples.}
#'      \item{\code{\link[sits]{sits_train}}: train a machine learning
#'          model based on image time series.}
#'      \item{\code{\link[sits]{sits_classify}}: classify a data cube
#'          using a machine learning model and obtain a probability cube.}
#'      \item{\code{\link[sits]{sits_smooth}}: post-process a probability cube
#'          using a spatial smoother to remove outliers and
#'          increase spatial consistency.}
#'      \item{\code{\link[sits]{sits_label_classification}}: produce a
#'          classified map by selecting the label with the highest probability
#'          from a smoothed cube.}
#' }
#'      The regularization operation converts subsets of image collections
#'      available in cloud providers into regular data cubes. It is an essential
#'      part of the \code{sits} workflow.
#'      The input to \code{sits_regularize} should be an ARD cube
#'      which includes the cloud band. The aggregation method used in
#'      \code{sits_regularize} sorts the images based on cloud cover,
#'      putting images with the least clouds at the top of the stack. Once
#'      the stack of images is sorted, the method uses the first valid value to
#'      create the temporal aggregation.
#'
#'      The "period" parameter is mandatory, and defines the time interval
#'      between two images of the regularized cube. When combining
#'      Sentinel-1A and Sentinel-1B images, experiments show that a
#'      16-day period ("P16D") are a good default. Landsat images require
#'      a longer period of one to three months.
#'
#'      By default, the date of the first image of the input cube
#'      is taken as the starting
#'      date for the regular cube. In many situations, users may want
#'      to pre-define the required times using the "timeline" parameter.
#'      The "timeline" parameter, if used, must contain a set of
#'      dates which are compatible with the input cube.
#'
#'      The optional "roi" parameter defines a region of interest. It can be
#'      an sf_object, a shapefile, or a bounding box vector with
#'      named XY values ("xmin", "xmax", "ymin", "ymax") or
#'      named lat/long values ("lat_min", "lat_max", "long_min", "long_max").
#'      \code{sits_regularize()} function will crop the images
#'      that contain the region of interest().
#'
#'      The optional \code{tiles} parameter indicates which tiles of the
#'      input cube will be used for regularization.
#'
#'      The \code{grid_system} parameter allows the user to
#'      reproject the files to a grid system which is
#'      different from that used in the ARD image collection of
#'      the could provider. Currently, the package supports
#'      the use of MGRS grid system and those used by the Brazil
#'      Data Cube ("BDC_LG_V2" "BDC_MD_V2" "BDC_SM_V2").
#'
#'
#' @return A \code{raster_cube} object with aggregated images.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # define a non-regular Sentinel-2 cube in AWS
#'     s2_cube_open <- sits_cube(
#'         source = "AWS",
#'         collection = "SENTINEL-2-L2A",
#'         tiles = c("20LKP", "20LLP"),
#'         bands = c("B8A", "CLOUD"),
#'         start_date = "2018-10-01",
#'         end_date = "2018-11-01"
#'     )
#'     # regularize the cube
#'     rg_cube <- sits_regularize(
#'         cube = s2_cube_open,
#'         period = "P16D",
#'         res = 60,
#'         multicores = 2,
#'         output_dir = tempdir()
#'     )
#'
#'     ## Sentinel-1 SAR
#'     roi <- c("lon_min" = -50.410, "lon_max" = -50.379,
#'              "lat_min" = -10.1910, "lat_max" = -10.1573)
#'     s1_cube_open <- sits_cube(
#'         source = "MPC",
#'         collection = "SENTINEL-1-GRD",
#'         bands = c("VV", "VH"),
#'         orbit = "descending",
#'         roi = roi,
#'         start_date = "2020-06-01",
#'         end_date = "2020-09-28"
#'     )
#'     # regularize the cube
#'     rg_cube <- sits_regularize(
#'         cube = s1_cube_open,
#'         period = "P12D",
#'         res = 60,
#'         roi = roi,
#'         multicores = 2,
#'         output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_regularize <- function(cube, ...) {
    .check_set_caller("sits_regularize")
    # Pre-conditions
    .check_na_null_parameter(cube)
    UseMethod("sits_regularize", cube)
}
#' @rdname sits_regularize
#' @export
sits_regularize.raster_cube <- function(cube, ...,
                                        period,
                                        res,
                                        output_dir,
                                        timeline = NULL,
                                        roi = NULL,
                                        tiles = NULL,
                                        grid_system = NULL,
                                        multicores = 2L,
                                        progress = TRUE) {
    # Preconditions
    .check_raster_cube_files(cube)
    # check period
    .check_period(period)
    # check resolution
    .check_num_parameter(res, exclusive_min = 0)
    # check output_dir
    output_dir <- .file_path_expand(output_dir)
    .check_output_dir(output_dir)
    # check for ROI and tiles
    if (!is.null(roi) || !is.null(tiles)) {
        .check_roi_tiles(roi, tiles)
    }
    # check multicores
    .check_num_parameter(multicores, min = 1, max = 2048)
    # check progress
    progress <- .message_progress(progress)
    # Does cube contain cloud band? If not, issue a warning
    .message_warnings_regularize_cloud(cube)
    if (.has(roi)) {
        crs <- NULL
        if (.roi_type(roi) == "bbox" && !.has(roi[["crs"]])) {
            crs <- .crs(cube)
            if (length(crs) > 1)
                .message_warnings_regularize_crs()
        }
        roi <- .roi_as_sf(roi, default_crs = crs[[1]])
    }
    # Convert input cube to the user's provided grid system
    if (.has(grid_system)) {
        .check_grid_system(grid_system)
        cube <- suppressWarnings(
            .reg_tile_convert(
                cube = cube,
                grid_system = grid_system,
                roi = roi,
                tiles = tiles
            )
        )
        .check_that(nrow(cube) > 0,
                    msg = .conf("messages", "sits_regularize_roi")
        )
    }
    # Display warning message in case regularization is done via STAC
    # We prefer to regularize local files
    .message_warnings_regularize_local(cube)
    # Regularize
    .gc_regularize(
        cube = cube,
        timeline = timeline,
        period = period,
        res = res,
        roi = roi,
        tiles = tiles,
        output_dir = output_dir,
        multicores = multicores,
        progress = progress
    )
}
#' @rdname sits_regularize
#' @export
sits_regularize.sar_cube <- function(cube, ...,
                                     period,
                                     res,
                                     output_dir,
                                     timeline = NULL,
                                     grid_system = "MGRS",
                                     roi = NULL,
                                     tiles = NULL,
                                     multicores = 2L,
                                     progress = TRUE) {
    # Preconditions
    .check_raster_cube_files(cube)
    .check_period(period)
    .check_num_parameter(res, exclusive_min = 0)
    output_dir <- .file_path_expand(output_dir)
    .check_output_dir(output_dir)
    .check_num_parameter(multicores, min = 1, max = 2048)
    progress <- .message_progress(progress)
    # check for ROI and tiles
    if (!is.null(roi) || !is.null(tiles)) {
        .check_roi_tiles(roi, tiles)
    } else {
        roi <- .cube_as_sf(cube)
    }
    if (.has(grid_system))
        .check_grid_system(grid_system)

    # Convert input sentinel1 cube to the user's provided grid system
    cube <- .reg_tile_convert(
        cube = cube,
        grid_system = grid_system,
        roi = roi,
        tiles = tiles
    )
    .check_that(nrow(cube) > 0,
        msg = .conf("messages", "sits_regularize_roi")
    )
    # Filter tiles
    if (is.character(tiles)) {
        cube <- .cube_filter_tiles(cube, tiles)
    }
    # Display warning message in case STAC cube
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Call regularize in parallel
    .reg_cube(
        cube = cube,
        timeline = timeline,
        res = res,
        roi = roi,
        period = period,
        output_dir = output_dir,
        progress = progress
    )
}
#' @rdname sits_regularize
#' @export
sits_regularize.combined_cube <- function(cube, ...,
                                          period,
                                          res,
                                          output_dir,
                                          grid_system = NULL,
                                          roi = NULL,
                                          tiles = NULL,
                                          multicores = 2L,
                                          progress = TRUE) {
    # Preconditions
    .check_raster_cube_files(cube)
    .check_period(period)
    .check_num_parameter(res, exclusive_min = 0)
    output_dir <- .file_path_expand(output_dir)
    .check_output_dir(output_dir)
    .check_num_parameter(multicores, min = 1, max = 2048)
    progress <- .message_progress(progress)
    # check for ROI and tiles
    .check_roi_tiles(roi, tiles)
    if (.has(grid_system)) {
        .check_grid_system(grid_system)
    } else if (any("NoTilingSystem" %in% .cube_tiles(cube))) {
            grid_system <- "MGRS"
    }
    # Get a global timeline
    timeline <- .gc_get_valid_timeline(
        cube = cube, period = period
    )
    # Grouping by unique values for each type of cube: sar, optical, etc..
    cubes <- dplyr::group_by(
        cube, .data[["source"]], .data[["collection"]], .data[["satellite"]]
    ) |> dplyr::group_map(~{
        class(.x) <- .cube_s3class(.x)
        .x
    }, .keep = TRUE)
    # Regularizing each cube
    reg_cubes <- purrr::map(cubes, function(cube) {
        sits_regularize(
            cube = cube,
            timeline = timeline,
            period = period,
            res = res,
            roi = roi,
            tiles = tiles,
            output_dir = output_dir,
            grid_system = grid_system,
            multicores = multicores,
            progress = progress
        )
    })
    # In case where more than two cubes need to be merged
    purrr::reduce(reg_cubes, sits_merge)
}
#' @rdname sits_regularize
#' @export
sits_regularize.rainfall_cube <- function(cube, ...,
                                          period,
                                          res,
                                          output_dir,
                                          timeline = NULL,
                                          grid_system = "MGRS",
                                          roi = NULL,
                                          tiles = NULL,
                                          multicores = 2L,
                                          progress = TRUE) {
    # Preconditions
    .check_raster_cube_files(cube)
    .check_period(period)
    .check_num_parameter(res, exclusive_min = 0)
    output_dir <- .file_path_expand(output_dir)
    .check_output_dir(output_dir)
    .check_num_parameter(multicores, min = 1, max = 2048)
    progress <- .message_progress(progress)
    # check for ROI and tiles
    if (!is.null(roi) || !is.null(tiles)) {
        .check_roi_tiles(roi, tiles)
    } else {
        roi <- .cube_as_sf(cube)
    }
    if (.has(grid_system)) {
        .check_grid_system(grid_system)
    }
    # Convert input sentinel1 cube to the user's provided grid system
    cube <- .reg_tile_convert(
        cube = cube,
        grid_system = grid_system,
        roi = roi,
        tiles = tiles
    )
    .check_that(nrow(cube) > 0,
                msg = .conf("messages", "sits_regularize_roi")
    )
    # Filter tiles
    if (is.character(tiles)) {
        cube <- .cube_filter_tiles(cube, tiles)
    }
    # Display warning message in case STAC cube
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Call regularize in parallel
    .reg_cube(
        cube = cube,
        timeline = timeline,
        res = res,
        roi = roi,
        period = period,
        output_dir = output_dir,
        progress = progress
    )
}
#' @rdname sits_regularize
#' @export
sits_regularize.dem_cube <- function(cube, ...,
                                     res,
                                     output_dir,
                                     grid_system = "MGRS",
                                     roi = NULL,
                                     tiles = NULL,
                                     multicores = 2L,
                                     progress = TRUE) {
    # Preconditions
    .check_raster_cube_files(cube)
    .check_num_parameter(res, exclusive_min = 0)
    output_dir <- .file_path_expand(output_dir)
    .check_output_dir(output_dir)
    .check_num_parameter(multicores, min = 1, max = 2048)
    progress <- .message_progress(progress)
    # check for ROI and tiles
    if (!is.null(roi) || !is.null(tiles)) {
        .check_roi_tiles(roi, tiles)
    } else {
        roi <- .cube_as_sf(cube)
    }
    # Convert input sentinel1 cube to the user's provided grid system
    cube <- .reg_tile_convert(
        cube = cube,
        grid_system = grid_system,
        roi = roi,
        tiles = tiles
    )
    .check_that(nrow(cube) > 0,
                msg = .conf("messages", "sits_regularize_roi")
    )
    # Filter tiles
    if (is.character(tiles)) {
        cube <- .cube_filter_tiles(cube, tiles)
    }
    # DEMs don't have the temporal dimension, so the period is fixed in 1 day.
    period <- "P1D"

    # Display warning message in case STAC cube
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Call regularize in parallel
    .reg_cube(
        cube = cube,
        timeline = NULL,
        res = res,
        roi = roi,
        period = period,
        output_dir = output_dir,
        progress = progress
    )
}
#' @rdname sits_regularize
#' @export
sits_regularize.derived_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_regularize_default"))
}
#' @rdname sits_regularize
#' @export
sits_regularize.default <- function(cube, ...) {
    stop(.conf("messages", "sits_regularize_default"))
}
