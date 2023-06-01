#' @title Build a regular data cube from an irregular one
#'
#' @name sits_regularize
#'
#' @description Produces regular data cubes for analysis-ready data (ARD)
#' image collections. Analysis-ready data (ARD) collections available in
#' AWS, MPC, USGS and DEAfrica are not regular in space and time.
#' Bands may have different resolutions,
#' images may not cover the entire time, and time intervals are not regular.
#' For this reason, subsets of these collection need to be converted to
#' regular data cubes before further processing and data analysis.
#'
#' This function requires users to include the cloud band in their ARD-based
#' data cubes.
#'
#' @references Appel, Marius; Pebesma, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092.
#'
#' @param cube       \code{raster_cube} object whose observation
#'                   period and/or spatial resolution is not constant.
#' @param period     ISO8601-compliant time period for regular
#'                   data cubes, with number and unit, where
#'                   "D", "M" and "Y" stand for days, month and year;
#'                    e.g., "P16D" for 16 days.
#' @param res        Spatial resolution of regularized images (in meters).
#' @param roi        A named \code{numeric} vector with a region of interest.
#'                   See more above.
#' @param multicores Number of cores used for regularization;
#'                   used for parallel processing of input.
#' @param output_dir Valid directory for storing regularized images.
#' @param progress   show progress bar?
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lat_min", "lat_max", "long_min", "long_max").
#'    The \code{sits_regularize} function will crop the images that contain the
#'    roi region.
#' @note
#'       The aggregation method used in \code{sits_regularize}
#'       sorts the images based on cloud cover, where images with the fewest
#'       clouds at the top of the stack. Once
#'       the stack of images is sorted, the method uses the first valid value to
#'       create the temporal aggregation.
#' @note
#'       The input (non-regular) ARD cube needs to include the cloud band for
#'       the regularization to work.
#'
#' @return A \code{raster_cube} object with aggregated images.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # define a non-regular Sentinel-2 cube in AWS
#'     s2_cube_open <- sits_cube(
#'         source = "AWS",
#'         collection = "SENTINEL-S2-L2A-COGS",
#'         tiles = c("20LKP", "20LLP"),
#'         bands = c("B8A", "SCL"),
#'         start_date = "2018-10-01",
#'         end_date = "2018-11-01"
#'     )
#'     # regularize the cube
#'     rg_cube <- sits_regularize(
#'         cube = s2_cube_open,
#'         output_dir = tempdir(),
#'         res = 60,
#'         period = "P16D",
#'         multicores = 2
#'     )
#' }
#'
#' @export
sits_regularize <- function(cube,
                            period,
                            res,
                            output_dir,
                            roi = NULL,
                            multicores = 2,
                            progress = TRUE) {

    # Pre-conditions
    .check_is_raster_cube(cube)
    # Does cube contain cloud band?
    if (!all(.cube_contains_cloud(cube))) {
        if (.check_warnings())
            warning("Cloud band not found in provided cube.
                    'sits_regularize()' ",
                    "will just fill nodata values.",
                    call. = FALSE,
                    immediate. = TRUE
            )
    }
    .period_check(period)
    .check_num_parameter(res, exclusive_min = 0)
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
    }
    # Normalize path
    output_dir <- .file_normalize(output_dir)
    .check_output_dir(output_dir)
    .check_multicores(multicores)
    .check_progress(progress)
    # Display warning message in case STAC cube
    if (!.cube_is_local(cube)) {
        if (.check_warnings()) {
            warning("Regularization works better when data store locally. ",
                    "Please, use 'sits_cube_copy()' to copy data locally ",
                    "before regularization", call. = FALSE, immediate. = TRUE)
        }
    }
    # Regularize
    .gc_regularize(
        cube = cube,
        period = period,
        res = res,
        roi = roi,
        output_dir = output_dir,
        multicores = multicores,
        progress = progress
    )
}
