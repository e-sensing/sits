#' @title Copy the images of a cube to a local directory
#' @name sits_cube_copy
#' @description
#'
#' This function downloads the images of a cube in parallel.
#' A region of interest (\code{roi}) can be provided to crop
#' the images and a resolution (\code{res}) to resample the
#' bands.
#'
#' @param cube       A data cube (class "raster_cube")
#' @param roi        Region of interest.
#'                   Either an sf_object, a shapefile,
#'                   or a bounding box vector with
#'                   named XY values ("xmin", "xmax", "ymin", "ymax") or
#'                   named lat/long values
#'                   ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param res        An integer value corresponds to the output
#'                   spatial resolution of the images. Default is NULL.
#' @param crs        Reference system for output cube (by default,
#'                   the same CRS from the input cube is assumed)
#' @param n_tries    Number of attempts to download the same image.
#'                   Default is 3.
#' @param multicores Number of cores for parallel downloading
#'                   (integer, min = 1, max = 2048).
#' @param output_dir Output directory where images will be saved.
#'                   (character vector of length 1).
#' @param progress   Logical: show progress bar?
#' @return Copy of input data cube (class "raster cube").
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Creating a sits cube from BDC
#'     bdc_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "CBERS-WFI-16D",
#'         tiles = c("007004", "007005"),
#'         bands = c("B15", "CLOUD"),
#'         start_date = "2018-01-01",
#'         end_date = "2018-01-12"
#'     )
#'     # Downloading images to a temporary directory
#'     cube_local <- sits_cube_copy(
#'         cube = bdc_cube,
#'         output_dir = tempdir(),
#'         roi = c(
#'             lon_min = -46.5,
#'             lat_min = -45.5,
#'             lon_max = -15.5,
#'             lat_max = -14.6
#'         ),
#'         multicores = 2L,
#'         res = 250,
#'     )
#' }
#'
#' @export
sits_cube_copy <- function(cube,
                           roi = NULL,
                           res = NULL,
                           crs = NULL,
                           n_tries = 3,
                           multicores = 2L,
                           output_dir,
                           progress = TRUE) {
    # Set caller for error msgs
    .check_set_caller("sits_cube_copy")
    # Pre-conditions
    .check_is_raster_cube(cube)
    # Check n_tries parameter
    .check_num_min_max(x = n_tries, min = 1, max = 50)
    # Check files
    .check_raster_cube_files(cube)
    # Spatial filter
    if (.has(roi)) {
        # if crs is not NULL, use user parameter as default
        # else use input cube crs
        roi <- .roi_as_sf(
            roi,
            default_crs = ifelse(.has(crs), crs, .cube_crs(cube))
        )
        cube <- .cube_filter_spatial(cube = cube, roi = roi)

        if (!.cube_has_unique_resolution(cube)) {
            .check_that(
                .has(res),
                msg = .conf("messages", "sits_cube_copy_different_resolutions")
            )
        }
    }
    .check_int_parameter(multicores, min = 1, max = 2048)
    # Check Output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check progress
    .check_progress(progress)
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Adjust tile system name
    cube <- .cube_convert_tile_name(cube)
    # Update token (for big tiffs and slow networks)
    cube <- .cube_token_generator(cube)
    # Create assets as jobs
    cube_assets <- .cube_split_assets(cube)
    # Process each tile sequentially
    cube_assets <- .jobs_map_parallel_dfr(cube_assets, function(asset) {
        .download_asset(
            asset = asset,
            roi = roi,
            res = res,
            n_tries = n_tries,
            output_dir = output_dir
        )
    }, progress = progress)
    # Check and return
    .check_empty_data_frame(cube_assets)
    cube_assets <- .cube_merge_tiles(cube_assets)
    # Revert tile system name
    .cube_revert_tile_name(cube_assets)
}
