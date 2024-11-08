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
#' @param n_tries    Number of attempts to download the same image.
#'                   Default is 3.
#' @param multicores Number of cores for parallel downloading
#'                   (integer, min = 1, max = 2048).
#' @param output_dir Output directory where images will be saved.
#'                   (character vector of length 1).
#' @param progress   Logical: show progress bar?
#' @param ...        Additional parameters to httr package.
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
sits_cube_copy <- function(cube, ...,
                           roi = NULL,
                           res = NULL,
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
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
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
    cube <- .cube_adjust_tile_name(cube)

    # Create assets as jobs
    cube_assets <- .cube_split_assets(cube)
    # Process each tile sequentially
    cube_assets <- .jobs_map_parallel_dfr(cube_assets, function(asset) {
        # Get asset path and expand it
        file <- .file_path_expand(.tile_path(asset))
        # Create output file
        asset[["sensor"]] <- .download_remove_slash(.tile_sensor(asset))
        output_file <- .file_eo_name(
            tile = asset,
            band = .tile_bands(asset),
            date = .tile_start_date(asset),
            output_dir = output_dir
        )

        while (n_tries > 0) {
            # update token for mpc cubes
            # in case of big tiffs and slow networks
            asset <- .cube_token_generator(asset)
            # download asset
            local_asset <- .try(
                expr = .crop_asset(
                    asset = asset,
                    roi = roi,
                    output_dir = output_dir,
                    gdal_params = list("-tr" = c(res, res))),
                default = NULL
            )
            if (.has(local_asset) &&
                .raster_is_valid(.cube_paths(local_asset))) {
                return(local_asset)
            }
            n_tries <- n_tries - 1

            secs_to_retry <- .conf("cube_token_generator_sleep_time")
            secs_to_retry <- sample(x = seq_len(secs_to_retry), size = 1)
            Sys.sleep(secs_to_retry)
            message(paste("Trying to download image in X seconds", file))
        }
        # Return local asset
        local_asset
    }, progress = progress)
    .check_empty_data_frame(cube_assets)
    .cube_merge_tiles(cube_assets)
}
