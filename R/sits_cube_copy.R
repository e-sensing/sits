#'
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
#' @param n_tries    Number of tries to download the same image. Default is 3.
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
    # Cupe copy does not work for SAR data
    if (inherits(cube, "sar_cube") && !.cube_is_regular(cube)) {
        warning(.conf("messages"), "sits_cube_copy_sar_no_copy")
        return(cube)
    }
    # Check n_tries parameter
    .check_num_min_max(x = n_tries, min = 1, max = 50)
    # check files
    .check_raster_cube_files(cube)
    if (.has(roi)) {
        sf_roi <- .roi_as_sf(roi, default_crs = cube[["crs"]][[1]])
    } else {
        sf_roi <- NULL
    }
    if (inherits(output_dir, "character")) {
        output_dir <- path.expand(output_dir)
    }
    .check_output_dir(output_dir)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_progress(progress)

    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Create assets as jobs
    cube_assets <- .cube_split_assets(cube)
    # Process each tile sequentially
    cube_assets <- .jobs_map_parallel_dfr(cube_assets, function(asset) {
        # if there is a ROI which does not intersect asset, do nothing
        if (.has(roi)) {
            sf_asset <- .bbox_as_sf(.tile_bbox(asset))
            if (sf::st_crs(sf_asset) != sf::st_crs(sf_roi)) {
                sf_roi <- sf::st_transform(sf_roi, crs = .tile_crs(asset))
            }
            g1 <- sf::st_intersects(sf_asset, sf_roi, sparse = TRUE)
            if (lengths(g1) == 0) {
                return(NULL)
            }
        }
        # download asset
        local_asset <- .download_asset(
            asset = asset,
            res = res,
            sf_roi = sf_roi,
            n_tries = n_tries,
            output_dir = output_dir,
            progress = progress, ...
        )
        # Return local tile
        local_asset
    }, progress = progress)
    .check_empty_data_frame(cube_assets)
    .cube_merge_tiles(cube_assets)
}
