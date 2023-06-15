sits_clean <- function(cube,
                       clean_fn,
                       window_size,
                       memsize,
                       multicores,
                       output_dir,
                       version,
                       progress) {
    # Check cube
    .check_cube_is_class_cube(cube)
    # Check window size
    .check_window_size(window_size)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output_dir
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)
    # Check fn function

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = 1,
        nbytes = 8, proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Create features as jobs
    assets_cube <- .cube_split_assets(cube)

    # Process each feature in parallel
    features_band <- .jobs_map_parallel_dfr(assets_cube, function(asset) {
        # Process the data
        output_asset <- .clean_asset(
            asset = asset,
            block = block,
            clean_fn = clean_fn,
            window_size = window_size,
            overlap = overlap,
            output_dir = output_dir
        )
        return(output_asset)
    }, progress = progress)
    # Join output features as a cube and return it
    .cube_merge_tiles(dplyr::bind_rows(list(assets_cube, features_band)))
}

.clean_asset <- function(asset,
                         block,
                         clean_fn,
                         window_size,
                         overlap,
                         output_dir) {

}
