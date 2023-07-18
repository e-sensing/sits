sits_segment <- function(cube,
                         seg_fn = sits_supercells_temp(),
                         roi = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         memsize = 8,
                         multicores = 2,
                         output_dir,
                         version = "v1",
                         progress = TRUE) {
    # Preconditions
    .check_is_raster_cube(cube)
    .check_is_regular(cube)
    .check_memsize(memsize)
    .check_output_dir(output_dir)
    .check_version(version)
    .check_progress(progress)

    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    # Temporal filter
    if (.has(start_date) || .has(end_date)) {
        cube <- .cube_filter_interval(
            cube = cube, start_date = start_date, end_date = end_date
        )
    }
    start_date <- .default(start_date, .cube_start_date(cube))
    end_date <- .default(end_date, .cube_end_date(cube))

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(cube)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_seg")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize, block = block,
        image_size = .tile_size(.tile(cube)), memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores, output_dir = output_dir)
    on.exit(.parallel_stop(), add = TRUE)
    # Segmentation
    # Process each tile sequentially
    segs_cube <- .cube_foreach_tile(cube, function(tile) {
        # Segment the data
        segs_tile <- .segment_tile(
            tile = tile,
            seg_fn = seg_fn,
            band = "segment",
            block = block,
            roi = roi,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
        return(segs_tile)
    })
    return(segs_cube)
}
