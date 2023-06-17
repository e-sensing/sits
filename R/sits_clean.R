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
            output_dir = output_dir,
            version = version
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
                         output_dir,
                         version) {
    # Output file
    out_file <- .file_derived_name(
        tile = asset, band = out_band,
        version = version, output_dir = output_dir
    )
    # Resume asset
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery(out_file)

        # Create tile based on template
        # TODO: check this
        asset <- .tile_derived_from_file(
            file = out_file, band = out_band,
            base_tile = asset, update_bbox = FALSE
        )
        return(asset)
    }
    # Remove remaining incomplete fractions files
    unlink(out_file)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = asset, overlap = overlap, block = block
    )
    # Process jobs sequentially
    block_files <- .jobs_map_sequential(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Block file name for each fraction
        block_files <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- .clean_data_read(
            tile = asset, block = block
        )
        # Evaluate expression here
        # Band and kernel evaluation
        values <- clean_fn(values)
        # Prepare fractions to be saved
        band_conf <- .tile_band_conf(tile = asset, band = out_band)
        # Job crop block
        crop_block <- .block(.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        .raster_write_block(
            files = block_files, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_files
    })
    # Merge blocks into a new class_cube tile
    band_tile <- .tile_derived_merge_blocks(
        file = out_file,
        bands = out_band,
        labels = labels,
        base_tile = asset,
        derived_class = .tile_derived_class(asset),
        block_files = block_files,
        multicores = 1,
        update_bbox = FALSE
    )
    # Return a asset
    band_tile
}

.clean_data_read <- function(tile, block) {
    # Get band values
    band <- .tile_bands(tile)
    values <- .tile_read_block(tile = tile, band = band, block = block)
    values <- as.data.frame(values)
    # Set columns name
    colnames(values) <- band
    # Return values
    values
}

