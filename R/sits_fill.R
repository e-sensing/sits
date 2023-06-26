sits_fill <- function(cube,
                      window_size,
                      memsize = 4,
                      multicores = 2,
                      output_dir,
                      version = "v1",
                      progress = TRUE) {
    # Check cube
    .check_cube_is_probs_cube(cube)
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
    # Check progress
    .check_progress(progress)

    # Get input band
    band <- .cube_bands(cube)

    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = 1, nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Create assets as jobs
    assets_cube <- .cube_split_assets(cube)

    # Process each feature in parallel
    assets_band <- .jobs_map_parallel_dfr(assets_cube, function(asset) {
        # Process the data
        output_asset <- .fill_asset(
            asset = asset,
            block = block,
            band = band,
            window_size = window_size,
            overlap = overlap,
            output_dir = output_dir,
            version = version
        )
        return(output_asset)
    }, progress = progress)
    # Join output assets and return it
    .cube_merge_tiles(assets_band)
}

.fill_asset <- function(asset,
                        block,
                        band,
                        window_size,
                        overlap,
                        output_dir,
                        version) {
    # Output file
    out_file <- .file_derived_name(
        tile = asset, band = band,
        version = version, output_dir = output_dir
    )
    labels <- .tile_labels(asset)
    # Resume asset
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery(out_file)
        # Create tile based on template
        asset <- .tile_derived_from_file(
            file = out_file, band = band,
            base_tile = asset, derived_class = .tile_derived_class(asset),
            labels = labels,
            update_bbox = FALSE
        )
        return(asset)
    }
    # Remove remaining incomplete files
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
        values <- .fill_data_read(
            tile = asset, block = block, band = band, labels = labels
        )
        if (any(is.na(values))) {
            values <- .fill_values(values, labels)
        }
        # Prepare fractions to be saved
        band_conf <- .tile_band_conf(tile = asset, band = band)
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
        band = band,
        labels = labels,
        base_tile = asset,
        derived_class = "probs_cube",
        block_files = block_files,
        multicores = 1,
        update_bbox = FALSE
    )
    # Return a asset
    band_tile
}

.fill_values <- function(values, labels) {
    na_idxs <- which(is.na(values), arr.ind = TRUE)[, "row"]
    rand_values <- runif(length(labels), min = 0, max = 1)
    rand_values <- rand_values / sum(rand_values)
    values[na_idxs, ] <- rand_values
    return(values)
}

.fill_data_read <- function(tile, block, band, labels) {
    # Get band values
    values <- .tile_read_block(tile = tile, band = band, block = block)
    values <- as.data.frame(values) * 10000
    # Set columns name
    colnames(values) <- labels
    # Return values
    values
}
