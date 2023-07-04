.clean_asset <- function(asset,
                         block,
                         band,
                         window_size,
                         overlap,
                         output_dir,
                         version) {
    # Output file
    out_file <- .file_clean_name(
        tile = asset, band = band,
        version = version, output_dir = output_dir
    )
    # Resume asset
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery(out_file)
        # Create tile based on template
        asset <- .tile_derived_from_file(
            file = out_file, band = band,
            base_tile = asset, derived_class = .tile_derived_class(asset),
            labels = .tile_labels(asset),
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
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
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
            tile = asset, block = block, band = band
        )
        # Apply kernel modal
        values <- C_kernel_modal(
            x = as.matrix(values),
            ncols = block[["ncols"]],
            nrows = block[["nrows"]],
            band = 0,
            window_size = window_size
        )
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
        labels = .tile_labels(asset),
        base_tile = asset,
        derived_class = .tile_derived_class(asset),
        block_files = block_files,
        multicores = 1,
        update_bbox = FALSE
    )
    # Return a asset
    band_tile
}

.clean_data_read <- function(tile, block, band) {
    # Get band values
    values <- .tile_read_block(tile = tile, band = band, block = block)
    values <- as.data.frame(values)
    # Set columns name
    colnames(values) <- band
    # Return values
    values
}