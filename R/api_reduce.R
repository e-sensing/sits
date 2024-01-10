.reduce_tile <- function(tile,
                         block,
                         expr,
                         window_size,
                         out_band,
                         in_bands,
                         overlap,
                         output_dir,
                         progress) {

    # Output file
    out_file <- .file_eo_name(
        tile = tile, band = out_band,
        date = .tile_start_date(tile),
        output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        .check_recovery(out_file)

        # Create tile based on template
        tile <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(tile)),
            bands = out_band, date = .tile_start_date(tile),
            base_tile = tile, update_bbox = FALSE
        )
        return(tile)
    }
    # Remove remaining incomplete fractions files
    unlink(out_file)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile, overlap = overlap, block = block
    )
    # Filter tile bands
    if (.band_cloud() %in% .tile_bands(tile)) {
        in_bands <- c(in_bands, .band_cloud())
    }
    tile <- .tile_filter_bands(tile, in_bands)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .classify_data_read(
            tile = tile,
            block = block,
            bands = in_bands,
            ml_model = NULL,
            filter_fn = NULL
        )
        # Convert to named list
        values <- list(values)
        names(values) <- in_bands
        # Evaluate expression here
        # Band and kernel evaluation
        values <- eval(
            expr = expr[[out_band]],
            envir = values,
            enclos = .kern_functions(
                window_size = window_size,
                img_nrow = block[["nrows"]],
                img_ncol = block[["ncols"]]
            )
        )
        # Prepare fractions to be saved
        band_conf <- .tile_band_conf(tile = tile, band = out_band)
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            values <- values / scale
        }
        # Job crop block
        crop_block <- .block(.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_file

    }, progress = progress)
    # Merge blocks into a new eo_cube tile
    band_tile <- .tile_eo_merge_blocks(
        files = out_file,
        bands = out_band,
        base_tile = tile,
        block_files = block_files,
        multicores = 1,
        update_bbox = FALSE
    )
    # Return a reduced tile
    band_tile
}



