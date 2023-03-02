#---- internal functions ----
.uncert <- function() {

}

.uncert_tile <- function(tile,
                              band,
                              overlap,
                              uncert_fn,
                              output_dir,
                              version,
                              progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
        message("Recovery: tile '", tile[["tile"]], "' already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)")
        uncert_tile <- .tile_uncertainty_from_file(
            file = out_file,
            band = band,
            base_tile = tile
        )
        return(uncert_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = overlap)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile,
            band = .tile_bands(tile),
            block = block
        )
        # Apply the labeling function to values
        values <- uncert_fn(values = values, block = block)
        # Prepare uncertainty to be saved
        band_conf <- .conf_derived_band(
            derived_class = "uncertainty_cube",
            band = band
        )
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            values <- values / scale
            values[values > 10000] <- 10000
        }
        # Job crop block
        crop_block <- .block(.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file,
            block = block,
            bbox = .bbox(chunk),
            values = values,
            data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Return block file
        block_file
    }, progress = progress)
    # Merge blocks into a new uncertainty_cube tile
    uncert_tile <- .tile_uncertainty_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        multicores = .jobs_multicores()
    )
    # Return uncertainty tile
    uncert_tile
}

#---- uncertainty functions ----

.uncert_fn_least <- function(window_size) {
    # Check window size
    .check_window_size(window_size)

    # Define uncertainty function
    uncert_fn <- function(values, block) {
        # Used in check (below)
        input_pixels <- nrow(values)
        # Process least confidence
        # return a matrix[rows(values),1]
        values <- C_least_probs(values)
        # Process window
        if (window_size > 1) {
            values <- C_kernel_median(
                x = values,
                ncols = .ncols(block),
                nrows = .nrows(block),
                band = 0,
                window_size = window_size
            )
        }
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return data
        values
    }
    # Return closure
    uncert_fn
}

.uncert_fn_entropy <- function(window_size) {
    # Check window size
    .check_window_size(window_size)

    # Define uncertainty function
    uncert_fn <- function(values, block) {
        # Used in check (below)
        input_pixels <- nrow(values)
        # Process least confidence
        values <- C_entropy_probs(values) # return a matrix[rows(values),1]
        # Process window
        if (window_size > 1) {
            values <- C_kernel_median(
                x = values, ncols = .ncols(block), nrows = .nrows(block),
                band = 0, window_size = window_size
            )
        }
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return data
        values
    }
    # Return closure
    uncert_fn
}

.uncert_fn_margin <- function(window_size) {
    # Check window size
    .check_window_size(window_size)

    # Define uncertainty function
    uncert_fn <- function(values, block) {
        # Pocess least confidence
        values <- C_margin_probs(values) # return a matrix[rows(data),1]
        # Process window
        if (window_size > 1) {
            values <- C_kernel_median(
                x = values, ncols = .ncols(block), nrows = .nrows(block),
                band = 0, window_size = window_size
            )
        }
        # Return data
        values
    }
    # Return closure
    uncert_fn
}
