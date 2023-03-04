#---- internal functions ----
.uncert <- function(cube,
                    uncert_fn,
                    band,
                    window_size,
                    memsize,
                    multicores,
                    output_dir,
                    version,
                    progress) {

    # Check memory and multicores
    # Get block size
    block_size <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block_size, overlap = overlap),
        npaths = length(.cube_labels(cube)) + 1,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block_size <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block_size,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Call the uncertainty method
    # Process each tile sequentially
    uncert_cube <- .cube_foreach_tile(cube, function(tile) {
        uncert_tile <- .uncert_tile(
            tile = tile,
            band = band,
            uncert_fn = uncert_fn,
            block_size = block_size,
            overlap = overlap,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
        uncert_tile
    })
    uncert_cube
}

.uncert_tile <- function(tile,
                         band,
                         uncert_fn,
                         block_size,
                         overlap,
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
        uncert_tile <- .tile_uncert_from_file(
            file = out_file,
            band = band,
            base_tile = tile
        )
        return(uncert_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile,
        overlap = overlap,
        block = block_size
    )
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
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
        }
        min <- .min_value(band_conf)
        if (.has(max)) {
            values[values < min] <- min
        }
        max <- .max_value(band_conf)
        if (.has(max)) {
            values[values > max] <- max
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
            values <- matrix(values, ncol = 1)
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
            values <- matrix(values, ncol = 1)
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
            values <- matrix(values, ncol = 1)
        }
        # Return data
        values
    }
    # Return closure
    uncert_fn
}
