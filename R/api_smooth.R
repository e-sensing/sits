#---- internal functions ----
.smooth <- function(cube,
                    smooth_fn,
                    band,
                    window_size,
                    memsize,
                    multicores,
                    output_dir,
                    version,
                    progress, ...) {

    # Check memory and multicores
    # Get block size
    block_size <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block_size, overlap = overlap),
        npaths = length(.tile_labels(cube)) * 2,
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

    # Call the smooth method
    # Process each tile sequentially
    probs_cube <- .cube_foreach_tile(cube, function(tile) {
        probs_tile <- .smooth_tile(
            tile = tile,
            band = band,
            smooth_fn = smooth_fn,
            block_size = block_size,
            overlap = overlap,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
        probs_tile
    })
    probs_cube
}

.smooth_tile <- function(tile,
                         band,
                         smooth_fn,
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
        probs_tile <- .tile_probs_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .tile_labels(tile),
            update_bbox = FALSE
        )
        return(probs_tile)
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
        # Apply the probability function to values
        values <- smooth_fn(values = values, block = block)
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube",
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
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_probs_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Return probs tile
    probs_tile
}

#---- smooth functions ----

.smooth_fn_bayes <- function(window_size,
                             neigh_fraction,
                             smoothness,
                             covar,
                             nlabels) {
    # check number of values
    num_values <- window_size * window_size * neigh_fraction
    .check_num(num_values, min = 30,
               msg = paste0("Sample size too small \n",
                            "Please choose a larger window\n",
                            "or increase the neighborhood fraction")
    )
    # Create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)

    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Compute logit
        values <- log(values / (rowSums(values) - values))
        # Process Bayesian
        values <- bayes_smoother(
            m = values,
            m_nrow = .nrows(block),
            m_ncol = .ncols(block),
            w = window,
            sigma = smoothness,
            covar_sigma0 = covar,
            neigh_fraction = neigh_fraction
        )
        # Compute inverse logit
        values <- exp(values) / (exp(values) + 1)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}

.smooth_fn_bilat <- function(window_size, sigma, tau) {
    # Create a Gaussian window
    center <- ceiling(window_size / 2)
    seq <- seq_len(window_size)
    seq_h <- rep(seq, each = window_size)
    seq_v <- rep(seq, window_size)
    x <- stats::dnorm(
        x = sqrt((seq_h - center)^2 + (seq_v - center)^2), sd = sigma
    ) / stats::dnorm(0)
    # Normalize and convert to matrix
    window <- matrix(x / sum(x), nrow = window_size, byrow = TRUE)

    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Process bilateral smoother and return
        values <- bilateral_smoother(
            m = values,
            m_nrow = .nrows(block),
            m_ncol = .ncols(block),
            w = window,
            tau = tau
        )
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
