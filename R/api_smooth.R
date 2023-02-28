#---- internal functions ----

.smooth_tile <- function(tile, band, block, overlap, smooth_fn, output_dir,
                         version) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
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
            file = out_file, band = band, base_tile = tile,
            labels = .tile_labels(tile), update_bbox = FALSE
        )
        return(probs_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = overlap, block = block)
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
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Apply the probability function to values
        values <- smooth_fn(values = values, block = block)
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube", band = band
        )
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
        # Return block file
        block_file
    })
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_probs_merge_blocks(
        file = out_file, band = band, labels = .tile_labels(tile),
        base_tile = tile, block_files = block_files,
        multicores = .jobs_multicores(), update_bbox = FALSE
    )
    # Return probs tile
    probs_tile
}

.smooth_use_method <- function(cube, type, block, window_size, memsize,
                               multicores, output_dir, version, ...) {
    smooth_fn <- switch(
        type,
        bayes = .smooth_bayes,
        bilateral = .smooth_bilateral,
        stop("Invalid `type` parameter (value should be one of 'bayes', ",
             "'bilateral').")
    )
    smooth_fn(cube = cube,
              block = block,
              window_size = window_size,
              memsize = memsize,
              multicores = multicores,
              output_dir = output_dir,
              version = version, ...)
}

#---- smooth methods ----

.smooth_bayes <- function(cube, block, ...,
                          window_size = 9,
                          neigh_fraction = 0.5,
                          smoothness = 20,
                          covar = FALSE,
                          multicores = 2,
                          memsize = 4,
                          output_dir = getwd(),
                          version = "v1") {
    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .smooth_fn_bayes(
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        smoothness = smoothness,
        covar = covar,
        nlabels = length(.tile_labels(cube))
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Smoothing
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # Smooth the data
        .smooth_tile(
            tile = tile,
            band = "bayes",
            block = block,
            overlap = overlap,
            smooth_fn = smooth_fn,
            output_dir = output_dir,
            version = version
        )
    })
}

.smooth_bilateral <- function(cube, block, ...,
                              window_size = 5,
                              sigma = 8,
                              tau = 0.1,
                              multicores = 2,
                              memsize = 4,
                              output_dir = getwd(),
                              version = "v1") {
    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .smooth_fn_bilat(
        window_size = window_size, sigma = sigma, tau = tau
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Smoothing
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # Smooth the data
        .smooth_tile(
            tile = tile,
            band = "bilat",
            block = block,
            overlap = overlap,
            smooth_fn = smooth_fn,
            output_dir = output_dir,
            version = version
        )
    })
}

#---- smooth functions ----

.smooth_fn_bayes <- function(window_size,
                             neigh_fraction,
                             smoothness,
                             covar,
                             nlabels) {
    # Check window size
    .check_window_size(window_size, min = 7)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, min = 0, max = 1)
    # check number of values
    num_values <- window_size * window_size * neigh_fraction
    .check_num(num_values, min = 30,
               msg = paste0("Sample size too small \n",
                            "Please choose a larger window\n",
                            "or increase the neighborhood fraction")
    )
    # Check covar
    .check_lgl_type(covar)
    # Prepare smoothness parameter
    if (!is.matrix(smoothness)) {
        smoothness <- diag(smoothness, nrow = nlabels, ncol = nlabels)
    }
    # Check smoothness
    .check_smoothness_mat(smoothness, nlabels)
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
    # Check window size
    .check_window_size(window_size)
    # Check variance
    .check_num_parameter(sigma, exclusive_min = 0)
    # Check tau
    .check_num_parameter(tau, exclusive_min = 0)
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
            m = values, m_nrow = .nrows(block), m_ncol = .ncols(block),
            w = window, tau = tau
        )
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
