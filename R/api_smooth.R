
#---- internal functions ----

.smooth_tile <- function(tile, band, overlap, smooth_fn, output_dir,
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

#' @rdname sits_smooth
#' @export
sits_smooth_variance <- function(cube,
                                 window_size = 5,
                                 multicores = 2,
                                 memsize = 4,
                                 output_dir = getwd(),
                                 version = "v1") {

    # Check if cube has probability data
    .check_is_probs_cube(cube)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        # npaths = input(nlayers) + output(nlayers)
        npaths = length(.tile_labels(cube)) * 2,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .smooth_fn_variance(window_size = window_size)
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Smoothing
    # Process each tile sequentially
    variance_cube <- .cube_foreach_tile(cube, function(tile) {
        # Smooth the data
        variance_tile <- .smooth_tile(
            tile = tile,
            band = "variance",
            overlap = overlap,
            smooth_fn = smooth_fn,
            output_dir = output_dir,
            version = version
        )
        return(variance_tile)
    })
    return(variance_cube)
}
.smooth_fn_variance <- function(window_size) {
    # Check window size
    .check_window_size(window_size)
    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Compute logit
        values <- log(values / (rowSums(values) - values))
        # Process Bayesian
        for (i in seq_len(ncol(values))) {
            values[,i] <- C_kernel_var(
                x = values,
                ncols = .ncols(block),
                nrows = .nrows(block),
                band = i - 1,
                window_size = window_size)
        }
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
