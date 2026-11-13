#' @title Smooth one block of a tile
#' @name .smooth_block
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  chunk       Chunk of the block, with its overlap
#' @param  tile        Tile of the block
#' @param  band        Band to be processed
#' @param  out_file    File of the smoothed tile
#' @param  output_dir  Directory where image will be save
#' @param  smooth_fn   Smoothing function
#' @return             File of the smoothed block
.smooth_block <- function(chunk, tile, band, out_file, output_dir, smooth_fn) {
    # Job block
    block <- .block(chunk)
    # Block file name
    block_file <- .file_block_name(
        pattern = .file_pattern(out_file),
        block = block,
        output_dir = output_dir
    )
    .debug_log(event = "start_block", key = "file", value = block_file)
    # Resume processing in case of failure
    if (all(.raster_is_valid(block_file))) {
        .debug_log(event = "end_block", key = "file", value = block_file)
        return(block_file)
    }
    # Read and preprocess values
    values <- .tile_read_block(
        tile = tile, band = .tile_bands(tile), block = block
    )
    # Apply the probability function to values
    .debug_log(event = "start_block_kernel", key = "file", value = block_file)
    values <- smooth_fn(values = values, block = block)
    .debug_log(event = "end_block_kernel", key = "file", value = block_file)
    # Prepare probability to be saved
    band_conf <- .conf_derived_band(
        derived_class = "probs_cube", band = band
    )
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0.0) {
        values <- values - offset
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1.0) {
        values <- values / scale
    }
    # Job crop block
    crop_block <- .block(.chunks_no_overlap(chunk))
    # Prepare and save results as raster
    .debug_log(event = "start_block_write", key = "file", value = block_file)
    .raster_write_block(
        files = block_file, block = block, bbox = .bbox(chunk),
        values = values, data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf),
        crop_block = crop_block
    )
    .debug_log(event = "end_block_write", key = "file", value = block_file)
    # Free memory
    .debug_log(event = "start_block_gc", key = "file", value = block_file)
    gc()
    .debug_log(event = "end_block_gc", key = "file", value = block_file)
    .debug_log(event = "end_block", key = "file", value = block_file)
    # Return block file
    block_file
}
#' @title Merge the smoothed blocks of a tile
#' @name .smooth_merge
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  tile            Tile of the blocks
#' @param  band            Band to be processed
#' @param  out_file        File of the smoothed tile
#' @param  block_files     Files of the smoothed blocks
#' @param  exclusion_mask  Exclusion mask cropped to the chunks, or NULL
#' @param  multicores      Threads of the merge
#' @param  output_dir      Directory where image will be save
#' @param  version         Version of result
#' @return                 Smoothed tile
.smooth_merge <- function(tile, band, out_file, block_files, exclusion_mask,
                          multicores, output_dir, version) {
    # Check if there is a exclusion_mask
    # If exclusion_mask exists, blocks are merged to a different directory
    # than output_dir, which is used to save the final cropped version
    merge_out_file <- out_file
    if (.has(exclusion_mask)) {
        merge_out_file <- .file_derived_name(
            tile = tile,
            band = band,
            version = version,
            output_dir = file.path(output_dir, ".sits")
        )
    }
    # Merge blocks into a new probs_cube tile
    .debug_log(event = "start_tile_merge", key = "file", value = out_file)
    probs_tile <- .tile_derived_merge_blocks(
        file = merge_out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = multicores,
        update_bbox = FALSE
    )
    .debug_log(event = "end_tile_merge", key = "file", value = out_file)
    # Exclude masked areas
    if (.has(exclusion_mask)) {
        # .crop() starts no cluster here, but resets the debug flag
        log_flag <- .debug()
        log_dir <- sits_env[["output_dir"]]
        probs_tile_crop <- .crop(
            cube = probs_tile,
            roi = exclusion_mask,
            output_dir = output_dir,
            multicores = 1L,
            overwrite = TRUE,
            progress = FALSE
        )
        .debug(flag = log_flag, output_dir = log_dir)
        # delete old files
        unlink(.fi_paths(.fi(probs_tile)))
        # assign new cropped value in the old probs variable
        probs_tile <- probs_tile_crop
    }
    # Return probs tile
    probs_tile
}
#' @title Job of the smoothing: one block, the merge or the resume of a tile
#' @name .smooth_job
#' @keywords internal
#' @noRd
#' @param  job        Row of the jobs of \code{.smooth_tile_jobs()}
#' @param  smooth_fn  Smoothing function
#' @return File of the smoothed block, or the smoothed tile
.smooth_job <- function(job, smooth_fn) {
    args <- job[["args"]][[1L]]
    switch(job[["role"]],
        block = .smooth_block(
            chunk = args[["chunk"]], tile = args[["tile"]],
            band = args[["band"]], out_file = job[["out_file"]],
            output_dir = args[["output_dir"]], smooth_fn = smooth_fn
        ),
        merge = .smooth_merge(
            tile = args[["tile"]], band = args[["band"]],
            out_file = job[["out_file"]], block_files = args[["block_files"]],
            exclusion_mask = args[["exclusion_mask"]],
            multicores = args[["multicores"]],
            output_dir = args[["output_dir"]], version = args[["version"]]
        ),
        resume = args[["tile"]]
    )
}
#' @title Log the start of the smoothing of a tile
#' @name .smooth_log_start
#' @keywords internal
#' @noRd
#' @param  job  First job of the tile
#' @return No value, called for side effect
.smooth_log_start <- function(job) {
    .debug_log(event = "start_tile", key = "file", value = job[["out_file"]])
    # a resumed tile has no blocks
    if (job[["role"]] != "resume") {
        .debug_log(
            event = "start_tile_blocks", key = "file", value = job[["out_file"]]
        )
    }
}
#' @title Log the end of the blocks of a tile
#' @name .smooth_log_last
#' @keywords internal
#' @noRd
#' @param  job  Last job of the tile
#' @return No value, called for side effect
.smooth_log_last <- function(job) {
    # a merge is sent once the blocks of its tile are done
    if (job[["role"]] == "merge") {
        .debug_log(
            event = "end_tile_blocks", key = "file", value = job[["out_file"]]
        )
    }
}
#' @title Log the end of the smoothing of a tile
#' @name .smooth_log_end
#' @keywords internal
#' @noRd
#' @param  job    Last job of the tile
#' @param  value  Smoothed tile
#' @return No value, called for side effect
.smooth_log_end <- function(job, value) {
    .debug_log(event = "end_tile", key = "file", value = job[["out_file"]])
}
#' @title Jobs of the smoothing of one tile
#' @name .smooth_tile_jobs
#' @keywords internal
#' @noRd
#' @param  tile            Tile to be smoothed
#' @param  band            Band to be processed
#' @param  block           Individual block that will be processed
#' @param  overlap         Overlapping pixels of the blocks
#' @param  exclusion_mask  Exclusion mask, or NULL
#' @param  merge_threads   Threads of the merge
#' @param  output_dir      Directory where image will be save
#' @param  version         Version of result
#' @return Tibble with the columns role, out_file and args: the blocks and
#'         then the merge, or one resume job when a valid file of the tile
#'         exists
.smooth_tile_jobs <- function(tile, band, block, overlap, exclusion_mask,
                              merge_threads, output_dir, version) {
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        # the recovery message is shown by this process, not by a worker
        .check_recovery()
        probs_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .tile_labels(tile),
            derived_class = "probs_cube",
            update_bbox = FALSE
        )
        return(tibble::tibble(
            role = "resume", out_file = out_file,
            args = list(list(tile = probs_tile))
        ))
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile, overlap = overlap, block = block
    )
    tile_mask <- exclusion_mask
    if (.has(tile_mask)) {
        # Remove chunks within the exclusion mask
        chunks <- .chunks_filter_mask(chunks = chunks, mask = tile_mask)
        tile_mask <- .chunks_crop_mask(chunks = chunks, mask = tile_mask)
    }
    chunk_list <- slider::slide(chunks, identity)
    block_files <- purrr::map_chr(chunk_list, function(chunk) {
        .file_block_name(
            pattern = .file_pattern(out_file),
            block = .block(chunk),
            output_dir = output_dir
        )
    })
    block_args <- purrr::map(chunk_list, function(chunk) {
        list(chunk = chunk, tile = tile, band = band, output_dir = output_dir)
    })
    merge_args <- list(
        tile = tile, band = band, block_files = block_files,
        exclusion_mask = tile_mask, multicores = merge_threads,
        output_dir = output_dir, version = version
    )
    tibble::tibble(
        role = c(rep("block", length(block_args)), "merge"),
        out_file = out_file,
        args = c(block_args, list(merge_args))
    )
}

#---- Bayesian smoothing ----
#' @title Smooth probability cubes with spatial predictors
#' @noRd
#' @description One queue of jobs over every tile, run by
#'   \code{.jobs_map_queue()}: the blocks of each tile, in tile order, and
#'   the merge of a tile, which waits for its blocks and goes before the
#'   blocks of the next tiles. So a worker merges a tile while the others
#'   smooth the blocks of the next tile.
#' @param  cube              Probability data cube.
#' @param  block             Individual block that will be processed
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a vector or a scalar.
#' @param  use_torch         Use torch to run the Bayesian smoother?
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @param  progress          Check progress bar?
#' @return                   Smoothed data cube
#'
.smooth <- function(cube,
                    block,
                    window_size,
                    neigh_fraction,
                    smoothness,
                    use_torch,
                    exclusion_mask,
                    multicores,
                    memsize,
                    output_dir,
                    version,
                    progress) {
    # Select smoothing implementation
    smooth_fn <- if (use_torch) {
        .smooth_fn_bayes_torch
    } else {
        .smooth_fn_bayes
    }
    # Create smooth function
    smooth_fn <- smooth_fn(
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        smoothness = smoothness
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    band <- "bayes"
    jobs <- .cube_jobs_create(
        cube, .smooth_tile_jobs, band = band, block = block,
        overlap = overlap, exclusion_mask = exclusion_mask,
        # threads of the merge, as when the main process merged
        merge_threads = .jobs_multicores(), output_dir = output_dir,
        version = version
    )
    tiles <- .jobs_map_queue(
        jobs, fn = .smooth_job, smooth_fn = smooth_fn,
        on_start = .smooth_log_start, on_last = .smooth_log_last,
        on_end = .smooth_log_end, progress = progress
    )
    dplyr::bind_rows(tiles)
}
#' @title Define smoothing function
#' @noRd
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a vector or a scalar.
#' @return Function to be applied to smoothen data
.smooth_fn_bayes <- function(window_size,
                             neigh_fraction,
                             smoothness) {
    # Check window size
    .check_int_parameter(window_size, min = 5L, max = 21L, is_odd = TRUE)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, min = 0.1, max = 1.0)

    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Compute logit
        # adjust values to avoid -Inf or +Inf in logits
        values[values == 1.0] <- 0.999999
        values[values == 0.0] <- 0.000001
        # tranform to logits
        values <- log(values / (rowSums(values) - values))
        # Process Bayesian
        values <- bayes_smoother_fraction(
            logits = values,
            nrows = .nrows(block),
            ncols = .ncols(block),
            window_size = window_size,
            smoothness = smoothness,
            neigh_fraction = neigh_fraction
        )
        # Compute inverse logit
        values <- exp(values) / (exp(values) + 1.0)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
#' @title Smooth a vector probability cube
#' @name .smooth_vector
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  cube              Probability vector cube.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter).
#' @param  output_dir        Output directory for image files.
#' @param  version           Version of resulting image.
#' @param  progress          Show progress bar?
#' @param ...                Additional parameters to exactextractr.
#' @return                   Smoothed data cube.
.smooth_vector <- function(cube,
                           neigh_fraction,
                           smoothness,
                           output_dir,
                           version,
                           progress, ...) {
    # Process each tile sequentially
    smooth_cube <- .cube_foreach_tile(cube, function(tile) {
        .smooth_vector_tile(
            tile = tile,
            neigh_fraction = neigh_fraction,
            smoothness = smoothness,
            output_dir = output_dir,
            version = version,
            progress = progress, ...
        )
    })
    # Set probs_vector_cube class chain
    vector_classes <- c(
        .conf_vector_s3class("probs_vector_cube"),
        class(smooth_cube)
    )
    smooth_cube <- .cube_set_class(smooth_cube, vector_classes)
    return(smooth_cube)
}
#' @title Smooth a vector probability tile
#' @name .smooth_vector_tile
#' @keywords internal
#' @noRd
#' @param tile              Tile of a data cube.
#' @param neigh_fraction    Fraction of neighbors with high probabilities
#'                          to be used in Bayesian inference.
#' @param smoothness        Smoothness vector (one value per class).
#' @param output_dir        Output directory for image files.
#' @param version           Version of resulting image.
#' @param progress          Show progress bar?
#' @param ...               Additional parameters to exactextractr.
#' @return                  Smoothed tile.
.smooth_vector_tile <- function(tile,
                                neigh_fraction,
                                smoothness,
                                output_dir,
                                version,
                                progress, ...) {
    band <- "bayes"
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        smooth_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .tile_labels(tile),
            derived_class = "probs_cube",
            update_bbox = FALSE
        )
        smooth_tile[["vector_info"]] <- tile[["vector_info"]]
        vector_classes <- c(
            .conf_vector_s3class("probs_vector_cube"),
            class(smooth_tile)
        )
        return(.cube_set_class(smooth_tile, vector_classes))
    }

    # Get tile band
    tile_band <- .tile_bands(tile)
    # Get band configuration
    band_conf <- .tile_band_conf(tile = tile, band = tile_band)
    # Get labels
    labels <- .tile_labels(tile)
    # Read the segments
    segments <- .segments_read_vec(tile)
    # Create ID for segments
    segments[["ID"]] <- seq_len(nrow(segments))
    # Open probability raster (all bands)
    probs_path <- .tile_path(tile)
    probs_rast <- .raster_open_rast(probs_path)

    # Extract values
    values <- .segments_extract_features(
        rast = probs_rast,
        segments = segments,
        fun = NULL,
        seg_id_col = "ID",
        include_cell = TRUE,
        ...
    )
    # Get ID and cell from segments
    values_id <- values[["ID"]]
    values_cell <- values[["cell"]]
    # Probability columns (all bands in the probs raster)
    prob_cols <- setdiff(colnames(values), c("ID", "cell"))
    # Convert values to matrix
    values <- as.matrix(values[, prob_cols])
    # Apply scale and offset to extracted probability values
    values <- .tile_scale(tile = tile, band = tile_band, values = values)
    # Avoid zero or one values to prevent -Inf/Inf/NaN in logit
    values[values <= 0.00001] <- 0.00001
    values[values >= 0.99999] <- 0.99999

    # Calculate denom
    row_sums <- rowSums(values)
    denom <- row_sums - values
    denom[denom <= 0.00001] <- 0.00001
    # Calculate logits
    values <- log(values / denom)

    # Call C++ function to perform segment-based Bayesian smoothing
    values <- segment_bayes(
        logits = values,
        ids = as.integer(values_id),
        n_segments = nrow(segments),
        neigh_fraction = neigh_fraction,
        smoothness = smoothness
    )

    # Convert logits back to probabilities (inverse logit)
    values <- exp(values) / (exp(values) + 1.0)

    # Unscale values
    values <- .tile_unscale(
        tile = tile,
        band = band,
        values = values,
        derived_class = "probs_cube"
    )

    # Create empty raster with same structure
    smooth_rast <- .raster_rast(probs_rast, nlayers = length(prob_cols))
    .raster_init_value(smooth_rast, NA_real_)
    names(smooth_rast) <- prob_cols

    # Assign smoothed values to corresponding cells
    smooth_rast[values_cell] <- values

    # Set missing value
    smooth_rast <- .raster_set_na(smooth_rast, .miss_value(band_conf))
    # Write raster
    .raster_write_rast(
        rast = smooth_rast,
        file = out_file,
        data_type = .data_type(band_conf),
        overwrite = TRUE,
        missing_value = .miss_value(band_conf)
    )
    # Create output tile from file
    smooth_tile <- .tile_derived_from_file(
        file = out_file,
        band = band,
        base_tile = tile,
        derived_class = "probs_cube",
        labels = labels,
        update_bbox = FALSE
    )
    # Preserve vector_info from the input probs tile
    smooth_tile[["vector_info"]] <- tile[["vector_info"]]
    # Set probs_vector_cube + probs_cube chain
    vector_classes <- c(
        .conf_vector_s3class("probs_vector_cube"),
        class(smooth_tile)
    )
    .cube_set_class(smooth_tile, vector_classes)
}
