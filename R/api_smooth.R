#' @title Smooth a tile
#' @name .apply_feature
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  tile.           Subset of a data cube containing one tile
#' @param  band            Band to be processed
#' @param  block           Individual block that will be processed
#' @param  overlap         Overlap between tiles (if required)
#' @param  smooth_fn       Smoothing function
#' @param  output_dir      Directory where image will be save
#' @param  version         Version of result
#' @return                 Smoothed tile-band combination
.smooth_tile <- function(tile,
                         band,
                         block,
                         overlap,
                         smooth_fn,
                         output_dir,
                         version) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        .check_recovery(tile[["tile"]])
        probs_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .tile_labels(tile),
            derived_class = "probs_cube",
            update_bbox = FALSE
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
    probs_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Return probs tile
    probs_tile
}


#---- Bayesian smoothing ----
#' @title Smooth probability cubes with spatial predictors
#' @noRd
#' @param  cube              Probability data cube.
#' @param  block             Individual block that will be processed
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a vector or a scalar.
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#'
.smooth <- function(cube,
                    block,
                    window_size,
                    neigh_fraction,
                    smoothness,
                    multicores,
                    memsize,
                    output_dir,
                    version) {
    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .smooth_fn_bayes(
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        smoothness = smoothness
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
    .check_window_size(window_size, min = 5)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, exclusive_min = 0, max = 1)

    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Compute logit
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
        values <- exp(values) / (exp(values) + 1)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
