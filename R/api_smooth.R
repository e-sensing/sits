#' @title Smooth a tile
#' @name .smooth_tile
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  tile            Subset of a data cube containing one tile
#' @param  band            Band to be processed
#' @param  block           Individual block that will be processed
#' @param  overlap         Overlap between tiles (if required)
#' @param  smooth_fn       Smoothing function
#' @param  output_dir      Directory where image will be save
#' @param  version         Version of result
#' @param  progress        Check progress bar?
#' @return                 Smoothed tile-band combination
.smooth_tile <- function(tile,
                         band,
                         block,
                         overlap,
                         exclusion_mask,
                         smooth_fn,
                         output_dir,
                         version,
                         progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
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
    # Calculate exclusion mask
    if (.has(exclusion_mask)) {
        # Remove chunks within the exclusion mask
        chunks <- .chunks_filter_mask(
            chunks = chunks,
            mask = exclusion_mask
        )
        exclusion_mask <- .chunks_crop_mask(
            chunks = chunks,
            mask = exclusion_mask
        )
    }
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
        if (all(.raster_is_valid(block_file))) {
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
    }, progress = progress)
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
    probs_tile <- .tile_derived_merge_blocks(
        file = merge_out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Exclude masked areas
    if (.has(exclusion_mask)) {
        # crop
        probs_tile_crop <- .crop(
            cube = probs_tile,
            roi = exclusion_mask,
            output_dir = output_dir,
            multicores = 1L,
            overwrite = TRUE,
            progress = progress
        )

        # delete old files
        unlink(.fi_paths(.fi(probs_tile)))

        # assign new cropped value in the old probs variable
        probs_tile <- probs_tile_crop
    }
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
#' @param  progress          Check progress bar?
#' @return                   Smoothed data cube
#'
.smooth <- function(cube,
                    block,
                    window_size,
                    neigh_fraction,
                    smoothness,
                    exclusion_mask,
                    multicores,
                    memsize,
                    output_dir,
                    version,
                    progress) {
    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .smooth_fn_bayes(
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        smoothness = smoothness
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Smoothing
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # Smooth the data
        .smooth_tile(
            tile = tile,
            band = "bayes",
            block = block,
            overlap = overlap,
            exclusion_mask = exclusion_mask,
            smooth_fn = smooth_fn,
            output_dir = output_dir,
            version = version,
            progress = progress
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
    .check_int_parameter(window_size, min = 5L, is_odd = TRUE)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, exclusive_min = 0.0, max = 1.0)

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
#' @return                   Smoothed data cube.
.smooth_vector <- function(cube,
                           neigh_fraction,
                           smoothness,
                           output_dir,
                           version,
                           progress) {
    # Process each tile sequentially
    smooth_cube <- .cube_foreach_tile(cube, function(tile) {
        .smooth_vector_tile(
            tile = tile,
            neigh_fraction = neigh_fraction,
            smoothness = smoothness,
            output_dir = output_dir,
            version = version,
            progress = progress
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
#' @return                  Smoothed tile.
.smooth_vector_tile <- function(tile,
                                neigh_fraction,
                                smoothness,
                                output_dir,
                                version,
                                progress) {
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

    # Get labels
    labels <- .tile_labels(tile)
    # Read the segments
    segments <- .segments_read_vec(tile)
    # Open probability raster (all bands)
    probs_path <- .tile_path(tile)
    probs_rast <- .raster_open_rast(probs_path)
    # Extract pixel probabilities for each segment
    extracted <- .raster_extract(
        rast = probs_rast,
        xy = .raster_open_vect(segments),
        fun = NULL
    )
    # Apply scale and offset to extracted probability values
    prob_cols <- setdiff(colnames(extracted), "ID")
    probs_band <- .tile_bands(tile)[[1L]]
    probs_band_conf <- .tile_band_conf(tile, probs_band)
    probs_scale <- .scale(probs_band_conf)
    if (.has(probs_scale) && probs_scale != 1.0) {
        extracted[, prob_cols] <- extracted[, prob_cols] * probs_scale
    }
    probs_offset <- .offset(probs_band_conf)
    if (.has(probs_offset) && probs_offset != 0.0) {
        extracted[, prob_cols] <- extracted[, prob_cols] + probs_offset
    }

    # Probability columns (all bands in the probs raster)
    prob_cols <- setdiff(colnames(extracted), "ID")
    probs_matrix <- as.matrix(extracted[, prob_cols, drop = FALSE])

    # Avoid zero or one values to prevent -Inf/Inf/NaN in logit
    probs_matrix[probs_matrix <= 0.00001] <- 0.00001
    probs_matrix[probs_matrix >= 0.99999] <- 0.99999

    row_sums <- rowSums(probs_matrix)
    denom <- row_sums - probs_matrix
    denom[denom <= 0.00001] <- 0.00001

    logit_probs <- log(probs_matrix / denom)

    # Call C++ function to perform segment-based Bayesian smoothing
    smoothed_logits <- segment_bayes(
        logits = logit_probs,
        ids = as.integer(extracted[["ID"]]),
        n_segments = nrow(segments),
        neigh_fraction = neigh_fraction,
        smoothness = smoothness
    )

    # Convert logits back to probabilities (inverse logit)
    smoothed_probs <- exp(smoothed_logits) / (exp(smoothed_logits) + 1.0)

    # Aggregate smoothed probabilities per segment (mean)
    segment_ids <- sort(unique(extracted[["ID"]]))
    seg_probs <- lapply(segment_ids, function(sid) {
        seg_rows <- which(extracted[["ID"]] == sid)
        if (length(seg_rows) == 0L) {
            return(rep(NA_real_, length(prob_cols)))
        }
        colMeans(smoothed_probs[seg_rows, , drop = FALSE], na.rm = TRUE)
    })
    seg_probs_matrix <- do.call(rbind, seg_probs)
    colnames(seg_probs_matrix) <- prob_cols

    # Band configuration for saving
    band_conf <- .conf_derived_band(
        derived_class = "probs_cube", band = band
    )
    # Apply offset/scale for saving probabilities
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0.0) {
        seg_probs_matrix <- seg_probs_matrix - offset
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1.0) {
        seg_probs_matrix <- seg_probs_matrix / scale
        seg_probs_matrix[seg_probs_matrix > 10000.0] <- 10000.0
    }

    # Rasterize: assign smoothed probabilities to all pixels within each segment
    seg_vect <- .raster_open_vect(segments[segment_ids, ])
    for (class_name in colnames(seg_probs_matrix)) {
        seg_vect[[class_name]] <- seg_probs_matrix[, class_name]
    }
    # Rasterize segments onto the template for each class individually
    smooth_rasts <- lapply(colnames(seg_probs_matrix), function(class_name) {
        template_rast <- .raster_rast(probs_rast, nlayers = 1L)
        .raster_rasterize(
            vect = seg_vect,
            rast = template_rast,
            field = class_name,
            fun = "max"
        )
    })
    # Combine the rasters into a multi-layer SpatRaster
    smooth_rast <- terra::rast(smooth_rasts)
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
