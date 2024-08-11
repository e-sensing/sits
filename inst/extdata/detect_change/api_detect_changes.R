#' @title Detect changes in time-series using various methods.
#' @name .detect_change_ts
#' @keywords internal
#' @noRd
.detect_change_ts <- function(samples,
                              cd_method,
                              filter_fn,
                              multicores,
                              progress) {
    # Start parallel workers
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Get bands from model
    bands <- .ml_bands(cd_method)
    # Update samples bands order
    if (any(bands != .samples_bands(samples))) {
        samples <- .samples_select_bands(samples = samples,
                                         bands = bands)
    }
    # Apply time series filter
    if (.has(filter_fn)) {
        samples <- .apply_across(data = samples,
                                 fn = filter_fn)
    }
    # Divide samples in chunks to parallel processing
    parts <- .pred_create_partition(pred = samples, partitions = multicores)
    # Detect changes!
    .jobs_map_parallel_dfr(parts, function(part) {
        # Get samples
        values <- .pred_part(part)
        # Detect changes! For detection, models can be time-aware. So, the
        # complete data, including dates, must be passed as argument.
        detections <- cd_method(values[["time_series"]], "ts")
        detections <- tibble::tibble(detections)
        # Prepare result
        result <- tibble::tibble(data.frame(values, detections = detections))
        class(result) <- class(values)
        # return
        result
    }, progress = progress)
}

#' @title Detect changes from a chunk of raster data using multicores
#' @name .detect_change_tile
#' @keywords internal
#' @noRd
#' @param  tile            Single tile of a data cube.
#' @param  band            Band to be produced.
#' @param  cd_method       Change Detection Model.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @param  impute_fn       Imputation function.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @param  verbose         Print processing information?
#' @param  progress        Show progress bar?
#' @return List of the classified raster layers.
.detect_change_tile <- function(tile,
                           band,
                           cd_method,
                           block,
                           roi,
                           filter_fn,
                           impute_fn,
                           output_dir,
                           version,
                           verbose,
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
        if (.check_messages()) {
            .check_recovery(out_file)
        }
        detected_changes_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .ml_labels_code(cd_method),
            derived_class = "detections_cube",
            update_bbox = TRUE
        )
        return(detected_changes_tile)
    }
    # Show initial time for tile classification
    tile_start_time <- .tile_classif_start(
        tile = tile,
        verbose = verbose
    )
    # Tile timeline
    tile_timeline <- .tile_timeline(tile)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile,
        overlap = 0,
        block = block
    )
    # By default, update_bbox is FALSE
    update_bbox <- FALSE
    if (.has(roi)) {
        # How many chunks there are in tile?
        nchunks <- nrow(chunks)
        # Intersecting chunks with ROI
        chunks <- .chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
        # Should bbox of resulting tile be updated?
        update_bbox <- nrow(chunks) != nchunks
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
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .classify_data_read(
            tile = tile,
            block = block,
            bands = .ml_bands(cd_method),
            base_bands = NULL,
            ml_model = cd_method,
            impute_fn = impute_fn,
            filter_fn = filter_fn
        )
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Fill with zeros remaining NA pixels
        values <- C_fill_na(values, 0)
        # Used to check values (below)
        input_pixels <- nrow(values)
        # Include names in cube predictors
        colnames(values) <- .pred_features_name(
            .ml_bands(cd_method), tile_timeline
        )
        # Prepare values
        values <- .pred_as_ts(values, .ml_bands(cd_method), tile_timeline) |>
                    tidyr::nest(.by = "sample_id", .key = "time_series")
        # Log here
        .debug_log(
            event = "start_block_data_detection",
            key = "model",
            value = .ml_class(cd_method)
        )
        # Detect changes!
        values <- cd_method(values[["time_series"]], "cube") |>
                    dplyr::as_tibble()
        # Are the results consistent with the data input?
        .check_processed_values(
            values = values,
            input_pixels = input_pixels
        )
        # Log here
        .debug_log(
            event = "end_block_data_detection",
            key = "model",
            value = .ml_class(cd_method)
        )
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "detections_cube",
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
        # Mask NA pixels with same probabilities for all classes
        values[na_mask, ] <- 0  # event detection = 1, no event = 0
        # Log
        .debug_log(
            event = "start_block_data_save",
            key = "file",
            value = block_file
        )
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file,
            block = block,
            bbox = .bbox(chunk),
            values = values,
            data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Log
        .debug_log(
            event = "end_block_data_save",
            key = "file",
            value = block_file
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = progress)
    # Merge blocks into a new detections_cube tile
    detections_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .ml_labels_code(cd_method),
        base_tile = tile,
        block_files = block_files,
        derived_class = "detections_cube",
        multicores = .jobs_multicores(),
        update_bbox = update_bbox
    )
    # show final time for detection
    .tile_classif_end(
        tile = tile,
        start_time = tile_start_time,
        verbose = verbose
    )
    # Return detections tile
    detections_tile
}
