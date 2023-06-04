#' @title Classify a chunk of raster data  using multicores
#' @name .classify_tile
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data using multicores. It breaks
#' the data into horizontal blocks and divides them between the available cores.
#'
#' Reads data using terra, cleans the data for NAs and missing values.
#' The clean data is stored in a data table with the time instances
#' for all pixels of the block. The algorithm then classifies data on
#' an year by year basis. For each year, extracts the sub-blocks for each band.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  tile            Single tile of a data cube.
#' @param  band            Band to be produced.
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @param  impute_fn       Impute function to replace NA.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @param  verbose         Print processing information?
#' @param  progress        Show progress bar?
#' @return List of the classified raster layers.
.classify_tile  <- function(tile,
                            band,
                            ml_model,
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
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        if (.check_messages()) {
            message("Recovery: tile '", tile[["tile"]], "' already exists.")
            message("(If you want to produce a new image, please ",
                    "change 'output_dir' or 'version' parameters)")
        }
        probs_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .ml_labels(ml_model),
            derived_class = "probs_cube",
            update_bbox = TRUE
        )
        return(probs_tile)
    }
    # # Callback final tile classification
    # .callback(process = "tile_classification", event = "started",
    #           context = environment())
    # Show initial time for tile classification
    if (verbose) {
        tile_start_time <- Sys.time()
        message("Starting classification of tile '",
                tile[["tile"]], "' at ", tile_start_time)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0, block = block)
    # By default, update_bbox is FALSE
    update_bbox <- FALSE
    if (.has(roi)) {
        # How many chunks there are in tile?
        nchunks <- nrow(chunks)
        # Intersecting chunks with ROI
        chunks <- .chunks_filter_spatial(chunks, roi = roi)
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
            ml_model = ml_model,
            impute_fn = impute_fn,
            filter_fn = filter_fn
        )
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Fill with zeros remaining NA pixels
        values <- C_fill_na(values, 0)
        # Used to check values (below)
        input_pixels <- nrow(values)

        #
        # Log here
        #
        .debug_log(
            event = "start_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )

        # Apply the classification model to values
        values <- ml_model(values)

        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)

        #
        # Log here
        #
        .debug_log(
            event = "end_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )

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

        # Mask NA pixels
        values[na_mask, ] <- NA

        #
        # Log here
        #
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

        #
        # Log here
        #
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
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .ml_labels(ml_model),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = .jobs_multicores(),
        update_bbox = update_bbox
    )
    # # Callback final tile classification
    # .callback(event = "tile_classification", status = "end",
    #           context = environment())
    # show final time for classification
    if (verbose) {
        tile_end_time <- Sys.time()
        message("Tile '", tile[["tile"]], "' finished at ", tile_end_time)
        message("Elapsed time of ",
                format(round(tile_end_time - tile_start_time, digits = 2)))
        message("")
    }
    # Return probs tile
    probs_tile
}
#' @title Read a block of values retrieved from a set of raster images
#' @name  .classify_data_read
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  tile            Input tile to read data.
#' @param  block           Bounding box in (col, row, ncols, nrows).
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @param  impute_fn       Impute function to replace NA.
#' @return A matrix with values for classification.
.classify_data_read <- function(tile, block, ml_model, impute_fn, filter_fn) {
    # For cubes that have a time limit to expire (MPC cubes only)
    tile <- .cube_token_generator(tile)
    # Read and preprocess values of cloud
    # Get cloud values (NULL if not exists)
    cloud_mask <- .tile_cloud_read_block(tile = tile, block = block)
    # Read and preprocess values of each band
    values <- purrr::map_dfc(.ml_bands(ml_model), function(band) {
        # Get band values (stops if band not found)
        values <- .tile_read_block(tile = tile, band = band, block = block)

        #
        # Log here
        #
        .debug_log(
            event = "start_block_data_process",
            key = "process",
            value = "cloud-impute-filter"
        )

        # Remove cloud masked pixels
        if (.has(cloud_mask)) {
            values[cloud_mask] <- NA
        }


        # Remove NA pixels
        if (.has(impute_fn)) {
            values <- impute_fn(values)
        }
        # Filter the time series
        if (.has(filter_fn)) {
            values <- filter_fn(values)
        }
        # Normalize values for old version model classifiers that
        #   do not normalize values itself
        # Models trained after version 1.2 do this automatically before
        #   classification
        stats <- .ml_stats_0(ml_model) # works for old models only!!
        if (.has(stats)) {
            q02 <- .stats_0_q02(stats, band)
            q98 <- .stats_0_q98(stats, band)
            if (.has(q02) && .has(q98)) {
                # Use C_normalize_data_0 to process old version of normalization
                values <- C_normalize_data_0(values, q02, q98)
            }
        }

        #
        # Log here
        #
        .debug_log(
            event = "end_block_data_process",
            key = "band",
            value = band
        )

        # Return values
        as.data.frame(values)
    })
    # Compose final values
    values <- as.matrix(values)
    # Set values features name
    colnames(values) <- .ml_features_name(ml_model)
    # Return values
    values
}
#' @title Classify a distances tibble using machine learning models
#' @name .classify_ts
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  samples    a tibble with sits samples
#' @param  ml_model   model trained by \code{\link[sits]{sits_train}}.
#' @param  filter_fn  Smoothing filter to be applied (if desired).
#' @param  multicores number of threads to process the time series.
#' @param  progress   Show progress bar?
#' @return A tibble with the predicted labels.
.classify_ts <- function(samples,
                         ml_model,
                         filter_fn,
                         multicores,
                         progress) {

    # Start parallel workers
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Get bands from model
    bands <- .ml_bands(ml_model)

    # Update samples bands order
    if (any(bands != .samples_bands(samples))) {
        samples <- .samples_select_bands(samples = samples, bands = bands)
    }

    # Apply time series filter
    if (.has(filter_fn)) {
        samples <- .apply_across(data = samples, fn = filter_fn)
    }

    # Compute the breaks in time for multiyear classification
    class_info <- .timeline_class_info(
        data = samples, samples = .ml_samples(ml_model)
    )

    # Split long time series of samples in a set of small time series
    if (length(class_info[["dates_index"]][[1]]) > 1) {
        splitted <- .samples_split(
            samples = samples,
            split_intervals = class_info[["dates_index"]][[1]]
        )
        pred <- .predictors(samples = splitted, ml_model = ml_model)
        # Post condition: is predictor data valid?
        .check_predictors(pred, splitted)
    } else {
        # Convert samples time series in predictors and preprocess data
        pred <- .predictors(samples = samples, ml_model = ml_model)
    }

    # Divide samples predictors in chunks to parallel processing
    parts <- .pred_create_partition(pred = pred, partitions = multicores)
    # Do parallel process
    prediction <- .jobs_map_parallel_dfr(parts, function(part) {
        # Get predictors of a given partition
        pred_part <- .pred_part(part)
        # Get predictors features to classify
        values <- .pred_features(pred_part)
        # Classify
        values <- ml_model(values)
        # Return classification
        values <- tibble::tibble(data.frame(values))
        values
    }, progress = progress)

    # Store the result in the input data
    if (length(class_info[["dates_index"]][[1]]) > 1) {
        prediction <- .tibble_prediction_multiyear(
            data = samples,
            class_info = class_info,
            prediction = prediction
        )
    } else {
        prediction <- .tibble_prediction(
            data = samples,
            prediction = prediction
        )
    }
    # Set result class and return it
    .set_class(x = prediction, "predicted", class(samples))
}
