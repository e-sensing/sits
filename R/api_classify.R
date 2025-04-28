#' @title Classify a chunk of raster data  using multicores
#' @name .classify_tile
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Classifies a block of data using multicores, breaking
#' the data into blocks and divides them between the available cores.
#' The size of the blocks is optimized to account for COG files and
#' for the balance of multicores and memory size.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  tile            Single tile of a data cube.
#' @param  out_band        Band to be produced.
#' @param  bands           Bands to extract time series
#' @param  base_bands      Base bands to extract values
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @param  impute_fn       Imputation function.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @param  verbose         Print processing information?
#' @param  progress        Show progress bar?
#' @return List of the classified raster layers.
.classify_tile <- function(tile,
                           out_band,
                           bands,
                           base_bands,
                           ml_model,
                           block,
                           roi,
                           exclusion_mask,
                           filter_fn,
                           impute_fn,
                           output_dir,
                           version,
                           verbose,
                           progress) {
    # Define the name of the output file
    out_file <- .file_derived_name(
        tile = tile,
        band = out_band,
        version = version,
        output_dir = output_dir
    )
    # If output file exists, builds a
    # probability cube directly from the file
    # and does not reprocess input
    if (file.exists(out_file)) {
        .check_recovery()
        probs_tile <- .tile_derived_from_file(
            file = out_file,
            band = out_band,
            base_tile = tile,
            labels = .ml_labels_code(ml_model),
            derived_class = "probs_cube",
            update_bbox = TRUE
        )
        return(probs_tile)
    }
    # Initial time for tile classification
    tile_start_time <- .tile_classif_start(
        tile = tile,
        verbose = verbose
    )
    # Create chunks to be allocated to jobs in parallel
    chunks <- .tile_chunks_create(
        tile = tile,
        overlap = 0L,
        block = block
    )
    # Create a variable to control updating of bounding box
    # by default, update_bbox is FALSE
    update_bbox <- FALSE
    if (.has(exclusion_mask)) {
        # How many chunks there are in tile?
        nchunks <- nrow(chunks)
        # Remove chunks within the exclusion mask
        chunks <- .chunks_filter_mask(
            chunks = chunks,
            mask = exclusion_mask
        )
        # Create crop region
        chunks["mask"] <- .chunks_crop_mask(
            chunks = chunks,
            mask = exclusion_mask
        )
        # Should bbox of resulting tile be updated?
        update_bbox <- nrow(chunks) != nchunks
    }
    if (.has(roi)) {
        # How many chunks do we need to process?
        nchunks <- nrow(chunks)
        # Intersect chunks with ROI
        chunks <- .chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
        # Update bbox to account for ROI
        update_bbox <- nrow(chunks) != nchunks
    }
    # Process jobs in parallel - one job per chunk
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Retrive block to be processed
        block <- .block(chunk)
        # Create a temporary block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values from files
        values <- .classify_data_read(
            tile = tile,
            block = block,
            bands = bands,
            base_bands = base_bands,
            ml_model = ml_model,
            impute_fn = impute_fn,
            filter_fn = filter_fn
        )
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Fill with zeros remaining NA pixels
        values <- C_fill_na(values, 0.0)
        # Define control variable to check for correct termination
        input_pixels <- nrow(values)
        # Start log file
        .debug_log(
            event = "start_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )
        # Apply the classification model to values
        # Uses the closure created by sits_train
        # Normalize and calibrate the values
        # Perform softmax for torch models,
        values <- values |>
            ml_model() |>
            .ml_normalize(ml_model)

        # Are the results consistent with the data input?
        .check_processed_values(
            values = values,
            input_pixels = input_pixels
        )
        # Log end of block
        .debug_log(
            event = "end_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )
        # Obtain configuration parameters for probability cube
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube",
            band = out_band
        )
        # Apply scaling to classified values
        band_scale <- .scale(band_conf)
        values <- values / band_scale
        # Put NA back in the result
        values[na_mask, ] <- NA
        # Log start of block saving
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
            crop_block = chunk[["mask"]]
        )
        # Log end of block saving
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
    # If ROI exists, blocks are merged to a different directory
    # than output_dir, which is used to save the final cropped version
    merge_out_file <- out_file
    if (.has(roi)) {
        merge_out_file <- .file_derived_name(
            tile = tile,
            band = out_band,
            version = version,
            output_dir = file.path(output_dir, ".sits")
        )
    }
    probs_tile <- .tile_derived_merge_blocks(
        file = merge_out_file,
        band = out_band,
        labels = .ml_labels_code(ml_model),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = .jobs_multicores(),
        update_bbox = update_bbox
    )
    # Clean GPU memory allocation
    .ml_gpu_clean(ml_model)
    # if there is a ROI, crop the probability cube
    if (.has(roi)) {
        probs_tile_crop <- .crop(
            cube = probs_tile,
            roi = roi,
            output_dir = output_dir,
            multicores = 1L,
            progress = progress
        )
        unlink(.fi_paths(.fi(probs_tile)))
    }
    # show final time for classification
    .tile_classif_end(
        tile = tile,
        start_time = tile_start_time,
        verbose = verbose
    )
    # Return probs tile (cropped version in case of ROI)
    if (.has(roi)) {
        probs_tile_crop
    } else {
        probs_tile
    }
}

#' @title Classify a chunk of raster data  using multicores
#' @name .classify_vector_tile
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Classifies a block of data using multicores. Breaks
#' the data into blocks and divides them between the available cores.
#' After all cores process their blocks,
#' joins the result and then writes it.
#'
#' @param  tile       Single tile of a data cube.
#' @param  bands      Bands to extract time series
#' @param  base_bands Base bands to extract values
#' @param  ml_model   Model trained by \code{\link[sits]{sits_train}}.
#' @param  block      Optimized block to be read into memory.
#' @param  roi        Region of interest.
#' @param  filter_fn  Smoothing filter function to be applied to the data.
#' @param  impute_fn  Imputation function to remove NA values.
#' @param  n_sam_pol  Number of samples per polygon to be read
#'                    for POLYGON or MULTIPOLYGON vector objects.
#' @param  multicores Number of cores for classification
#' @param  gpu_memory Memory available in GPU (default = NULL)
#' @param  version    Version of result.
#' @param  output_dir Output directory.
#' @param  progress   Show progress bar?
#' @return List of the classified raster layers.
.classify_vector_tile <- function(tile,
                                  bands,
                                  base_bands,
                                  ml_model,
                                  block,
                                  roi,
                                  filter_fn,
                                  impute_fn,
                                  n_sam_pol,
                                  multicores,
                                  memsize,
                                  gpu_memory,
                                  version,
                                  output_dir,
                                  progress) {
    # Define output vector file name and extension
    out_file <- .file_derived_name(
        tile = tile,
        band = "probs",
        version = version,
        output_dir = output_dir,
        ext = "gpkg"
    )
    # Checks if output file already exists
    # If TRUE, returns the existing file and avoids re-processing
    if (.segments_is_valid(out_file)) {
        .check_recovery()
        # Create tile based on template
        probs_tile <- .tile_segments_from_file(
            file = out_file,
            band = "probs",
            base_tile = tile,
            labels = .ml_labels(ml_model),
            vector_class = "probs_vector_cube",
            update_bbox = FALSE
        )
        return(probs_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile,
        overlap = 0L,
        block = block
    )
    # By default, update_bbox is FALSE
    if (.has(roi)) {
        # Intersecting chunks with ROI
        chunks <- .chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
    }
    # Filter segments that intersects with each chunk
    chunks <- .chunks_filter_segments(
        chunks = chunks,
        tile = tile,
        output_dir = output_dir
    )
    # Define that chunks will be deleted upon exit
    on.exit(unlink(unlist(chunks[["segments"]])))
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Retrieve block from chunk
        block <- .block(chunk)
        # Define block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir,
            ext = "gpkg"
        )
        # Resume processing in case of failure
        if (.segments_is_valid(block_file)) {
            return(block_file)
        }
        # Extract time series from segments
        # Number of time series per segment is defined by n_sam_pol
        segments_ts <- .segments_poly_read(
            tile = tile,
            bands = bands,
            base_bands = base_bands,
            chunk = chunk,
            n_sam_pol = n_sam_pol,
            impute_fn = impute_fn
        )
        # Deal with NO DATA cases (e.g., cloudy areas)
        if (nrow(segments_ts) == 0L) {
            return("")
        }
        # Classify times series
        # This is the same function called to classify
        # individual time series (with an extra polygon_id)
        segments_ts <- .classify_ts(
            samples = segments_ts,
            ml_model = ml_model,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            multicores = 1L,
            gpu_memory = gpu_memory,
            progress = progress
        )
        # Join probability values with segments
        segments_ts <- .segments_join_probs(
            data = segments_ts,
            segments = .segments_read_vec(tile)
        )
        # Write segment block
        .vector_write_vec(
            v_obj = segments_ts,
            file_path = block_file
        )
        # Free memory
        gc()
        # Return block file
        block_file
    }, progress = progress)
    # Remove empty block files
    block_files <- purrr::discard(block_files, Negate(nzchar))
    # Read segments from all block files
    segments_ts <- block_files |>
        purrr::map(.vector_read_vec) |>
        dplyr::bind_rows()
    # Write segments to a vector data cube
    .vector_write_vec(v_obj = segments_ts, file_path = out_file)
    # Create probability vector tile
    # joining vector and raster components of data cube
    probs_tile <- .tile_segments_from_file(
        file = out_file,
        band = "probs",
        base_tile = tile,
        labels = .ml_labels(ml_model),
        vector_class = "probs_vector_cube",
        update_bbox = FALSE
    )
    # Remove file blocks
    unlink(block_files)
    # Return probability vector tile
    probs_tile
}

#' @title Read a block of values from a set of raster images
#' @name  .classify_data_read
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @param  tile            Input tile to read data.
#' @param  block           Bounding box in (col, row, ncols, nrows).
#' @param  bands           Bands to extract time series
#' @param  base_bands      Base bands to extract values
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  impute_fn       Imputation function
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @return A matrix with values for classification.
.classify_data_read <- function(tile, block, bands, base_bands,
                                ml_model, impute_fn, filter_fn) {
    # For cubes that have a time limit to expire (MPC cubes only)
    tile <- .cube_token_generator(tile)
    # Read and preprocess values of cloud
    # Get cloud values (NULL if data does not exist)
    cloud_mask <- .tile_cloud_read_block(
        tile = tile,
        block = block
    )
    # Read and preprocess values of each eo band
    values <- purrr::map(bands, function(band) {
        # Get band values (stops if band not found)
        values <- .tile_read_block(
            tile = tile,
            band = band,
            block = block
        )
        # Log
        .debug_log(
            event = "start_block_data_process",
            key = "process",
            value = "cloud-impute-filter"
        )
        # Remove cloud masked pixels
        if (.has(cloud_mask)) {
            values[cloud_mask] <- NA
        }
        # are there NA values? interpolate them
        if (anyNA(values)) {
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
                # Use C_normalize_data_0 to process old version
                values <- C_normalize_data_0(values, q02, q98)
            }
        }
        # Log
        .debug_log(
            event = "end_block_data_process",
            key = "band",
            value = band
        )
        # Return values
        as.data.frame(values)
    })
    # Read and preprocess values of each base band
    values_base <- purrr::map(base_bands, function(band) {
        # Read and preprocess values of each base band
        values_base <- .tile_read_block(
            tile = .tile_base_info(tile),
            band = band,
            block = block
        )
        # Return values
        as.data.frame(values_base)
    })
    # Combine two lists
    values <- c(values, values_base)
    # collapse list to get data.frame
    values <- suppressMessages(
        purrr::list_cbind(values, name_repair = "universal")
    )
    # Compose final values
    values <- as.matrix(values)
    # Set values features name
    if (.has(ml_model)) {
        colnames(values) <- .ml_features_name(ml_model)
    }
    # Return values
    values
}
#' @title Classify a sits tibble using machine learning models
#' @name .classify_ts
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  samples    Tibble with sits samples
#' @param  ml_model   Model trained by \code{\link[sits]{sits_train}}.
#' @param  filter_fn  Smoothing filter to be applied (if desired).
#' @param  impute_fn  Imputation function (to remove NA)
#' @param  multicores number of threads to process the time series.
#' @param  gpu_memory Memory available in GPU
#' @param  progress   Show progress bar?
#' @return A tibble with the predicted labels.
.classify_ts <- function(samples,
                         ml_model,
                         filter_fn,
                         impute_fn,
                         multicores,
                         gpu_memory,
                         progress) {
    # Start parallel workers
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Get bands from model
    bands <- .ml_bands(ml_model)
    # Update samples bands order
    if (any(bands != .samples_bands(samples))) {
        samples <- .samples_select_bands(
            samples = samples,
            bands = bands
        )
    }
    # Apply time series filter
    if (.has(filter_fn)) {
        samples <- .apply_across(
            data = samples,
            fn = filter_fn
        )
    }
    # Apply imputation filter
    if (.has(impute_fn)) {
        samples <- .apply_across(
            data = samples,
            fn = impute_fn
        )
    }
    # Compute the breaks in time for multiyear classification
    class_info <- .timeline_class_info(
        data = samples,
        samples = .ml_samples(ml_model)
    )
    # Split long time series of samples in a set of small time series
    if (length(class_info[["dates_index"]][[1L]]) > 1L) {
        splitted <- .samples_split(
            samples = samples,
            split_intervals = class_info[["dates_index"]][[1L]]
        )
        pred <- .predictors(
            samples = splitted,
            ml_model = ml_model
        )
        # Post condition: is predictor data valid?
        .check_predictors(
            pred = pred,
            samples = splitted
        )
    } else {
        # Convert samples time series in predictors and preprocess data
        pred <- .predictors(
            samples = samples,
            ml_model = ml_model
        )
    }
    # choose between GPU and CPU
    if (.torch_gpu_classification() && .ml_is_torch_model(ml_model)) {
        prediction <- .classify_ts_gpu(
            pred = pred,
            ml_model = ml_model,
            gpu_memory = gpu_memory
        )
    } else {
        prediction <- .classify_ts_cpu(
            pred = pred,
            ml_model = ml_model,
            multicores = multicores,
            progress = progress
        )
    }
    # Store the result in the input data
    if (length(class_info[["dates_index"]][[1L]]) > 1L) {
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
    prediction <- .set_class(
        x = prediction, "predicted",
        class(samples)
    )
    prediction
}
#' @title Classify predictors using CPU
#' @name .classify_ts_cpu
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  pred       a tibble with predictors
#' @param  ml_model   model trained by \code{\link[sits]{sits_train}}.
#' @param  multicores number of threads to process the time series.
#' @param  progress   Show progress bar?
#' @return A tibble with the predicted values.
.classify_ts_cpu <- function(pred,
                             ml_model,
                             multicores,
                             progress) {
    # Divide samples predictors in chunks to parallel processing
    parts <- .pred_create_partition(
        pred = pred,
        partitions = multicores
    )
    # Do parallel process
    prediction <- .jobs_map_parallel_dfr(parts, function(part) {
        # Extract predictors of a given partition
        # Extract features to classify
        # Classify, normalize and calibrate value
        values <- part |>
            .pred_part() |>
            .pred_features() |>
            ml_model() |>
            .ml_normalize(ml_model)
        # Extract columns
        values_columns <- colnames(values)
        # Transform classification results
        values <- tibble::tibble(as.data.frame(values))
        # Fix column names to avoid errors with non-standard column name
        # (e.g., with spaces, icons)
        colnames(values) <- values_columns
        # Return classification
        values
    }, progress = progress)
    prediction
}
#' @title Classify predictors using GPU
#' @name .classify_ts_gpu
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  pred       a tibble with predictors
#' @param  ml_model   model trained by \code{\link[sits]{sits_train}}.
#' @param  gpu_memory memory available in GPU
#' @return A tibble with the predicted values.
.classify_ts_gpu <- function(pred,
                             ml_model,
                             gpu_memory) {
    # estimate size of GPU memory required (in GB)
    pred_size <- nrow(pred) * ncol(pred) * 8.0 / 1000000000.0
    # estimate how should we partition the predictors
    num_parts <- ceiling(pred_size / gpu_memory)
    # Divide samples predictors in chunks to parallel processing
    parts <- .pred_create_partition(
        pred = pred,
        partitions = num_parts
    )
    prediction <- slider::slide_dfr(parts, function(part) {
        # Get predictors of a given partition
        # Get predictors features to classify
        # Classify
        # normalize and calibrate values
        values <- part |>
            .pred_part() |>
            .pred_features() |>
            ml_model() |>
            .ml_normalize(ml_model)
        # Extract columns
        values_columns <- colnames(values)
        # Transform classification results
        values <- tibble::tibble(as.data.frame(values))
        # Fix column names to avoid errors with non-standard column name
        # (e.g., with spaces, icons)
        colnames(values) <- values_columns
        # Clean GPU memory
        .ml_gpu_clean(ml_model)
        values
    })
    prediction
}
#' @title Start recording processing time
#' @name .classify_verbose_start
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Prints the block size and computes
#' start time for processing
#'
#' @param  verbose TRUE/FALSE
#' @param  block   block size
#' @return start time for processing
.classify_verbose_start <- function(verbose, block) {
    if (verbose) {
        msg <- paste0(
            .conf("messages", ".verbose_block_size"), " ",
            .nrows(block), " x ", .ncols(block)
        )
        message(msg)
    }
    Sys.time()
}
#' @title End recording processing time
#' @name .classify_verbose_end
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Prints the processing time
#' @param  verbose TRUE/FALSE
#' @param  start_time   initial processing time
#' @return elapsed processing time for processing
.classify_verbose_end <- function(verbose, start_time) {
    if (verbose) {
        end_time <- Sys.time()
        message("")
        message(.conf("messages", ".verbose_task_end"), end_time)
        message(
            .conf("messages", ".verbose_task_elapsed"),
            format(round(end_time - start_time, digits = 2L))
        )
    }
}
