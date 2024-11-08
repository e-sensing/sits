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
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = out_band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        if (.check_messages()) {
            .check_recovery(out_file)
        }
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
    # Show initial time for tile classification
    tile_start_time <- .tile_classif_start(
        tile = tile,
        verbose = verbose
    )
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile,
        overlap = 0,
        block = block
    )
    # By default, update_bbox is FALSE
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
        chunks_mask <- .chunks_crop_mask(
            chunks = chunks,
            mask = exclusion_mask
        )
        # Should bbox of resulting tile be updated?
        update_bbox <- nrow(chunks) != nchunks
    }
    if (.has(roi)) {
        # How many chunks still available ?
        nchunks <- nrow(chunks)
        # Intersecting chunks with ROI
        chunks <- .chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
        # Should bbox of resulting tile be updated?
        update_bbox <- nrow(chunks) != nchunks
    }
    # Compute fractions probability
    probs_fractions <- 1 / length(.ml_labels(ml_model))
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
            bands = bands,
            base_bands = base_bands,
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
        # Log here
        .debug_log(
            event = "start_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )
        # Apply the classification model to values
        values <- ml_model(values)
        # normalize and calibrate the values
        values <- .ml_normalize(ml_model, values)
        # Are the results consistent with the data input?
        .check_processed_values(
            values = values,
            input_pixels = input_pixels
        )
        # Log
        .debug_log(
            event = "end_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube",
            band = out_band
        )
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            values <- values / scale
            probs_fractions  <- probs_fractions / scale
        }
        # Mask NA pixels with same probabilities for all classes
        values[na_mask, ] <- probs_fractions
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
            mask = chunks_mask
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
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = out_band,
        labels = .ml_labels_code(ml_model),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = .jobs_multicores(),
        update_bbox = update_bbox
    )
    # show final time for classification
    .tile_classif_end(
        tile = tile,
        start_time = tile_start_time,
        verbose = verbose
    )
    # Clean GPU memory allocation
    .ml_gpu_clean(ml_model)
    # Return probs tile
    probs_tile
}

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
#' @param  tile       Single tile of a data cube.
#' @param  bands      Bands to extract time series
#' @param  base_bands Base bands to extract values
#' @param  ml_model   Model trained by \code{\link[sits]{sits_train}}.
#' @param  block      Optimized block to be read into memory.
#' @param  roi        Region of interest.
#' @param  filter_fn  Smoothing filter function to be applied to the data.
#' @param  impute_fn  Imputation function to remove NA values.
#' @param  n_sam_pol  Number of samples per polygon to be read
#'                    for POLYGON or MULTIPOLYGON shapefiles or sf objects.
#' @param  multicores Number of cores to be used for classification
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
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = "probs",
        version = version,
        output_dir = output_dir,
        ext = "gpkg"
    )
    # Resume feature
    if (.segments_is_valid(out_file)) {
        if (.check_messages()) {
            .check_recovery(out_file)
        }
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
        overlap = 0,
        block = block
    )
    # By default, update_bbox is FALSE
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
    } else {
        update_bbox <- FALSE
    }
    # Filter segments that intersects with each chunk
    chunks <- .chunks_filter_segments(
        chunks = chunks,
        tile = tile,
        output_dir = output_dir
    )
    on.exit(unlink(unlist(chunks[["segments"]])))
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
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
        # Extract segments time series
        segments_ts <- .segments_poly_read(
            tile = tile,
            bands = bands,
            base_bands = base_bands,
            chunk = chunk,
            n_sam_pol = n_sam_pol,
            impute_fn = impute_fn
        )
        # Classify segments
        segments_ts <- .classify_ts(
            samples = segments_ts,
            ml_model = ml_model,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            multicores = 1,
            gpu_memory = gpu_memory,
            progress = FALSE
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
        return(block_file)
    }, progress = progress)
    # Read all segments
    segments_ts <- purrr::map(block_files, .vector_read_vec)
    segments_ts <- dplyr::bind_rows(segments_ts)
    # Write all segments
    .vector_write_vec(v_obj = segments_ts, file_path = out_file)
    # Create tile based on template
    probs_tile <- .tile_segments_from_file(
        file = out_file,
        band = "probs",
        base_tile = tile,
        labels = .ml_labels(ml_model),
        vector_class = "probs_vector_cube",
        update_bbox = FALSE
    )
    # Remove file block
    unlink(block_files)
    # Return probability vector tile
    return(probs_tile)
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
    # Get cloud values (NULL if not exists)
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
                # Use C_normalize_data_0 to process old version of normalization
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
        return(as.data.frame(values))
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
        return(as.data.frame(values_base))
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
    if (length(class_info[["dates_index"]][[1]]) > 1) {
        splitted <- .samples_split(
            samples = samples,
            split_intervals = class_info[["dates_index"]][[1]]
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
    if (.torch_cuda_enabled(ml_model) || .torch_mps_enabled(ml_model))
        prediction <- .classify_ts_gpu(
            pred = pred,
            ml_model = ml_model,
            gpu_memory = gpu_memory)
    else
        prediction <- .classify_ts_cpu(
            pred = pred,
            ml_model = ml_model,
            multicores = multicores,
            progress = progress
        )
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
#' @title Classify predictors using CPU
#' @name .classify_ts_cpu
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
        # Get predictors of a given partition
        pred_part <- .pred_part(part)
        # Get predictors features to classify
        values <- .pred_features(pred_part)
        # Classify
        values <- ml_model(values)
        # Return classification
        values <- tibble::as_tibble(values)
        values
    }, progress = progress)

    return(prediction)
}
#' @title Classify predictors using GPU
#' @name .classify_ts_gpu
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
    pred_size <- nrow(pred) * ncol(pred) * 4 / 1e+09
    # include processing bloat
    pred_size <- pred_size * .conf("processing_bloat_gpu")
    # estimate how should we partition the predictors
    num_parts <- ceiling(pred_size / gpu_memory)
    # Divide samples predictors in chunks to parallel processing
    parts <- .pred_create_partition(
        pred = pred,
        partitions = num_parts
    )
    prediction <- slider::slide_dfr(parts, function(part) {
        # Get predictors of a given partition
        pred_part <- .pred_part(part)
        # Get predictors features to classify
        values <- .pred_features(pred_part)
        # Classify
        values <- ml_model(values)
        # Return classification
        values <- tibble::tibble(data.frame(values))
        # Clean GPU memory
        .ml_gpu_clean(ml_model)
        return(values)
    })
    return(prediction)
}
#' @title Start recording processing time
#' @name .classify_verbose_start
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Prints the block size and computes
#' start time for processing
#'
#' @param  verbose TRUE/FALSE
#' @param  block   block size
#' @return start time for processing
.classify_verbose_start <- function(verbose, block) {
    start_time <- Sys.time()
    if (verbose) {
        msg <- paste0(.conf("messages", ".verbose_block_size"), " ",
                      .nrows(block), " x ", .ncols(block))
        message(msg)
    }
    return(start_time)
}
#' @title End recording processing time
#' @name .classify_verbose_end
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Prints the processing time
#' @param  verbose TRUE/FALSE
#' @param  start_time   initial processing time
#' @return elapsed processing time for processing
.classify_verbose_end <- function(verbose, start_time) {
    if (verbose) {
        end_time <- Sys.time()
        message("")
        message(.conf("messages", ".verbose_task_end"), end_time)
        message(.conf("messages", ".verbose_task_elapsed"),
                format(round(end_time - start_time, digits = 2))
        )
    }
}
