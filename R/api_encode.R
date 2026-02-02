#' @title encode a chunk of raster data  using multicores
#' @name .encode_tile
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description Uses a pre-trained sits deep-learning model to encode a block of data using multicores,
#' breaking the data into blocks and divides them between the available cores. The size of the blocks is optimized
#'  to account for COG files and for the balance of multicores and memory size.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the encoded images for each corresponding year.
#'
#' @param  tile            Single tile of a data cube.
#' @param  out_band        Band to be produced.
#' @param  bands           Bands to extract time series
#' @param  base_bands      Base bands to extract values
#' @param  dl_model        Encoder trained by \code{\link[sits]{sits_pre_train}}.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @param  impute_fn       Imputation function.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @param  verbose         Print processing information?
#' @param  progress        Show progress bar?
#' @return List of the encoded raster layers.
.encode_tile <- function(tile,
                         out_bands,
                         bands,
                         base_bands,
                         dl_model,
                         block,
                         roi,
                         filter_fn,
                         impute_fn,
                         output_dir,
                         version,
                         verbose,
                         progress) {
    # Define the name of the output file
    out_files <- .file_eo_name(
        tile = tile,
        band = out_bands,
        date = .tile_start_date(tile),
        output_dir = output_dir
    )
    # If output file exists, builds a
    # embeddings cube directly from the file
    # and does not reprocess input
    if (all(file.exists(out_files))) {
        .check_recovery()
        embedding_tile <- .tile_eo_from_files(
            files = out_files,
            fid   = .fi_fid(.fi(tile)),
            bands  = out_bands,
            date = .tile_start_date(tile),
            base_tile = tile,
            update_bbox = FALSE
        )
        return(embedding_tile)
    }
    # Initial time for tile embedding
    tile_start_time <- .tile_encode_start(
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
    block_files <- .jobs_map_parallel(chunks, function(chunk) {
        # Retrive block to be processed
        block <- .block(chunk)
        # Create a temporary block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_files),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values from files
        values <- .encode_data_read(
            tile = tile,
            block = block,
            bands = bands,
            base_bands = base_bands,
            dl_model = dl_model,
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
            event = "start_block_data_encoding",
            key = "model",
            value = .ml_class(dl_model)
        )
        # Apply the encoder model to values
        # Uses the closure created by sits_pre_train
        values <-  dl_model(values)

        # Are the results consistent with the data input?
        .check_processed_values(
            values = values,
            input_pixels = input_pixels
        )
        # Log end of block
        .debug_log(
            event = "end_block_data_encoding",
            key = "model",
            value = .ml_class(dl_model)
        )
        # Obtain configuration parameters for embeddings cube
        band_conf <- .conf("default_values", "INT2S")

        # Apply scaling to encoded values
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
    # Merge blocks into a new embeddings_cube tile
    # If ROI exists, blocks are merged to a different directory
    # than output_dir, which is used to save the final cropped version
    merge_out_file <- out_files
    if (.has(roi)) {
        merge_out_file <- .file_eo_name(
            tile = tile,
            band = out_bands,
            date = .tile_start_date(tile),
            output_dir = file.path(output_dir, ".sits")
        )
    }

    # Obtain configuration parameters for embeddings cube
    band_conf <- .conf("default_values", "INT2S")

    embedding_tile <- .tile_eo_merge_blocks(
        files = merge_out_file,
        bands = out_bands,
        band_conf = band_conf,
        base_tile = tile,
        block_files = block_files,
        multicores = .jobs_multicores(),
        update_bbox = update_bbox
    )
    # Clean GPU memory allocation
    .ml_gpu_clean(dl_model)
    # if there is a ROI, crop the embeddings cube
    if (.has(roi)) {
        embedding_tile_crop <- .crop(
            cube = embedding_tile,
            roi = roi,
            output_dir = output_dir,
            multicores = 1L,
            progress = progress
        )
        unlink(.fi_paths(.fi(embedding_tile)))
    }
    # show final time for embedding
    .tile_encode_end(
        tile = tile,
        start_time = tile_start_time,
        verbose = verbose
    )
    # Return encoded tile (cropped version in case of ROI)
    if (.has(roi)) {
        embedding_tile_crop
    } else {
        embedding_tile
    }
}

#' @title Read a block of values from a set of raster images
#' @name  .encode_data_read
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @param  tile            Input tile to read data.
#' @param  block           Bounding box in (col, row, ncols, nrows).
#' @param  bands           Bands to extract time series
#' @param  base_bands      Base bands to extract values
#' @param  dl_model        Encoder trained by \code{\link[sits]{sits_pre_train}}.
#' @param  impute_fn       Imputation function
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @return A matrix with values for embedding.
.encode_data_read <- function(tile, block, bands, base_bands,
                              dl_model, impute_fn, filter_fn) {
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
    if (.has(dl_model)) {
        colnames(values) <- .ml_features_name(dl_model)
    }
    # Return values
    values
}
#' @title encode a sits tibble using machine learning models
#' @name .encode_ts
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description Apply the pre-trained encoder model to a time series and returns the embeddings.
#'
#' @param  samples    Tibble with sits samples
#' @param  dl_model   Encoder trained by \code{\link[sits]{sits_pre_train}}.
#' @param  filter_fn  Smoothing filter to be applied (if desired).
#' @param  impute_fn  Imputation function (to remove NA)
#' @param  multicores number of threads to process the time series.
#' @param  gpu_memory Memory available in GPU
#' @param  progress   Show progress bar?
#' @return A tibble with the encoded time series.
.encode_ts <- function(samples,
                       dl_model,
                       filter_fn,
                       impute_fn,
                       multicores,
                       gpu_memory,
                       progress) {
    # Start parallel workers
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Get bands from model
    bands <- .ml_bands(dl_model)
    # Update samples bands order
    if (length(bands) != length(.samples_bands(samples))) {
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
    # Compute the breaks in time for multiyear embedding
    class_info <- .timeline_class_info(
        data = samples,
        samples = .ml_samples(dl_model)
    )
    # Split long time series of samples in a set of small time series
    if (length(class_info[["dates_index"]][[1L]]) > 1L) {
        splitted <- .samples_split(
            samples = samples,
            split_intervals = class_info[["dates_index"]][[1L]]
        )
        pred <- .predictors(
            samples = splitted,
            ml_model = dl_model
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
            ml_model = dl_model
        )
    }
    # choose between GPU and CPU
    if (.torch_gpu_classification() && .ml_is_torch_model(dl_model)) {
        prediction <- .encode_ts_gpu(
            pred = pred,
            dl_model = dl_model,
            gpu_memory = gpu_memory
        )
    } else {
        prediction <- .encode_ts_cpu(
            pred = pred,
            dl_model = dl_model,
            multicores = multicores,
            progress = progress
        )
    }

    # Obtain configuration parameters for embeddings cube
    band_conf <- .conf("default_values", "INT2S")

    # Apply scaling to encoded values
    band_scale <- .scale(band_conf)
    prediction <- as.matrix(prediction) / band_scale
    storage.mode(prediction) <- "integer"
    prediction <- prediction * band_scale

    # Store the result in the input data
    prediction <- .tibble_embedding(
        data = samples,
        embeddings = prediction
    )

    # Set result class and return it
    prediction <- .set_class(
        x = prediction, "embeddings",
        class(samples)
    )
    prediction
}
#' @title encode predictors using CPU
#' @name .encode_ts_cpu
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description Apply the pre-trained encoder model to a time series and returns the embeddings.
#'
#' @param  pred       a tibble with predictors
#' @param  dl_model   Encoder trained by \code{\link[sits]{sits_pre_train}}.
#' @param  multicores number of threads to process the time series.
#' @param  progress   Show progress bar?
#' @return A tibble with the encoded values.
.encode_ts_cpu <- function(pred,
                           dl_model,
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
        # Extract features to encode
        # encode, normalize and calibrate value
        values <- part |>
            .pred_part() |>
            .pred_features() |>
            dl_model()
            #.ml_normalize(dl_model)
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
#' @title encode predictors using GPU
#' @name .encode_ts_gpu
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description Apply the pre-trained encoder model to a time series and returns the embeddings.
#' @param  pred       a tibble with predictors
#' @param  dl_model   Encoder trained by \code{\link[sits]{sits_pre_train}}.
#' @param  gpu_memory memory available in GPU
#' @return A tibble with the encoded values.
.encode_ts_gpu <- function(pred,
                           dl_model,
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
        # Get predictors features to encode
        # encode
        # normalize and calibrate values
        values <- part |>
            .pred_part() |>
            .pred_features() |>
            dl_model()
            #.ml_normalize(dl_model)
        # Extract columns
        values_columns <- colnames(values)
        # Transform embedding results
        values <- tibble::tibble(as.data.frame(values))
        # Fix column names to avoid errors with non-standard column name
        # (e.g., with spaces, icons)
        colnames(values) <- values_columns
        # Clean GPU memory
        .ml_gpu_clean(dl_model)
        values
    })
    prediction
}
#' @title Start recording processing time
#' @name .encode_verbose_start
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @description Prints the block size and computes
#' start time for processing
#'
#' @param  verbose TRUE/FALSE
#' @param  block   block size
#' @return start time for processing
.encode_verbose_start <- function(verbose, block) {
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
#' @name .encode_verbose_end
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @description Prints the processing time
#' @param  verbose TRUE/FALSE
#' @param  start_time   initial processing time
#' @return elapsed processing time for processing
.encode_verbose_end <- function(verbose, start_time) {
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

.encode_band_names <- function(dl_model, bands_prefix) {
    paste0(bands_prefix, seq_len(environment(dl_model)[["embedding_dim"]]))
}
