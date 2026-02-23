#' @title Encode a chunk of raster data using multicores
#'
#' @description
#' Uses a pre-trained \pkg{sits} deep-learning encoder to encode a single
#' data-cube tile in parallel. The tile is partitioned into optimized blocks
#' and distributed across the available CPU cores, balancing I/O efficiency
#' (including Cloud-Optimized GeoTIFF access patterns) and memory usage.
#'
#' Each block is read, optionally smoothed and imputed, encoded by the
#' provided model, scaled to the configured output type, and written as a
#' temporary raster block. After all blocks finish, the function merges
#' the blocks into the final encoded raster layers for the tile date. When
#' a region of interest is provided, blocks are spatially filtered and the
#' final result is cropped to the ROI.
#'
#' If all expected output files already exist, the function performs a
#' recovery path: it validates the outputs and rebuilds the encoded tile
#' directly from the files without re-encoding the input data.
#'
#' @param tile Single tile of a data cube.
#' @param out_bands Character vector with the output band names to be
#'   produced by the encoder.
#' @param bands Character vector with the input bands used to build the
#'   time series for encoding.
#' @param base_bands Character vector with the base bands used to extract
#'   values from the input tile (e.g., reference bands required by the
#'   cube layout).
#' @param dl_model Encoder trained by \code{\link[sits]{sits_pre_train}}.
#'   The object must be callable on a matrix of pixels and return an
#'   encoded representation per pixel.
#' @param block Optimized block specification used to read data into
#'   memory and define chunk sizes.
#' @param roi Optional region of interest used to filter chunks and crop
#'   the final output. When provided, only blocks intersecting the ROI are
#'   processed, and the resulting tile may have an updated bounding box.
#' @param filter_fn Optional smoothing filter function applied during
#'   preprocessing of the input time series.
#' @param impute_fn Optional imputation function used to fill missing
#'   values during preprocessing.
#' @param output_dir Output directory where encoded rasters will be saved.
#' @param verbose Logical. If \code{TRUE}, print processing information.
#' @param progress Logical. If \code{TRUE}, show a progress bar while
#'   processing blocks in parallel.
#'
#' @return
#' Encoded tile as a \pkg{sits} cube tile object built from the output
#' raster layers. If \code{roi} is provided, returns the cropped version.
#'
#' @details
#' Parallel processing is performed at the chunk level, with one job per
#' block. For each block, the function reads and preprocesses pixel time
#' series, builds a missing-data mask, encodes values with \code{dl_model},
#' restores missing values, and writes the result to a temporary block
#' raster. Block rasters are then merged into the final tile rasters.
#'
#' The output scaling and data type are driven by the internal band
#' configuration used for embedding cubes. GPU allocations associated with
#' \code{dl_model} are cleaned after processing.
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @keywords internal
#' @noRd
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
            fid = .fi_fid(.fi(tile)),
            bands = out_bands,
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
        if (all(.raster_is_valid(block_file))) {
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
        values <- dl_model(values)

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
#' @name .encode_data_read
#'
#' @description
#' Reads a spatial block from the input tile, assembling the per-pixel
#' time-series features required by an encoder. For each requested
#' time-series band, the function:
#' \itemize{
#'   \item reads the block values;
#'   \item applies an optional cloud mask (setting masked pixels to
#'   \code{NA});
#'   \item imputes missing values using \code{impute_fn}; and
#'   \item optionally smooths the time series using \code{filter_fn}.
#' }
#'
#' In addition, the function reads the requested \code{base_bands} from
#' the base-tile information (e.g., static/reference layers) and appends
#' them to the feature set. The resulting features are column-bound into
#' a single numeric matrix suitable as input to the encoder model.
#'
#' @param tile Input tile to read data.
#' @param block Bounding box describing the block to read, typically in
#'   \code{(col, row, ncols, nrows)}.
#' @param bands Character vector with the time-series bands to read and
#'   preprocess.
#' @param base_bands Character vector with base bands to read from the
#'   base-tile information and append to the feature set.
#' @param dl_model Encoder trained by \code{\link[sits]{sits_pre_train}}.
#'   When provided, the output matrix columns are named using the model's
#'   expected feature names.
#' @param impute_fn Function used to impute missing values in each band
#'   after cloud masking. It must accept the band block values and return
#'   an object coercible to a \code{data.frame} with the same shape.
#' @param filter_fn Optional smoothing filter function applied after
#'   imputation. If \code{NULL} (or not set), no filtering is performed.
#'
#' @return
#' A numeric matrix with one row per pixel in the block and one column per
#' feature assembled from \code{bands} and \code{base_bands}. If
#' \code{dl_model} is provided, column names are set to match the
#' encoder's expected feature names.
#'
#' @details
#' For tiles backed by expiring credentials (e.g., MPC cubes), the tile is
#' first passed through an internal token generator before reading.
#'
#' Cloud masking is applied when cloud data is available for the tile; in
#' that case, masked pixels are set to \code{NA} prior to imputation and
#' filtering. Band and base-band blocks are read independently and then
#' combined using column binding with universal name repair.
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @keywords internal
#' @noRd
.encode_data_read <- function(tile,
                              block,
                              bands,
                              base_bands,
                              dl_model,
                              impute_fn,
                              filter_fn) {
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
#' @title Encode a sits tibble using machine learning models
#' @name .encode_ts
#'
#' @description
#' Applies a pre-trained encoder model to a \pkg{sits} samples tibble and
#' returns embeddings for each input time series. The function aligns the
#' input band order with the model requirements, optionally smooths and
#' imputes the time series, and then runs the encoder on either GPU or
#' CPU depending on the runtime capabilities and model type.
#'
#' For multi-year time series, the function can split long series into
#' smaller intervals according to the model training timeline metadata,
#' generate predictors for each interval, and encode them before
#' reassembling outputs into an embeddings tibble.
#'
#' @param samples Tibble with \pkg{sits} samples.
#' @param dl_model Encoder trained by \code{\link[sits]{sits_pre_train}}.
#'   The model must be compatible with \code{samples} and callable on the
#'   predictor matrix produced by the internal preprocessing steps.
#' @param filter_fn Optional smoothing function applied across time to
#'   each sample prior to encoding. If \code{NULL} (or not set), no
#'   filtering is performed.
#' @param impute_fn Optional imputation function applied across time to
#'   each sample prior to encoding, typically to remove \code{NA} values.
#'   If \code{NULL} (or not set), no imputation is performed.
#' @param multicores Integer. Number of parallel workers/threads used for
#'   CPU encoding and for preprocessing steps that leverage parallel
#'   execution.
#' @param gpu_memory Numeric. Amount of GPU memory available for encoding
#'   when using a torch-based model on GPU.
#' @param progress Logical. If \code{TRUE}, show a progress bar during CPU
#'   encoding.
#'
#' @return
#' A tibble with the encoded time series (embeddings). The returned object
#' keeps the original sample metadata and is assigned the internal
#' embeddings class.
#'
#' @details
#' The function starts a parallel backend using \code{multicores} and
#' stops it on exit. It ensures that the input band order matches the
#' model band order. Optional preprocessing (\code{filter_fn} and
#' \code{impute_fn}) is applied across the time dimension of each sample.
#'
#' Predictor matrices are built from the samples and passed through the
#' encoder. If GPU execution is available and \code{dl_model} is a torch
#' model, encoding is executed on GPU; otherwise, it runs on CPU with
#' optional parallelism and progress reporting.
#'
#' Encoded values are scaled according to the internal configuration used
#' by embedding cubes and then stored back into a tibble aligned with the
#' input samples.
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @keywords internal
#' @noRd
.encode_ts <- function(samples,
                       dl_model,
                       filter_fn,
                       impute_fn,
                       multicores,
                       gpu_memory,
                       progress) {
    # Prepare parallel processing
    if (.parallel_start(workers = multicores)) {
        on.exit(.parallel_stop(), add = TRUE)
    }
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
#' @title Encode predictors using CPU
#'
#' @description
#' Encodes predictor data using a pre-trained model with CPU-based
#' parallel processing. Predictors are split into partitions and encoded
#' concurrently across multiple cores.
#'
#' @param pred Tibble with predictor data derived from \pkg{sits} samples.
#' @param dl_model Encoder trained by
#'   \code{\link[sits]{sits_pre_train}}.
#' @param multicores Integer. Number of CPU threads used for parallel
#'   encoding.
#' @param progress Logical. If \code{TRUE}, show a progress bar.
#'
#' @return
#' A tibble with encoded predictor values (embeddings).
#'
#' @keywords internal
#' @noRd
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
        # .ml_normalize(dl_model)
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
#' @title Encode predictors using GPU
#'
#' @description
#' Encodes predictor data using a pre-trained model on GPU. Predictors are
#' processed in sequential partitions sized to fit the available GPU
#' memory and combined into a single embeddings tibble.
#'
#' @param pred Tibble with predictor data derived from \pkg{sits} samples.
#' @param dl_model Encoder trained by
#'   \code{\link[sits]{sits_pre_train}}.
#' @param gpu_memory Numeric. Available GPU memory (in GB) used to size
#'   predictor partitions.
#'
#' @return
#' A tibble with encoded predictor values (embeddings).
#'
#' @keywords internal
#' @noRd
.encode_ts_gpu <- function(pred, dl_model, gpu_memory) {
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
        # .ml_normalize(dl_model)
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
#'
#' @description
#' Optionally prints the block size and records the start time of a
#' processing step.
#'
#' @param verbose Logical. If \code{TRUE}, print block size information.
#' @param block Block specification used to report its dimensions.
#'
#' @return
#' A \code{POSIXct} timestamp marking the start of processing.
#'
#' @keywords internal
#' @noRd
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
#'
#' @description
#' Optionally prints the end time and the elapsed processing time since a
#' previously recorded start time.
#'
#' @param verbose Logical. If \code{TRUE}, print timing information.
#' @param start_time Initial processing time as returned by
#'   \code{.encode_verbose_start()}.
#'
#' @return
#' The elapsed processing time, invisibly.
#'
#' @keywords internal
#' @noRd
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
#' @title Generate embedding band names
#'
#' @description
#' Builds a sequence of embedding band names using a prefix and the
#' embedding dimension defined in the encoder model.
#'
#' @param dl_model Encoder model containing the embedding dimension.
#'
#' @return
#' A character vector with embedding band names.
#'
#' @keywords internal
#' @noRd
.encode_band_names <- function(dl_model) {
    bands_prefix <- environment(dl_model)[["bands_prefix"]]
    embedding_dim <- seq_len(environment(dl_model)[["embedding_dim"]])
    paste0(bands_prefix, embedding_dim)
}
