#' @title Jobs of the CPU classification of one tile
#' @name .classify_tile_jobs
#' @keywords internal
#' @noRd
#' @param  tile            Single tile of a data cube.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  exclusion_mask  Area to be excluded from classification
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @return Tibble with the columns role, out_file and tile: the blocks,
#'         with the chunk columns and block_file, and then the merge, with
#'         block_files and update_bbox; or one resume job when the probs
#'         file of the tile exists
.classify_tile_jobs <- function(tile, block, roi, exclusion_mask, ml_model,
                                output_dir, version) {
    out_file <- .classify_out_file(
        tile = tile, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # the recovery message is shown by this process, not by a worker
        .check_recovery()
        return(tibble::tibble(
            role = "resume", out_file = out_file,
            tile = list(.classify_tile_from_files(out_file, tile, ml_model))
        ))
    }
    plan <- .classify_tile_chunks(
        tile = tile,
        out_file = out_file,
        block = block,
        roi = roi,
        exclusion_mask = exclusion_mask,
        output_dir = output_dir
    )
    chunks <- plan[["chunks"]]
    # sfc masks of tiles in other CRSs cannot be stacked, so a list holds them
    if (.has(chunks[["mask"]])) {
        chunks[["mask"]] <- purrr::map(
            seq_along(chunks[["mask"]]), function(i) chunks[["mask"]][i]
        )
    }
    blocks <- NULL
    new_files <- character(0L)
    # a tile with every block recovered has only the merge
    if (nrow(chunks) > 0L) {
        blocks <- .chunks_jobs_create(
            chunks = chunks, out_file = out_file, output_dir = output_dir
        )
        blocks[["tile"]] <- list(tile)
        new_files <- blocks[["block_file"]]
    }
    merge <- tibble::tibble(
        role = "merge", out_file = out_file, tile = list(tile),
        block_files = list(c(plan[["recovered_files"]], new_files)),
        update_bbox = plan[["update_bbox"]]
    )
    dplyr::bind_rows(blocks, merge)
}
#' @title Job of the CPU classification: one block, the merge or the resume
#' @name .classify_job
#' @keywords internal
#' @noRd
#' @param  job            Row of the jobs of \code{.classify_tile_jobs()}
#' @param  out_band       Band to be produced.
#' @param  bands          Bands to extract time series
#' @param  base_bands     Base bands to extract values
#' @param  band_conf      Band configuration
#' @param  ml_model       Model closure (sequential processing only); in
#'                        a worker it is NULL and comes from the global
#'                        environment
#' @param  impute_fn      Imputation function.
#' @param  roi            Region of interest.
#' @param  output_dir     Output directory.
#' @param  version        Version of result.
#' @param  merge_threads  Threads of the merge
#' @return File of the block, or the probs tile
.classify_job <- function(job, out_band, bands, base_bands, band_conf,
                          ml_model, impute_fn, roi, output_dir, version,
                          merge_threads) {
    tile <- job[["tile"]][[1L]]
    # Get exported ml_model (parallel processing)
    if (is.null(ml_model)) {
        ml_model <- get("ml_model", envir = globalenv())
    }
    switch(job[["role"]],
        # the row of a block job has the columns of its chunk
        block = .classify_chunk_cpu(
            chunk = job, tile = tile, base_bands = base_bands,
            bands = bands, band_conf = band_conf, impute_fn = impute_fn,
            output_dir = output_dir, out_file = job[["out_file"]],
            ml_model = ml_model
        ),
        merge = .classify_merge(
            tile = tile, out_file = job[["out_file"]],
            block_files = job[["block_files"]][[1L]], ml_model = ml_model,
            out_band = out_band, version = version, roi = roi,
            update_bbox = job[["update_bbox"]], output_dir = output_dir,
            # the queue takes the progress argument itself
            multicores = merge_threads, progress = FALSE
        ),
        resume = tile
    )
}
#' @title Classify the tiles of a cube in CPU through one queue
#' @name .classify_tiles_cpu
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description One queue of jobs over every tile, run by
#' \code{.jobs_map_queue()}: the blocks of each tile, in tile order, and
#' the merge of a tile, which waits for its blocks and goes before the
#' blocks of the next tiles. So a worker merges a tile while the others
#' classify the blocks of the next tile. The elapsed time shown for a tile
#' overlaps the time of the next tile. Existing probs files and valid block
#' files are not computed again.
#'
#' @param  tiles           Data cube with the tiles to classify.
#' @param  out_band        Band to be produced.
#' @param  bands           Bands to extract time series
#' @param  base_bands      Base bands to extract values
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  exclusion_mask  Area to be excluded from classification
#' @param  impute_fn       Imputation function.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @param  verbose         Print processing information?
#' @param  progress        Show progress bar?
#' @return Cube with the probs tile of each tile.
.classify_tiles_cpu <- function(tiles,
                                out_band,
                                bands,
                                base_bands,
                                ml_model,
                                block,
                                roi,
                                exclusion_mask,
                                impute_fn,
                                output_dir,
                                version,
                                verbose,
                                progress) {
    # Obtain configuration parameters for probability cube
    band_conf <- .conf_derived_band(
        derived_class = "probs_cube",
        band = out_band
    )
    jobs <- .cube_jobs_create(
        tiles, .classify_tile_jobs, block = block, roi = roi,
        exclusion_mask = exclusion_mask, ml_model = ml_model,
        output_dir = output_dir, version = version
    )
    # start time of each tile, read when the tile ends
    start_times <- new.env(parent = emptyenv())
    on_start <- function(job) {
        .debug_log(
            event = "start_tile", key = "file", value = job[["out_file"]]
        )
        # a resumed tile has no blocks and no message
        if (job[["role"]] != "resume") {
            .debug_log(
                event = "start_tile_blocks", key = "file",
                value = job[["out_file"]]
            )
            start_times[[job[["group"]]]] <- .tile_classif_start(
                tile = job[["tile"]][[1L]], verbose = verbose
            )
        }
    }
    on_last <- function(job) {
        # a merge is sent once the blocks of its tile are done
        if (job[["role"]] == "merge") {
            .debug_log(
                event = "end_tile_blocks", key = "file",
                value = job[["out_file"]]
            )
        }
    }
    on_end <- function(job, value) {
        .debug_log(event = "end_tile", key = "file", value = job[["out_file"]])
        if (job[["role"]] != "resume") {
            .tile_classif_end(
                tile = job[["tile"]][[1L]],
                start_time = start_times[[job[["group"]]]],
                verbose = verbose
            )
        }
    }
    # In parallel processing the model was exported once to the workers'
    # global environment; in sequential processing it is passed directly
    probs_tiles <- .jobs_map_queue(
        jobs, fn = .classify_job, out_band = out_band, bands = bands,
        base_bands = base_bands, band_conf = band_conf,
        ml_model = if (.parallel_is_open()) NULL else ml_model,
        impute_fn = impute_fn, roi = roi, output_dir = output_dir,
        version = version,
        # threads of the merge, as when the main process merged
        merge_threads = .jobs_multicores(),
        on_start = on_start, on_last = on_last, on_end = on_end,
        progress = progress
    )
    dplyr::bind_rows(probs_tiles)
}
#' @title Classify the tiles of a cube in GPU with one dataloader
#' @name .classify_tiles_gpu
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Classifies all tiles with one torch dataloader, so its
#' workers start once per call. A tile is merged after its last block is
#' written. Existing probs files and valid block files are not computed
#' again.
#'
#' @param  tiles           Data cube with the tiles to classify.
#' @param  out_files       Probs file of each tile.
#' @param  out_band        Band to be produced.
#' @param  bands           Bands to extract time series
#' @param  base_bands      Base bands to extract values
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  exclusion_mask  Area to be excluded from classification
#' @param  impute_fn       Imputation function.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @param  verbose         Print processing information?
#' @param  progress        Show progress bar?
#' @return List with the probs tile of each tile.
.classify_tiles_gpu <- function(tiles,
                                out_files,
                                out_band,
                                bands,
                                base_bands,
                                ml_model,
                                block,
                                roi,
                                exclusion_mask,
                                impute_fn,
                                output_dir,
                                version,
                                verbose,
                                progress) {
    done <- file.exists(out_files)
    probs_tiles <- slider::slide2(tiles, out_files, function(tile, out_file) {
        if (!file.exists(out_file)) {
            return(NULL)
        }
        .check_recovery()
        .classify_tile_from_files(out_file, tile, ml_model)
    })
    plans <- slider::slide2(tiles, out_files, function(tile, out_file) {
        if (file.exists(out_file)) {
            return(NULL)
        }
        .classify_tile_chunks(
            tile = tile,
            out_file = out_file,
            block = block,
            roi = roi,
            exclusion_mask = exclusion_mask,
            output_dir = output_dir
        )
    })
    block_files <- purrr::map(plans, function(plan) {
        plan[["recovered_files"]]
    })
    n_pending <- purrr::map_int(plans, function(plan) {
        if (is.null(plan)) 0L else nrow(plan[["chunks"]])
    })
    # Tiles merge in cube order, as on the CPU path
    to_merge <- which(!done)
    # Tiles overlap in the dataloader, so a tile's time starts at the last merge
    tile_start_time <- NULL
    merge_ready <- function() {
        while (length(to_merge) > 0L) {
            i <- to_merge[[1L]]
            tile <- tiles[i, ]
            if (is.null(tile_start_time)) {
                tile_start_time <<- .tile_classif_start(tile, verbose)
            }
            if (n_pending[[i]] > 0L) {
                return(invisible(NULL))
            }
            probs_tiles[[i]] <<- .classify_merge(
                tile = tile,
                out_file = out_files[[i]],
                block_files = block_files[[i]],
                ml_model = ml_model,
                out_band = out_band,
                version = version,
                roi = roi,
                update_bbox = plans[[i]][["update_bbox"]],
                output_dir = output_dir,
                multicores = .jobs_multicores(),
                progress = progress
            )
            .tile_classif_end(
                tile = tile,
                start_time = tile_start_time,
                verbose = verbose
            )
            tile_start_time <<- NULL
            to_merge <<- to_merge[-1L]
            gc()
        }
        invisible(NULL)
    }
    # Called by the callback after it writes a block of tile i
    on_block <- function(i, files) {
        block_files[[i]] <<- c(block_files[[i]], files)
        n_pending[[i]] <<- n_pending[[i]] - 1L
        merge_ready()
    }
    # Tiles at the head with all blocks recovered are merged now
    merge_ready()
    if (sum(n_pending) > 0L) {
        # The callback writes blocks without the crop mask, whose sf
        # geometries differ in CRS among tiles
        chunks <- .map_dfr(which(n_pending > 0L), function(i) {
            chunks <- plans[[i]][["chunks"]]
            chunks[["mask"]] <- NULL
            chunks[["tile_index"]] <- i
            chunks
        })
        # Build the chunk dataset
        dataset <- .torch_chunks_dataset(
            chunks = chunks,
            tile = tiles,
            read_fn = .classify_data_read,
            bands = bands,
            base_bands = base_bands,
            stats = .ml_stats(ml_model),
            ml_features_name = .ml_features_name(ml_model),
            ml_temporal_model = .ml_torch_is_temporal(ml_model),
            impute_fn = impute_fn,
            verbose = verbose,
            output_dir = output_dir
        )
        # Get band configuration
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube",
            band = out_band
        )
        # Define post-process callback
        # This callback reconstructs + writes each block in its tile
        callback <- .callback_post_classify(
            chunks = chunks,
            output_dir = output_dir,
            out_file = out_files,
            out_band = out_band,
            band_conf = band_conf,
            ml_labels = .ml_labels(ml_model),
            crs = slider::slide_chr(tiles, .tile_crs),
            on_block = on_block
        )
        # Classify!
        new_files <- unlist(ml_model(
            list(dataset = dataset, callback = callback)
        ))
        # Log end of chunk group save
        .debug_log(
            event = "end_chunk_group_save",
            key = "n_blocks",
            value = length(new_files)
        )
    }
    # Every block was written, so every tile was merged
    .check_that(
        length(to_merge) == 0L,
        msg = .conf("messages", ".classify_tiles_gpu")
    )
    probs_tiles
}
#' @title Chunks of a tile to classify
#' @name .classify_tile_chunks
#' @keywords internal
#' @noRd
#' @param  tile            Single tile of a data cube.
#' @param  out_file        Probs file of the tile.
#' @param  block           Optimized block to be read into memory.
#' @param  roi             Region of interest.
#' @param  exclusion_mask  Area to be excluded from classification
#' @param  output_dir      Output directory.
#' @return List with the pending chunks, the recovered block files and
#'   update_bbox.
.classify_tile_chunks <- function(tile, out_file, block, roi,
                                  exclusion_mask, output_dir) {
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
    # Regenerate all block files
    block_files_all <- slider::slide_chr(chunks, function(chunk) {
        .classify_block_file(
            out_file = out_file,
            block = .block(chunk),
            output_dir = output_dir
        )
    })
    # Define which blocks are already processed
    recovered <- purrr::map_lgl(block_files_all, function(f) {
        all(.raster_is_valid(f))
    })
    list(
        chunks = chunks[!recovered, , drop = FALSE],
        recovered_files = block_files_all[recovered],
        update_bbox = update_bbox
    )
}
#' @title Probs file of a tile
#' @name .classify_out_file
#' @keywords internal
#' @noRd
.classify_out_file <- function(tile, version, output_dir) {
    .file_derived_name(
        tile = tile, band = "probs", version = version,
        output_dir = output_dir
    )
}
#' @title Block file of a probs file
#' @name .classify_block_file
#' @keywords internal
#' @noRd
#' @description The writers and the recovery check name a block here, so
#'   a written block is always found again.
.classify_block_file <- function(out_file, block, output_dir) {
    .file_block_name(
        pattern = .file_pattern(out_file),
        block = block,
        output_dir = output_dir
    )
}
#' @title Probs tile from its file
#' @name .classify_tile_from_files
#' @keywords internal
#' @noRd
.classify_tile_from_files <- function(out_file, tile, ml_model) {
    .tile_derived_from_file(
        file = out_file,
        band = "probs",
        base_tile = tile,
        labels = .ml_labels_code(ml_model),
        derived_class = "probs_cube",
        update_bbox = TRUE
    )
}
#' @title Merge the blocks of a tile into its probs file
#' @name .classify_merge
#' @keywords internal
#' @noRd
#' @description With a ROI, the merged file goes to .sits and is cropped
#'   into output_dir. The threads of the merge are an argument, since
#'   .jobs_multicores() is 0 in a worker.
.classify_merge <- function(tile, out_file, block_files, ml_model, out_band,
                            version, roi, update_bbox, output_dir,
                            multicores, progress) {
    merge_out_file <- out_file
    if (.has(roi)) {
        merge_out_file <- .file_derived_name(
            tile = tile,
            band = out_band,
            version = version,
            output_dir = file.path(output_dir, ".sits")
        )
    }
    # a merge sent again after a dead worker finds its leftovers
    .file_merge_clean(merge_out_file)
    unlink(out_file)
    .debug_log(event = "start_tile_merge", key = "file", value = out_file)
    probs_tile <- .tile_derived_merge_blocks(
        file = merge_out_file,
        band = out_band,
        labels = .ml_labels_code(ml_model),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = multicores,
        update_bbox = update_bbox,
        # with a ROI the blocks stay until the crop is done, for a resend
        keep_blocks = .has(roi)
    )
    .debug_log(event = "end_tile_merge", key = "file", value = out_file)
    if (!.has(roi)) {
        return(probs_tile)
    }
    # .crop() starts no cluster here, but resets the debug flag
    log_flag <- .debug()
    log_dir <- sits_env[["output_dir"]]
    probs_tile_crop <- .crop(
        cube = probs_tile,
        roi = roi,
        output_dir = output_dir,
        multicores = 1L,
        progress = progress
    )
    .debug(flag = log_flag, output_dir = log_dir)
    unlink(.fi_paths(.fi(probs_tile)))
    unlink(block_files)
    probs_tile_crop
}
.classify_write_block <- function(values, block, output_dir, out_file, out_band) {
    # Get block geometry
    block_geom <- .block(block)
    block_file <- .classify_block_file(
        out_file = out_file,
        block = block_geom,
        output_dir = output_dir
    )
    # Resume processing in case of failure
    if (all(.raster_is_valid(block_file))) {
        return(block_file)
    }
    # Obtain configuration parameters for probability cube
    band_conf <- .conf_derived_band(
        derived_class = "probs_cube",
        band = out_band
    )
    # Log start of block saving
    .debug_log(
        event = "start_block_data_save",
        key = "file",
        value = block_file
    )
    # Prepare and save results as raster
    .raster_write_block(
        files = block_file,
        block = block_geom,
        bbox = .bbox(block),
        values = values,
        data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf),
        crop_block = block[["mask"]]
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
#' @param  ml_features_name Names of features used in model trained by
#'                         \code{\link[sits]{sits_train}}.
#' @param  impute_fn       Imputation function
#' @return A matrix with values for classification.
.classify_data_read <- function(tile, block, bands, base_bands,
                                ml_features_name, impute_fn) {
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
    if (.has(ml_features_name)) {
        colnames(values) <- ml_features_name
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
#' @param  impute_fn  Imputation function (to remove NA)
#' @param  multicores number of threads to process the time series.
#' @param  gpu_memory Memory available in GPU
#' @param  progress   Show progress bar?
#' @return A tibble with the predicted labels.
.classify_ts <- function(samples,
                         ml_model,
                         impute_fn,
                         multicores,
                         gpu_memory,
                         progress) {
    # Prepare parallel processing
    started <- .parallel_start(workers = multicores)
    on.exit(.parallel_stop(started), add = TRUE)

    # Get bands from model
    bands <- .ml_bands(ml_model)
    # Update samples bands order
    if (any(bands != .samples_bands(samples))) {
        samples <- .samples_select_bands(
            samples = samples,
            bands = bands
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
    if (.ml_is_torch_model(ml_model) && .torch_gpu_classification()) {
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
        prediction, paste(class(samples)[[1L]], "predicted", sep = "_"),
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
#' @title Classify a chunk of data on CPU
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  chunk        Chunk to be processed
#' @param  tile         Input data tile
#' @param  base_bands  Base bands
#' @param  bands        Bands to be used
#' @param  band_conf    Band configuration
#' @param  impute_fn    Imputation function
#' @param  output_dir   Output directory
#' @param  out_file     Output file
#' @param  ml_model     Model closure, resolved by \code{.classify_job()}
#' @return              Block file path
.classify_chunk_cpu <- function(chunk,
                                tile,
                                base_bands,
                                bands,
                                band_conf,
                                impute_fn,
                                output_dir,
                                out_file,
                                ml_model) {
    # Retrive block to be processed
    block <- .block(chunk)
    block_file <- .classify_block_file(
        out_file = out_file,
        block = block,
        output_dir = output_dir
    )
    .debug_log(event = "start_block", key = "file", value = block_file)
    # Resume processing in case of failure
    if (all(.raster_is_valid(block_file))) {
        .debug_log(event = "end_block", key = "file", value = block_file)
        return(block_file)
    }
    # Read and preprocess values from files
    values <- .classify_data_read(
        tile = tile,
        block = block,
        bands = bands,
        base_bands = base_bands,
        ml_features_name = .ml_features_name(ml_model),
        impute_fn = impute_fn
    )
    # Get mask of NA pixels
    na_mask <- C_mask_na(values)
    # Filter out NA pixels - only classify valid pixels
    values <- values[!na_mask, , drop = FALSE]
    # Define control variable to check for correct termination
    input_pixels <- nrow(values)
    # Start log file
    .debug_log(
        event = "start_block_data_classification",
        key = "model",
        value = .ml_class(ml_model)
    )
    # Apply the classification model only to valid (non-NA) pixels
    if (input_pixels > 0L) {
        # Apply the classification model to values
        # Uses the closure created by sits_train
        values <- ml_model(values)
        # Normalize and calibrate the values
        # Perform softmax for torch models
        values <- .ml_normalize(values, ml_model)
        # Are the results consistent with the data input?
        .check_processed_values(
            values = values,
            input_pixels = input_pixels
        )
        # apply scale and offset
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0.0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1.0) {
            values <- values / scale
        }
    }
    # Log end of block
    .debug_log(
        event = "end_block_data_classification",
        key = "model",
        value = .ml_class(ml_model)
    )
    # Reconstruct full output matrix with NA for masked pixels
    n_labels <- length(.ml_labels(ml_model))
    full_values <- matrix(
        NA_real_,
        nrow = length(na_mask),
        ncol = n_labels,
        dimnames = list(NULL, .ml_labels(ml_model))
    )
    if (input_pixels > 0L) {
        full_values[!na_mask, ] <- values
    }
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
        values = full_values,
        data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf),
        # the jobs hold the crop mask in a list column
        crop_block = if (.has(chunk[["mask"]])) chunk[["mask"]][[1L]]
    )
    # Log end of block saving
    .debug_log(
        event = "end_block_data_save",
        key = "file",
        value = block_file
    )
    # Free memory
    .debug_log(event = "start_block_gc", key = "file", value = block_file)
    gc()
    .debug_log(event = "end_block_gc", key = "file", value = block_file)
    .debug_log(event = "end_block", key = "file", value = block_file)
    # Returned block file
    block_file
}
