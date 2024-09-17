#' @title Detect changes in time-series using various methods.
#' @name .change_detect_ts
#' @keywords internal
#' @noRd
.change_detect_ts <- function(samples,
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
#' @name .change_detect_tile
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
.change_detect_tile <- function(tile,
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
        output_dir = output_dir,
        ext = "gpkg"
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
    # Case where preprocessing is needed, default is NULL
    prep_data <- .change_detect_tile_prep(
        cd_method = cd_method,
        tile      = tile,
        filter_fn = filter_fn,
        impute_fn = impute_fn
    )
    # Create index timeline
    tile_tl <- .change_detect_create_timeline(tile)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        bbox <- .bbox(chunk)
        # Block file name
        hash_bundle <- digest::digest(list(block, cd_method), algo = "md5")
        block_file <- .file_block_name(
            pattern = paste0(hash_bundle, "_change"),
            block = block,
            output_dir = output_dir,
            ext = "gpkg"
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
        # Used to check values (below)
        input_pixels <- nrow(values)
        # Log here
        .debug_log(
            event = "start_block_data_detection",
            key = "model",
            value = .ml_class(cd_method)
        )
        # Detect changes!
        values <- cd_method(
            values = values,
            tile = tile,
            prep_data = prep_data
        )
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
        # Get date that corresponds to the index value
        values <- tile_tl[.as_chr(values)]
        # Polygonize values
        values <- .change_detect_as_polygon(
            values = values,
            block = block,
            bbox = bbox
        )
        # Remove non-detection polygons
        values <- values[values[["date"]] != "0", ]
        # Log
        .debug_log(
            event = "start_block_data_save",
            key = "file",
            value = block_file
        )
        # Prepare and save results as vector
        .vector_write_vec(
            v_obj = values,
            file_path = block_file
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

    # Merge blocks into a new segs_cube tile
    segs_tile <- .tile_segment_merge_blocks(
        block_files = block_files,
        base_tile = tile,
        band = band,
        vector_class = "segs_cube",
        out_file = out_file,
        update_bbox = update_bbox
    )
    # Show final time for detection
    .tile_classif_end(
        tile = tile,
        start_time = tile_start_time,
        verbose = verbose
    )
    # Return detection tile
    segs_tile
}

#' @export
.change_detect_tile_prep <- function(cd_method, tile, ...) {
    UseMethod(".change_detect_tile_prep", cd_method)
}

#' @export
.change_detect_tile_prep.default <- function(cd_method, tile, ...) {
    return(NULL)
}

.change_detect_create_timeline <- function(tile) {
    # Get the number of dates in the timeline
    tile_tl <- .as_chr(.tile_timeline(tile))
    tile_tl <- c("0", tile_tl)
    names(tile_tl) <- seq.int(
        from = 0, to = length(tile_tl) - 1, by = 1
    )
    tile_tl
}

.change_detect_as_polygon <- function(values, block, bbox) {
    # Create a template raster
    template_raster <- .raster_new_rast(
        nrows = block[["nrows"]], ncols = block[["ncols"]],
        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
        nlayers = 1, crs = bbox[["crs"]]
    )
    # Set values and NA value in template raster
    values <- .raster_set_values(template_raster, values)
    values <- .raster_set_na(values, 0)
    names(values) <- "date"
    # Extract polygons raster and convert to sf object
    values <- .raster_extract_polygons(values, dissolve = TRUE)
    values <- sf::st_as_sf(values)
    if (nrow(values) == 0) {
        return(values)
    }
    # Get only polygons segments
    values <- suppressWarnings(sf::st_collection_extract(values, "POLYGON"))
    # Return the segment object
    return(values)
}
