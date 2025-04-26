#' @title Detect changes in time-series using various methods.
#' @name .detect_change_ts
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
.detect_change_ts <- function(samples,
                              dc_method,
                              filter_fn,
                              multicores,
                              progress) {
    # Start parallel workers
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Get bands from model
    bands <- .dc_bands(dc_method)
    # Update samples bands order
    if (any(bands != .samples_bands(samples))) {
        samples <- .samples_select_bands(samples = samples,
                                         bands = bands)
    }
    # Apply time series filter
    if (.has(filter_fn)) {
        samples <- .apply_across(data = samples, fn = filter_fn)
    }
    # Divide samples in chunks to parallel processing
    parts <- .pred_create_partition(pred = samples, partitions = multicores)
    # Detect changes!
    .jobs_map_parallel_dfr(parts, function(part) {
        # Get samples
        values <- .pred_part(part)
        # Detect changes! For detection, models can be time-aware. So, the
        # complete data, including dates, must be passed as argument.
        detections <- dc_method(values[["time_series"]], "ts")
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
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  tile            Single tile of a data cube.
#' @param  band            Band to be produced.
#' @param  dc_method       Change Detection Model.
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
                                dc_method,
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
        .check_recovery()
        seg_tile <- .tile_segments_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            vector_class = "segs_cube",
            update_bbox = TRUE
        )
        return(seg_tile)
    }
    # Show initial time for tile classification
    tile_start_time <- .tile_classif_start(
        tile = tile,
        verbose = verbose
    )
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile,
        overlap = 0L,
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
    prep_data <- .detect_change_tile_prep(
        dc_method = dc_method,
        tile      = tile,
        filter_fn = filter_fn,
        impute_fn = impute_fn
    )
    # Create index timeline
    tile_tl <- .detect_change_create_timeline(tile)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        bbox <- .bbox(chunk)
        # Block file name
        hash_bundle <- digest::digest(list(block, dc_method), algo = "md5")
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
            bands = .dc_bands(dc_method),
            base_bands = NULL,
            ml_model = dc_method,
            impute_fn = impute_fn,
            filter_fn = filter_fn
        )
        # Used to check values (below)
        input_pixels <- nrow(values)
        # Log here
        .debug_log(
            event = "start_block_data_detection",
            key = "model",
            value = .dc_class(dc_method)
        )
        # Detect changes!
        values <- dc_method(
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
            value = .dc_class(dc_method)
        )
        # Get date that corresponds to the index value
        values <- tile_tl[.as_chr(values)]
        # Polygonize values
        values <- .detect_change_as_polygon(
            values = values,
            block = block,
            bbox = bbox
        )
        # Remove non-detection values
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
#' @title Pre-process tile to run detect_change method
#' @name .detect_change_tile_prep
#' @keywords internal
#' @noRd
#' @param  dc_method       Detect change method
#' @param  tile            Single tile of a data cube.
#' @param  ...             Additional parameters
#' @param  impute_fn       Imputation function
#' @return                 Scaled values for detect change method
.detect_change_tile_prep <- function(dc_method, tile, ...) {
    UseMethod(".detect_change_tile_prep", dc_method)
}
#' @noRd
#' @export
.detect_change_tile_prep.default <- function(dc_method, tile, ...) {
    NULL
}
#' @noRd
#' @export
.detect_change_tile_prep.bayts_model <-
    function(dc_method, tile, ..., impute_fn) {
        deseasonlize <- environment(dc_method)[["deseasonlize"]]

        if (!.has(deseasonlize)) {
            return(matrix(NA))
        }

        tile_bands <- .tile_bands(tile, FALSE)
        quantile_values <- purrr::map(tile_bands, function(tile_band) {
            tile_paths <- .tile_paths(tile, bands = tile_band)
            rast <- .raster_open_rast(tile_paths)
            quantile_values <- .raster_quantile(
                rast, quantile = deseasonlize, na.rm = TRUE
            )
            quantile_values <- impute_fn(t(quantile_values))
            # Fill with zeros remaining NA pixels
            quantile_values <- C_fill_na(quantile_values, 0.0)
            # Apply scale
            band_conf <- .tile_band_conf(tile = tile, band = tile_band)
            scale <- .scale(band_conf)
            if (.has(scale) && scale != 1.0) {
                quantile_values <- quantile_values * scale
            }
            offset <- .offset(band_conf)
            if (.has(offset) && offset != 0.0) {
                quantile_values <- quantile_values + offset
            }
            unname(quantile_values)
        })
        do.call(cbind, quantile_values)
    }
#' @title Pre-process tile to run detect_change method (bayts)
#' @name .detect_change_create_timeline
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  dc_method       Detect change method
#' @param  tile            Single tile of a data cube.
#' @param  ...             Additional parameters
#' @param  impute_fn       Imputation function
#' @return                 Timeline organized as sequence of values
.detect_change_create_timeline <- function(tile) {
    # Get the number of dates in the timeline
    tile_tl <- .as_chr(.tile_timeline(tile))
    tile_tl <- c("0", tile_tl)
    names(tile_tl) <- seq.int(
        from = 0L, to = length(tile_tl) - 1L, by = 1L
    )
    tile_tl
}
#' @title Detect change as a polygon
#' @name .detect_change_as_polygon
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @param values     Matrix of values for a raster (time series)
#' @param block      Data block that is being processed
#' @param bbox       Bounding box of the block
#' @return           Vector object with polygons
.detect_change_as_polygon <- function(values, block, bbox) {
    # Create a template raster
    template_raster <- .raster_new_rast(
        nrows = block[["nrows"]], ncols = block[["ncols"]],
        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
        nlayers = 1L, crs = bbox[["crs"]]
    )
    # Set values and NA value in template raster
    values <- .raster_set_values(template_raster, values)
    values <- .raster_set_na(values, 0.0)
    names(values) <- "date"
    # Extract polygons raster and convert to sf object
    values <- .raster_extract_polygons(values, dissolve = TRUE)
    values <- sf::st_as_sf(values)
    if (nrow(values) == 0L) {
        return(values)
    }
    # Get only polygons segments
    sf::st_collection_extract(values, "POLYGON")
}
#' @rdname .dc_samples
#' @title Retrieve samples available in a given detect change method.
#' @name .dc_samples
#' @keywords internal
#' @noRd
#' @param  dc_method       Detect change method
#' @return Samples available in the dc method.
.dc_samples <- function(dc_method) {
    environment(dc_method)[["samples"]]
}
#' @title Retrieve bands associated to detect_change method
#' @name .dc_bands
#' @keywords internal
#' @noRd
#' @param  dc_method       Detect change method
#' @return Bands associated to the detect change method
.dc_bands <- function(dc_method) {
    UseMethod(".dc_bands", dc_method)
}
#' @noRd
#' @export
.dc_bands.sits_model <- function(dc_method) {
    .samples_bands(.dc_samples(dc_method))
}
#' @noRd
#' @export
.dc_bands.bayts_model <- function(dc_method) {
    if (.has(.dc_samples(dc_method))) {
        return(NextMethod(".dc_bands", dc_method))
    }
    stats <- environment(dc_method)[["stats"]]
    stats <- unlist(lapply(stats, colnames))
    unique(stats)
}
#' @title Retrieve bands associated to detect_change method
#' @name .dc_class
#' @keywords internal
#' @noRd
#' @param  dc_method       Detect change method
#' @return Class of the model.
.dc_class <- function(dc_method) {
    class(dc_method)[[1L]]
}
