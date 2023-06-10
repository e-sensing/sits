sits_temporal_segment <- function(data, ...,
                                  impute_fn = sits_impute_linear(),
                                  memsize = 8,
                                  multicores = 2,
                                  verbose = FALSE,
                                  progress = TRUE) {
    # preconditions
    .check_is_raster_cube(data)
    .check_is_regular(data)
    .check_memsize(memsize)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(data)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize, block = block,
        image_size = .tile_size(.tile(data)), memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Show block information
    if (verbose) {
        start_time <- Sys.time()
        message("Using blocks of size (", .nrows(block),
                " x ", .ncols(block), ")")
    }
    # Classification
    # Process each tile sequentially
    probs_cube <- .cube_foreach_tile(data, function(tile) {
        # Classify the data
        probs_tile <- .segment_tile(
            tile = tile,
            block = block,
            impute_fn = impute_fn,
            verbose = verbose,
            progress = progress
        )
        return(probs_tile)
    })
    # Show block information
    if (verbose) {
        end_time <- Sys.time()
        message("")
        message("Classification finished at ", end_time)
        message("Elapsed time of ",
                format(round(end_time - start_time, digits = 2)))
    }
    return(probs_cube)
}

.segment_tile  <- function(tile,
                           block,
                           impute_fn,
                           verbose,
                           progress) {

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
    # Process jobs in parallel
    .jobs_map_parallel(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
       # Read cloud band
        cloud_mask <- .tile_cloud_read_block(tile = tile, block = block)
        # Read and preprocess values
        values <- purrr::map_dfc(.tile_bands(tile), function(band) {
            # Get band values (stops if band not found)
            values <- .tile_read_block(tile = tile, band = band, block = block)

            # Remove cloud masked pixels
            if (.has(cloud_mask)) {
                values[cloud_mask] <- NA
            }

            # Remove NA pixels
            if (.has(impute_fn)) {
                values <- impute_fn(values)
            }
            # Get mask of NA pixels
            na_mask <- C_mask_na(values)
            # Fill with zeros remaining NA pixels
            values <- C_fill_na(values, 0)
            # Used to check values (below)
            input_pixels <- nrow(values)

            # Free memory
            gc()

            # Apply the classification model to values
            return(as.data.frame(values))
        })
        # Compose final values
        values <- as.matrix(values)
        # Return values
        values
    }, progress = progress)
}
