#---- internal functions ----
.comb <- function(probs_cubes,
                  uncert_cubes,
                  comb_fn,
                  memsize,
                  multicores,
                  output_dir,
                  version, ...) {
    # Check memory and multicores
    # Get block size
    base_cube <- probs_cubes[[1]]
    block <- .raster_file_blocksize(
        .raster_open_rast(.tile_path(base_cube))
    )
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block),
        npaths = length(probs_cubes) * nrow(base_cube) *
            length(sits_labels(base_cube)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(base_cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Combining
    # process each brick layer (each time step) individually
    probs_cube <- purrr::map_dfr(seq_len(nrow(base_cube)), function(i) {
        probs_tile <- .comb_tiles(
            probs_tiles = lapply(probs_cubes, .slice_dfr, i),
            uncert_tiles = lapply(uncert_cubes, .slice_dfr, i),
            band = "probs",
            comb_fn = comb_fn,
            block = block,
            output_dir = output_dir,
            version = version
        )
        probs_tile
    })
    probs_cube
}

.comb_tiles <- function(probs_tiles,
                        uncert_tiles,
                        band,
                        comb_fn,
                        block,
                        output_dir,
                        version) {
    base_tile <- probs_tiles[[1]]
    # Output file
    out_file <- .file_derived_name(
        tile = base_tile, band = band, version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
        message("Recovery: tile '", base_tile[["tile"]], "' already exists.")
        message("(If you want to produce a new probability image, please ",
                "change 'output_dir' or 'version' parameters)")
        probs_tile <- .tile_probs_from_file(
            file = out_file, band = band, base_tile = base_tile,
            labels = .tile_labels(base_tile), update_bbox = FALSE
        )
        return(probs_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = base_tile, overlap = 0, block = block)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- lapply(probs_tiles, function(tile) {
            .tile_read_block(
                tile = tile, band = .tile_bands(tile), block = block
            )
        })
        # If an uncertainty cube has been passed, read it
        uncert_values <- NULL
        if (.has(uncert_tiles)) {
            # Read and preprocess values
            uncert_values <- lapply(uncert_tiles, function(tile) {
                .tile_read_block(
                    tile = tile, band = .tile_bands(tile), block = block
                )
            })
        }
        # Apply the probability function to values
        values <- comb_fn(values, uncert_values = uncert_values)
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
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf)
        )
        # Free memory
        gc()
        # Return block file
        block_file
    })
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_probs_merge_blocks(
        file = out_file, band = band, labels = .tile_labels(base_tile),
        base_tile = base_tile, block_files = block_files,
        multicores = .jobs_multicores(), update_bbox = FALSE
    )
    # Return probs tile
    probs_tile
}

#---- combine functions ----

.comb_fn_average <- function(cubes, weights) {
    # Average probability calculation
    comb_fn <- function(values, uncert_values = NULL) {
        # Check values length
        input_pixels <- nrow(values[[1]])
        # Combine by average
        values <- weighted_probs(values, weights)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        .check_that(
            ncol(values) == length(sits_labels(cubes[[1]])),
            msg = paste("number of columns of processed matrix is different",
                        "from the number of cube labels")
        )
        # Return values
        values
    }
    comb_fn
}

.comb_fn_uncertainty <- function(cubes) {
    # Average probability calculation
    comb_fn <- function(values, uncert_values) {
        # Check values length
        input_pixels <- nrow(values[[1]])
        # Combine by average
        values <- weighted_uncert_probs(values, uncert_values)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        .check_that(
            ncol(values) == length(sits_labels(cubes[[1]])),
            msg = paste("number of columns of processed matrix is different",
                        "from the number of cube labels")
        )
        # Return values
        values
    }
    comb_fn
}
