
#---- internal functions ----

.reclassify_tile <- function(tile, mask, band, labels, reclassify_fn,
                             output_dir, version) {
    # Output files
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        .check_recovery(tile[["tile"]])
        class_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            derived_class = "class_cube",
            update_bbox = FALSE
        )
        return(class_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0)
    # start parallel process
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Output file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Output mask file name
        mask_block_file <- .file_block_name(
            pattern = .file_pattern(out_file, suffix = "_mask"),
            block = block, output_dir = output_dir
        )
        # If there is any mask file delete it
        unlink(mask_block_file)
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Project mask block to template block
        # Get band conf missing value
        band_conf <- .conf_derived_band(
            derived_class = "class_cube", band = band
        )
        # Create template block for mask
        .gdal_template_block(
            block = block, bbox = .bbox(chunk), file = mask_block_file,
            nlayers = 1, miss_value = .miss_value(band_conf),
            data_type = .data_type(band_conf)
        )
        # Copy values from mask cube into mask template
        .gdal_merge_into(
            file = mask_block_file,
            base_files = .fi_paths(.fi(mask)), multicores = 1
        )
        # Build a new tile for mask based on template
        mask_tile <- .tile_derived_from_file(
            file = mask_block_file,
            band = "class",
            base_tile = .tile(mask),
            derived_class = "class_cube",
            update_bbox = FALSE
        )
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Read and preprocess values of mask block
        mask_values <- .tile_read_block(
            tile = mask_tile, band = .tile_bands(mask_tile), block = NULL
        )
        # Evaluate expressions
        values <- reclassify_fn(values = values, mask_values = mask_values)
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
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Delete unneeded mask block file
        unlink(mask_block_file)
        # Free memory
        gc()
        # Returned value
        block_file
    })
    # Merge blocks into a new class_cube tile
    class_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = labels,
        base_tile = tile,
        block_files = block_files,
        derived_class = "class_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Return class tile
    class_tile
}

.reclassify_fn_expr <- function(rules, labels_cube, labels_mask) {
    # Check if rules are named
    if (!all(.has_name(rules))) {
        stop("rules should be named")
    }
    # Get output labels
    labels <- unique(c(labels_cube, names(rules)))
    # Define reclassify function
    reclassify_fn <- function(values, mask_values) {
        # Check compatibility
        if (!all(dim(values) == dim(mask_values))) {
            stop("cube and mask values have different sizes")
        }
        # Used to check values (below)
        input_pixels <- nrow(values)
        # New evaluation environment
        env <- list2env(list(
            # Read values and convert to character
            cube = labels_cube[values], mask = labels_mask[mask_values]
        ))
        # Get values as character
        values <- env[["cube"]]
        # Evaluate each expression
        for (label in names(rules)) {
            # Get expression
            expr <- rules[[label]]
            # Evaluate
            result <- eval(expr, envir = env)
            # Update values
            if (!is.logical(result)) {
                stop("expression should evaluate to logical values")
            }
            values[result] <- label
        }
        # Get values as numeric
        values <- matrix(data = match(values, labels), nrow = input_pixels)
        # Mask NA values
        values[is.na(env[["mask"]])] <- NA
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return closure
    reclassify_fn
}
