#---- internal functions ----
#' @title Create an uncertainty raster cube
#' @name .uncertainty_raster_cube
#' @keywords internal
#' @noRd
#' @param cube A cube
#' @param band band name
#' @param uncert_fn function to compute uncertainty
#' @param output_dir directory where files will be saved
#' @param version version name of resulting cube
#' @param  progress        Check progress bar?
#' @return uncertainty cube
.uncertainty_raster_cube <- function(cube,
                                     band,
                                     uncert_fn,
                                     output_dir,
                                     version,
                                     progress) {
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # Compute uncertainty
        .uncertainty_raster_tile(
            tile = tile,
            band = band,
            uncert_fn = uncert_fn,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    })
}
#' @title Create an uncertainty tile-band asset
#' @name .uncertainty_raster_tile
#' @keywords internal
#' @noRd
#' @param tile tile of data cube
#' @param band band name
#' @param uncert_fn function to compute uncertainty
#' @param output_dir directory where files will be saved
#' @param version version name of resulting cube
#' @param  progress        Check progress bar?
#' @return uncertainty tile-band combination
.uncertainty_raster_tile <- function(tile,
                                     band,
                                     uncert_fn,
                                     output_dir,
                                     version,
                                     progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        # return the existing tile
        uncert_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            derived_class = "uncertainty_cube"
        )
        return(uncert_tile)
    }
    # If output file does not exist
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0L)
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
        if (all(.raster_is_valid(block_file))) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile,
            band = .tile_bands(tile),
            block = block
        )
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Fill with zeros remaining NA pixels
        values <- C_fill_na(values, 0.0)
        # Apply the labeling function to values
        values <- uncert_fn(values)
        # Prepare uncertainty to be saved
        band_conf <- .conf_derived_band(
            derived_class = "uncertainty_cube",
            band = band
        )
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0.0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1.0) {
            values <- values / scale
            values[values > 10000L] <- 10000L
        }
        # Put NA back in the result
        values[na_mask, ] <- NA
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file,
            block = block,
            bbox = .bbox(chunk),
            values = values,
            data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf)
        )
        # Free memory
        gc()
        # Return block file
        block_file
    }, progress = progress)
    # Merge blocks into a new uncertainty_cube tile
    .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        derived_class = "uncertainty_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
}

#---- internal functions ----
#' @title Create an uncertainty raster cube
#' @name .uncertainty_vector_cube
#' @keywords internal
#' @noRd
#' @param cube A cube
#' @param band band name
#' @param output_dir directory where files will be saved
#' @param version version name of resulting cube#'
#' @return uncertainty cube
.uncertainty_vector_cube <- function(cube,
                                     band,
                                     output_dir,
                                     version) {
    # Process each tile sequentially
    uncert_cube <- .cube_foreach_tile(cube, function(tile) {
        # Compute uncertainty
        .uncertainty_vector_tile(
            tile = tile,
            band = band,
            output_dir = output_dir,
            version = version
        )
    })
    class(uncert_cube) <- c("uncertainty_vector_cube", class(cube))
    uncert_cube
}
#' @title Create an uncertainty vector tile
#' @name .uncertainty_vector_tile
#' @keywords internal
#' @noRd
#' @param tile tile of data cube
#' @param band band name
#' @param output_dir directory where files will be saved
#' @param version version name of resulting cube
#' @return uncertainty tile-band combination
.uncertainty_vector_tile <- function(tile,
                                     band,
                                     output_dir,
                                     version) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = band,
        version = version,
        output_dir = output_dir,
        ext = "gpkg"
    )
    # Resume feature
    if (all(.segments_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        uncert_tile <- .tile_segments_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            vector_class = "uncertainty_vector_cube",
            update_bbox = TRUE
        )
        return(uncert_tile)
    }
    # select uncertainty function
    uncert_fn <- switch(band,
        least   = .uncertainty_fn_least(),
        margin  = .uncertainty_fn_margin(),
        entropy = .uncertainty_fn_entropy()
    )
    # get the labels
    labels <- unname(.tile_labels(tile))
    # read the segments
    sf_seg <- .segments_read_vec(tile)
    # extract matrix values from segments
    probs_matrix <- sf_seg |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(labels)) |>
        as.matrix()
    # apply uncertainty function
    uncert_values <- uncert_fn(probs_matrix)
    colnames(uncert_values) <- band
    uncert_values <- tibble::as_tibble(uncert_values)
    # merge uncertainty values
    sf_seg <- sf_seg |>
        dplyr::bind_cols(uncert_values) |>
        dplyr::relocate(dplyr::all_of(band), .before = "geom")
    # Prepare and save results as vector
    .vector_write_vec(v_obj = sf_seg, file_path = out_file)
    # Set information on uncert_tile
    uncert_tile <- tile
    uncert_tile[["vector_info"]][[1L]][["band"]] <- band
    uncert_tile[["vector_info"]][[1L]][["path"]] <- out_file
    class(uncert_tile) <- c("uncertainty_vector_cube", class(uncert_tile))
    uncert_tile
}

#---- uncertainty functions ----
#' @title Least uncertainty function
#' @name .uncertainty_fn_least
#' @keywords internal
#' @noRd
#' @return a closure to apply a function in matrix values
.uncertainty_fn_least <- function() {
    # Define uncertainty function
    uncert_fn <- function(values) {
        # Used in check (below)
        input_pixels <- nrow(values)
        # Process least confidence
        # return a matrix[rows(values),1]
        values <- C_least_probs(values)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return data
        values
    }
    # Return closure
    uncert_fn
}
#' @title Entropy uncertainty function
#' @name .uncertainty_fn_entropy
#' @keywords internal
#' @noRd
#' @return a closure to apply a function in matrix values
.uncertainty_fn_entropy <- function() {
    # Define uncertainty function
    uncert_fn <- function(values) {
        # Used in check (below)
        input_pixels <- nrow(values)
        # avoid passing zero values
        values[values < 0.00001] <- 0.00001
        # Process least confidence
        values <- C_entropy_probs(values) # return a matrix[rows(values),1]
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return data
        values
    }
    # Return closure
    uncert_fn
}
#' @title margin uncertainty function
#' @name .uncertainty_fn_margin
#' @keywords internal
#' @noRd
#' @return a closure to apply a function in matrix values
.uncertainty_fn_margin <- function() {
    # Define uncertainty function
    uncert_fn <- function(values) {
        # Used in check (below)
        input_pixels <- nrow(values)
        # Process margin
        values <- C_margin_probs(values) # return a matrix[rows(data),1]
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return data
        values
    }
    # Return closure
    uncert_fn
}

#' @title Create an uncertainty segment tile
#' @name .uncertainty_segment_tile
#' @keywords internal
#' @noRd
#' @param tile tile of data cube
#' @param band band name
#' @param agg_method aggregation method
#' @param output_dir directory where files will be saved
#' @param version version name of resulting cube
#' @param progress progress bar
#' @return uncertainty tile
.uncertainty_segment_tile <- function(tile,
                                      band,
                                      agg_method,
                                      output_dir,
                                      version,
                                      progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        uncert_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            derived_class = "uncertainty_cube",
            update_bbox = FALSE
        )
        uncert_tile[["vector_info"]] <- tile[["vector_info"]]
        vector_classes <- c(
            .conf_vector_s3class("uncertainty_vector_cube"),
            class(uncert_tile)
        )
        return(.cube_set_class(uncert_tile, vector_classes))
    }

    # Get labels
    labels <- .tile_labels(tile)
    # Read the segments
    segments <- .segments_read_vec(tile)
    # Open probability raster (all bands)
    probs_path <- .tile_path(tile)
    probs_rast <- .raster_open_rast(probs_path)
    # Extract pixel probabilities for each segment
    extracted <- .raster_extract(
        rast = probs_rast,
        xy = .raster_open_vect(segments),
        fun = NULL
    )
    # Apply scale and offset to extracted probability values
    prob_cols <- setdiff(colnames(extracted), "ID")
    probs_band <- .tile_bands(tile)[[1L]]
    probs_band_conf <- .tile_band_conf(tile, probs_band)
    probs_scale <- .scale(probs_band_conf)
    if (.has(probs_scale) && probs_scale != 1.0) {
        extracted[, prob_cols] <- extracted[, prob_cols] * probs_scale
    }
    probs_offset <- .offset(probs_band_conf)
    if (.has(probs_offset) && probs_offset != 0.0) {
        extracted[, prob_cols] <- extracted[, prob_cols] + probs_offset
    }
    # Aggregation function
    agg_fn <- switch(agg_method,
        "mean" = function(probs) {
            colMeans(probs, na.rm = TRUE)
        },
        "median" = function(probs) {
            med <- apply(probs, 2L, stats::median, na.rm = TRUE)
            sum_med <- sum(med)
            if (sum_med > 0) med / sum_med else med
        },
        stop("Unknown aggregation method: ", agg_method, call. = FALSE)
    )
    # Select uncertainty function
    uncert_fn <- switch(band,
        least   = .uncertainty_fn_least(),
        margin  = .uncertainty_fn_margin(),
        entropy = .uncertainty_fn_entropy()
    )
    # Probability columns (all bands in the probs raster)
    prob_cols <- setdiff(colnames(extracted), "ID")
    # Aggregate probabilities per segment
    segment_ids <- sort(unique(extracted[["ID"]]))
    seg_probs <- lapply(segment_ids, function(sid) {
        seg_pixels <- extracted[extracted[["ID"]] == sid, prob_cols,
            drop = FALSE
        ]
        if (nrow(seg_pixels) == 0L || all(is.na(seg_pixels))) {
            return(rep(NA_real_, length(prob_cols)))
        }
        agg_fn(as.matrix(seg_pixels))
    })
    # Convert list of vectors to a matrix (segments x classes)
    seg_probs_matrix <- do.call(rbind, seg_probs)
    # Compute uncertainty
    seg_uncert <- uncert_fn(seg_probs_matrix)
    # Band configuration
    band_conf <- .conf_derived_band(
        derived_class = "uncertainty_cube", band = band
    )
    # Apply offset/scale for saving uncertainty
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0.0) {
        seg_uncert <- seg_uncert - offset
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1.0) {
        seg_uncert <- seg_uncert / scale
        seg_uncert[seg_uncert > 10000.0] <- 10000.0
    }
    # Rasterize: assign uncertainty to all pixels within each segment
    seg_vect <- .raster_open_vect(segments[segment_ids, ])
    seg_vect[["uncert_value"]] <- as.vector(seg_uncert)
    # Create output raster from template
    template_rast <- .raster_rast(probs_rast, nlayers = 1L)
    # Rasterize segments onto the template
    uncert_rast <- .raster_rasterize(
        vect = seg_vect,
        rast = template_rast,
        field = "uncert_value",
        fun = "max"
    )
    # Set missing value
    uncert_rast <- .raster_set_na(uncert_rast, .miss_value(band_conf))
    # Write raster
    .raster_write_rast(
        rast = uncert_rast,
        file = out_file,
        data_type = .data_type(band_conf),
        overwrite = TRUE,
        missing_value = .miss_value(band_conf)
    )
    # Create output tile from file
    uncert_tile <- .tile_derived_from_file(
        file = out_file,
        band = band,
        base_tile = tile,
        derived_class = "uncertainty_cube",
        update_bbox = FALSE
    )
    # Preserve vector_info from the input probs tile (segments are not modified)
    uncert_tile[["vector_info"]] <- tile[["vector_info"]]
    # Set uncertainty_vector_cube + uncertainty_cube chain
    vector_classes <- c(
        .conf_vector_s3class("uncertainty_vector_cube"),
        class(uncert_tile)
    )
    .cube_set_class(uncert_tile, vector_classes)
}

