#---- internal functions ----
#' @title Create an uncertainty raster cube
#' @name .uncertainty_raster_cube
#' @keywords internal
#' @noRd
#' @param cube A cube
#' @param band band name
#' @param uncert_fn function to compute uncertainty
#' @param output_dir directory where files will be saved
#' @param version version name of resulting cube#'
#' @return uncertainty cube
.uncertainty_raster_cube <- function(cube,
                              band,
                              uncert_fn,
                              output_dir,
                              version) {
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # Compute uncertainty
        .uncertainty_raster_tile(
            tile = tile,
            band = band,
            uncert_fn = uncert_fn,
            output_dir = output_dir,
            version = version
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
#' @return uncertainty tile-band combination
.uncertainty_raster_tile <- function(tile,
                              band,
                              uncert_fn,
                              output_dir,
                              version) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile,
        band = band,
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        .check_recovery(tile[["tile"]])
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
    chunks <- .tile_chunks_create(tile = tile, overlap = 0)
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
        values <- .tile_read_block(
            tile = tile,
            band = .tile_bands(tile),
            block = block
        )
        # Fill with zeros remaining NA pixels
        values <- C_fill_na(values, 0)
        # Apply the labeling function to values
        values <- uncert_fn(values)
        # Prepare uncertainty to be saved
        band_conf <- .conf_derived_band(
            derived_class = "uncertainty_cube",
            band = band
        )
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            values <- values / scale
            values[values > 10000] <- 10000
        }
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
    })
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
    if (file.exists(out_file)) {
        if (.check_messages()) {
            .check_recovery(out_file)
        }
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
    uncert_fn <- switch(
        band,
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
    uncert_tile[["vector_info"]][[1]][["band"]] <- band
    uncert_tile[["vector_info"]][[1]][["path"]] <- out_file
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
