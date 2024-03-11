#' @title Build a classified map from a tile
#' @noRd
#' @param tile     Tile of data cube
#' @param band     Spectral band
#' @param label_fn Function to be used for labelling
#' @param output_dir Directory where file will be saved
#' @param version  Version name
#' @param progress Show progress bar?
#' @returns        File path for derived file
.label_tile <- function(tile, band, label_fn, output_dir, version, progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        .check_recovery(tile[["tile"]])
        class_tile <- .tile_derived_from_file(
            file = out_file,
            band = "class",
            base_tile = tile,
            derived_class = "class_cube",
            labels = .tile_labels(tile),
            update_bbox = FALSE
        )
        return(class_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Output file name
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
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Apply the labeling function to values
        values <- label_fn(values)
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "class_cube", band = band
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
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned value
        block_file
    }, progress = progress)
    # Merge blocks into a new class_cube tile
    class_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .tile_labels(tile),
        base_tile = tile,
        block_files = block_files,
        derived_class = "class_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Return class tile
    class_tile
}

#' @title Build a classified vector segments from a tile
#' @noRd
#' @param tile     Tile of data cube
#' @param band     Spectral band
#' @param output_dir Directory where file will be saved
#' @param version  Version name
#' @return        Classified vector tile
.label_vector_tile <- function(tile, band, version, output_dir) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = "class", version = version,
        output_dir = output_dir, ext = "gpkg"
    )
    # Resume feature
    if (.segments_is_valid(out_file)) {
        if (.check_messages()) {
            message("Recovery: tile '", tile[["tile"]], "' already exists.")
            message(
                "(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)"
            )
        }
        # Create tile based on template
        class_tile <- .tile_segments_from_file(
            file = out_file,
            band = "class",
            base_tile = tile,
            labels = .tile_labels(tile),
            vector_class = "class_vector_cube",
            update_bbox = FALSE
        )
        # Return classified vector tile
        return(class_tile)
    }
    # Get tile labels
    labels <- unname(.tile_labels(tile))
    # Read probability segments
    probs_segments <- .segments_read_vec(tile)
    # Classify each segment by majority probability
    probs_segments <- probs_segments |>
        dplyr::rowwise() |>
        dplyr::filter(!any(is.na(dplyr::c_across(dplyr::all_of(labels))))) |>
        dplyr::mutate(class = labels[which.max(
            dplyr::c_across(dplyr::all_of(labels)))]) |>
        dplyr::mutate(pol_id = as.numeric(.data[["pol_id"]]))
    # Write all segments
    .vector_write_vec(v_obj = probs_segments, file_path = out_file)
    # Create tile based on template
    class_tile <- .tile_segments_from_file(
        file = out_file,
        band = "class",
        base_tile = tile,
        labels = .tile_labels(tile),
        vector_class = "class_vector_cube",
        update_bbox = FALSE
    )
    # Return classified vector tile
    return(class_tile)
}

#---- label functions ----
#' @title Build a classified map from probs cube based on maximal probability
#' @noRd
#' @returns       Function to be used to labelling
.label_fn_majority <- function() {
    label_fn <- function(values) {
        # Used to check values (below)
        input_pixels <- nrow(values)
        values <- C_label_max_prob(values)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return closure
    label_fn
}
.label_gpkg_file <- function(gpkg_file){
    sf <- sf::st_read(gpkg_file, quiet = TRUE)
    labels <- setdiff(colnames(sf), c("supercells", "x", "y",
                                      "pol_id", "geom", "class"))
    return(labels)
}
