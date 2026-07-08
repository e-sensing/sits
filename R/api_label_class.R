#' @title Build a classified map from a tile
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
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
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
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
    chunks <- .tile_chunks_create(tile = tile, overlap = 0L)
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
        if (all(.raster_is_valid(block_file))) {
            return(block_file)
        }
        band_conf <- .conf_derived_band(
            derived_class = "class_cube", band = band
        )
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Apply the labeling function to values
        values <- label_fn(values)
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
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
    if (all(.segments_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
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
    tile_labels <- unname(.tile_labels(tile))
    # Read probability segments
    probs_segments <- .segments_read_vec(tile)
    # Segment labels
    segment_labels <- setdiff(
        colnames(probs_segments), c("supercells", "x", "y", "pol_id", "geom")
    )
    # Required when not all labels are present on the tile
    labels <- intersect(tile_labels, segment_labels)
    # Classify each segment by majority probability
    probs_segments <- probs_segments |>
        dplyr::rowwise() |>
        dplyr::filter(!anyNA(dplyr::c_across(dplyr::all_of(labels)))) |>
        dplyr::mutate(
            class = labels[which.max(dplyr::c_across(dplyr::all_of(labels)))],
            pol_id = as.numeric(.data[["pol_id"]])
        )

    # Write all segments
    .vector_write_vec(v_obj = probs_segments, file_path = out_file)
    # Create class tile based on template and return empty vector tile
    .tile_segments_from_file(
        file = out_file,
        band = "class",
        base_tile = tile,
        labels = .tile_labels(tile),
        vector_class = "class_vector_cube",
        update_bbox = FALSE
    )
}

#' @title Label a probs_vector_cube using segment-based aggregation
#' @name .label_segment_tile
#' @keywords internal
#' @noRd
#' @description Aggregates pixel-level probabilities inside each segment,
#'   assigns a class per segment, and rasterizes the result. The input
#'   segments GPKG is not modified.
#' @param tile         Single tile of a probs_vector_cube.
#' @param band         Output band name (typically "class").
#' @param label_method Decision method: "mean", "median", or "majority".
#' @param output_dir   Directory where output files will be saved.
#' @param version      Version string.
#' @param progress     Show progress bar?
#' @return             A tile with class_vector_cube + class_cube chain.
.label_segment_tile <- function(tile, band, label_method,
                                output_dir, version, progress) {
    # Output raster file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature: if raster output already exists, return from file
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        class_tile <- .tile_derived_from_file(
            file = out_file,
            band = "class",
            base_tile = tile,
            derived_class = "class_cube",
            labels = .tile_labels(tile),
            update_bbox = FALSE
        )
        # Preserve vector_info and set class_vector_cube + class_cube chain
        class_tile[["vector_info"]] <- tile[["vector_info"]]
        vector_classes <- c(
            .conf_vector_s3class("class_vector_cube"),
            class(class_tile)
        )
        return(.cube_set_class(class_tile, vector_classes))
    }
    # Get labels
    labels <- .tile_labels(tile)
    # Read segment polygons
    segments <- .segments_read_vec(tile)
    # Open probability raster (all bands)
    probs_path <- .tile_path(tile)
    probs_rast <- .raster_open_rast(probs_path)
    # Extract pixel probabilities for each segment
    # Returns a data.frame with an ID column matching segment row indices
    extracted <- .raster_extract(
        rast = probs_rast,
        xy = .raster_open_vect(segments),
        fun = NULL
    )
    # Probability columns (all bands in the probs raster)
    prob_cols <- setdiff(colnames(extracted), "ID")
    # Aggregate probabilities per segment and assign a class.
    seg_results <- .label_segment_classes(
        extracted = extracted,
        prob_cols = prob_cols,
        label_method = label_method
    )
    segment_ids <- seg_results[["ids"]]
    seg_class_idx <- seg_results[["class_idx"]]
    # Rasterize: assign class index to all pixels within each segment
    seg_vect <- .raster_open_vect(segments[segment_ids, ])
    seg_vect[["class_value"]] <- seg_class_idx
    # Create output raster from template
    template_rast <- .raster_rast(probs_rast, nlayers = 1L)
    band_conf <- .conf_derived_band(
        derived_class = "class_cube", band = band
    )
    # Rasterize segments onto the template
    class_rast <- .raster_rasterize(
        vect = seg_vect,
        rast = template_rast,
        field = "class_value",
        fun = "max"
    )
    # Set missing value
    class_rast <- .raster_set_na(class_rast, .miss_value(band_conf))
    # Write raster
    .raster_write_rast(
        rast = class_rast,
        file = out_file,
        data_type = .data_type(band_conf),
        overwrite = TRUE,
        missing_value = .miss_value(band_conf)
    )
    # Create output tile from file
    class_tile <- .tile_derived_from_file(
        file = out_file,
        band = "class",
        base_tile = tile,
        derived_class = "class_cube",
        labels = labels,
        update_bbox = FALSE
    )
    # Preserve vector_info from the input probs tile (segments are not modified)
    class_tile[["vector_info"]] <- tile[["vector_info"]]
    # Set class_vector_cube + class_cube chain
    vector_classes <- c(
        .conf_vector_s3class("class_vector_cube"),
        class(class_tile)
    )
    .cube_set_class(class_tile, vector_classes)
}

#' @title Define a class for each segment
#' @name .label_segment_classes
#' @keywords internal
#' @noRd
#' @description Aggregate pixel probabilities per segment and assign a class.
#' @param extracted Tibble with an \code{ID} column and one column per
#'                  probability band.
#' @param prob_cols Names of the probability columns in \code{extracted}.
#' @param label_method Label selection method: "mean", "median", or "majority".
#' @return A list with \code{ids} and \code{class_idx} (integer class index per
#'         segment. Value is \code{NA} when the segment has no valid pixels).
.label_segment_classes <- function(extracted, prob_cols, label_method) {
    # Get ID
    ids <- extracted[["ID"]]
    # Extract probs
    probs <- as.matrix(extracted[prob_cols])
    # Get labels
    n_labels <- length(prob_cols)
    # Define IDs as factor
    ids_factor <- factor(ids)
    # Get segment ids
    segment_ids <- as.integer(levels(ids_factor))
    # Aggregate pixels into a segment x class matrix
    agg <- switch(label_method,
        "mean" = {
            # Compute mean of each class
            # Note: add `+ 0` to transform is.na in numeric
            rowsum(probs, ids_factor, na.rm = TRUE) /
                rowsum((!is.na(probs)) + 0, ids_factor)
        },
        "majority" = {
            # Define, for each pixel, the index of its highest-probability class
            pixel_class <- max.col(probs, ties.method = "first")
            # Compare every winner pixel class against each class index
            pixel_votes <- outer(pixel_class, seq_len(n_labels), `==`) + 0
            # Define, for each segment, the votes for each class
            rowsum(pixel_votes, ids_factor, na.rm = TRUE)
        },
        "median" = {
            # Divide the pixel row numbers into into groups
            groups <- split(seq_len(nrow(probs)), ids_factor)
            # Stack median vectors by segment
            do.call(rbind, purrr::map(groups, function(rows) {
                apply(
                    probs[rows, , drop = FALSE], 2L, stats::median, na.rm = TRUE
                )
            }))
        }
    )
    # Assign a class only to segments with at least one valid pixel
    n_valid <- rowsum((rowSums(!is.na(probs)) > 0) + 0, ids_factor)[, 1L]
    # Define class indices (NA is the default for a segment with no valid pixel)
    class_idx <- rep(NA_integer_, length(segment_ids))
    # Define which segments actually get a class
    labelled <- n_valid > 0
    # For each labelled segment, define the class-column with the largest
    # aggregate (mean / vote count / median)
    class_idx[labelled] <- max.col(
        agg[labelled, , drop = FALSE],
        ties.method = "first"
    )
    # Return ids and class_idx
    list(
        ids = segment_ids,
        class_idx = class_idx
    )
}

#' @title Label the probs maps with the most probable class
#' @name .label_fn_majority
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @description Build a classified map from probs cube
#' based on maximal probability
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
#' @title    Label a classified vector cube
#' @name .label_gpkg_file
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @description Extract the labels required by sits from GPKG file
#' @param gpkg_file    File in GPKG format
#' @noRd
#' @returns    labels required by sits
.label_gpkg_file <- function(gpkg_file) {
    sf <- sf::st_read(gpkg_file, quiet = TRUE)
    # Extract the labels required by sits from GPKG file
    setdiff(colnames(sf), c(
        "supercells", "x", "y",
        "pol_id", "geom", "class"
    ))
}
