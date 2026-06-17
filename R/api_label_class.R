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
#'   assigns a class per segment, and rasterizes the result. Also writes
#'   segment summaries (with a 'class' column) to the associated GPKG.
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
    # Output GPKG file for segment summaries
    out_gpkg <- .file_derived_name(
        tile = tile, band = band, version = version,
        output_dir = output_dir, ext = "gpkg"
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
        .cube_set_class(class_tile, vector_classes)
        return(class_tile)
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
    extracted <- terra::extract(
        x = probs_rast,
        y = terra::vect(segments),
        fun = NULL
    )
    # Get the label method closure
    method_fn <- .label_method_fn(label_method)
    # Probability columns (all bands in the probs raster)
    prob_cols <- setdiff(colnames(extracted), "ID")
    # Aggregate probabilities per segment and assign class
    segment_ids <- sort(unique(extracted[["ID"]]))
    seg_results <- lapply(segment_ids, function(sid) {
        seg_pixels <- extracted[extracted[["ID"]] == sid, prob_cols,
            drop = FALSE
        ]
        if (nrow(seg_pixels) == 0L || all(is.na(seg_pixels))) {
            return(list(
                id = sid,
                class_idx = NA_integer_,
                class_name = NA_character_
            ))
        }
        # Apply label method: aggregate + decide
        class_idx <- method_fn(as.matrix(seg_pixels))
        class_name <- labels[[class_idx]]
        list(id = sid, class_idx = class_idx, class_name = class_name)
    })
    # Build lookup: segment index -> class index
    seg_class_idx <- vapply(seg_results, `[[`, integer(1L), "class_idx")
    seg_class_name <- vapply(seg_results, `[[`, character(1L), "class_name")
    # Add class column to segments and write GPKG summary
    segments[["class"]] <- NA_character_
    segments[["class"]][segment_ids] <- seg_class_name
    .vector_write_vec(v_obj = segments, file_path = out_gpkg)
    # Rasterize: assign class index to all pixels within each segment
    seg_vect <- terra::vect(segments[segment_ids, ])
    seg_vect[["class_value"]] <- seg_class_idx
    # Create output raster from template
    template_rast <- terra::rast(probs_rast, nlyrs = 1L)
    band_conf <- .conf_derived_band(
        derived_class = "class_cube", band = band
    )
    # Rasterize segments onto the template
    class_rast <- terra::rasterize(
        x = seg_vect,
        y = template_rast,
        field = "class_value",
        fun = "max"
    )
    # Set missing value
    terra::NAflag(class_rast) <- .miss_value(band_conf)
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
    # Preserve vector_info (pointing to the updated GPKG)
    vi <- tile[["vector_info"]]
    vi[[1L]][["path"]] <- out_gpkg
    class_tile[["vector_info"]] <- vi
    # Set class_vector_cube + class_cube chain
    vector_classes <- c(
        .conf_vector_s3class("class_vector_cube"),
        class(class_tile)
    )
    .cube_set_class(class_tile, vector_classes)
}

#' @title Get label method function by name
#' @name .label_method_fn
#' @keywords internal
#' @noRd
#' @param label_method  Label method name ("mean", "median", "majority").
#' @return              A closure that takes a numeric matrix (pixels x classes)
#'                      and returns an integer index of the chosen class.
.label_method_fn <- function(label_method) {
    switch(label_method,
        "mean" = function(probs) {
            which.max(colMeans(probs, na.rm = TRUE))
        },
        "median" = function(probs) {
            which.max(apply(probs, 2L, stats::median, na.rm = TRUE))
        },
        "majority" = function(probs) {
            pixel_classes <- max.col(probs, ties.method = "first")
            # Most frequent per-pixel class
            counts <- tabulate(pixel_classes, nbins = ncol(probs))
            which.max(counts)
        },
        stop("Unknown label method: ", label_method, call. = FALSE)
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
