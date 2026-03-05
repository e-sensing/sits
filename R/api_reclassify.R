#' @title Reclassify tile
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  tile.           Subset of a data cube
#' @param  mask            Reclassification mask
#' @param  band            Output band
#' @param  labels          Output labels
#' @param  reclassify_fn   Function to be applied for reclassification
#' @param  block           Image block to be processed
#' @param  multicores      Number of cores to run the function
#' @param  memsize         Maximum overall memory (in GB) to run the function
#' @param  output_dir      Directory where image will be save
#' @param  version         Version of result.
#' @param  progress        Show progress bar?
#' @return reclassified tile
.reclassify_tile <- function(tile, mask, band, labels, reclassify_fn, block,
                             multicores, memsize, output_dir, version, progress) {
    # Output files
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        class_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            derived_class = "class_cube",
            labels = labels,
            update_bbox = FALSE
        )
        # Update tile labels
        class_tile <- .tile_update_label(
            tile = class_tile,
            labels = labels,
            multicores = multicores,
            memsize = memsize
        )
        return(class_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0L, block = block)
    # start parallel process
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Output file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
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
        if (all(.raster_is_valid(block_file))) {
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
            nlayers = 1L, miss_value = .miss_value(band_conf),
            data_type = .data_type(band_conf)
        )
        # Copy values from mask cube into mask template
        .gdal_merge_into(
            file = mask_block_file,
            base_files = .fi_paths(.fi(mask)), multicores = 1L
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
        # Does values is valid? In case of a matrix with integer(0) values
        if (.has_not(values)) {
            values <- rep(NA, .block_size(block))
        }
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0.0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1.0) {
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
    }, progress = progress)
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
    # Update tile labels
    class_tile <- .tile_update_label(
        tile = class_tile,
        labels = labels,
        multicores = multicores,
        memsize = memsize
    )
    # Return class tile
    class_tile
}

#' @title Reclassify function
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  rules           Rules to be applied
#' @param  labels_cube     Labels of input cube
#' @param  labels_mask     Labels of reclassification mask
#' @param  exclude_mask_na Set NA to output when an NA is found in mask?
#' @return function to be applied for reclassification
.reclassify_fn_expr <- function(rules, labels_cube, labels_mask,
                                exclude_mask_na) {
    .check_set_caller(".reclassify_fn_expr")
    # Check if rules are named
    .check_that(all(.has_name(rules)))
    # Get output labels
    labels_rule <- setdiff(names(rules), labels_cube)
    names(labels_rule) <- max(.as_int(names(labels_cube))) +
        seq_along(labels_rule)
    labels <- c(labels_cube, labels_rule)
    labels_code <- .as_int(names(labels))
    names(labels_code) <- unname(labels)

    label_as_int <- function(label, lut) {
        valid_label <- label %in% names(lut)
        if (!all(valid_label)) {
            warning(
                sprintf(
                    .conf("messages", ".reclassify_label_as_int"),
                    paste0(label[!valid_label], collapse = ", ")
                ),
                call. = FALSE
            )
            label <- label[valid_label]
            if (!.has(label)) {
                stop(.conf("messages", ".reclassify_label_invalid"))
            }
        }
        unname(lut[label])
    }

    # Internal function to convert label expressions into integer expressions
    rule_as_int <- function(rule) {
        luts <- list(cube = labels_cube, mask = labels_mask)
        luts <- lapply(luts, function(x) {
            lut <- as.integer(names(x))
            names(lut) <- unname(x)
            lut
        })

        env <- list(
            `(` = function(x) {
                if (!is.call(x)) {
                    return(x)
                }
                as.call(list(as.symbol("("), x))
            },
            `!` = function(x) {
                as.call(list(as.symbol("!"), x))
            },
            `|` = function(e1, e2) {
                as.call(list(as.symbol("|"), e1, e2))
            },
            `&` = function(e1, e2) {
                as.call(list(as.symbol("&"), e1, e2))
            },
            `==` = function(e1, e2) {
                if (is.character(e1) && is.symbol(e2)) {
                    e1 <- label_as_int(e1, luts[[as.character(e2)]])
                } else if (is.character(e2) && is.symbol(e1)) {
                    e2 <- label_as_int(e2, luts[[as.character(e1)]])
                } else {
                    stop(.conf("messages", ".reclassify_rule_as_int_eq_op"))
                }
                as.call(list(as.symbol("=="), e1, e2))
            },
            `!=` = function(e1, e2) {
                if (is.character(e1) && is.symbol(e2)) {
                    e1 <- label_as_int(e1, luts[[as.character(e2)]])
                } else if (is.character(e2) && is.symbol(e1)) {
                    e2 <- label_as_int(e2, luts[[as.character(e1)]])
                } else {
                    stop(.conf("messages", ".reclassify_rule_as_int_neq_op"))
                }
                as.call(list(as.symbol("!="), e1, e2))
            },
            `%in%` = function(x, table) {
                if (!is.symbol(x)) {
                    stop(.conf("messages", ".reclassify_rule_as_int_in_op_lhs"))
                }
                if (!is.character(table)) {
                    stop(.conf("messages", ".reclassify_rule_as_int_in_op_rhs"))
                }
                table <- label_as_int(table, luts[[as.character(x)]])
                as.call(list(as.symbol("%in%"), x, table))
            },
            `c` = function(...) {
                x <- c(...)
                if (!is.character(x)) {
                    stop(.conf("messages", ".reclassify_rule_as_int_c_fn"))
                }
                x
            },
            `is.na` = function(x) {
                as.call(list(as.symbol("is.na"), x))
            }
        )

        # Define namespaces from luts
        ns <- lapply(names(luts), as.symbol)
        names(ns) <- names(luts)
        env <- c(env, ns)

        eval(rule, envir = env, enclos = emptyenv())
    }

    # Convert labels to its respective integer values
    rules <- lapply(rules, rule_as_int)

    # Define reclassify function
    reclassify_fn <- function(values, mask_values) {
        # Check compatibility
        if (!all(dim(values) == dim(mask_values))) {
            stop(.conf("messages", ".reclassify_fn_cube_mask"))
        }
        # Used to check values (below)
        input_pixels <- nrow(values)
        # New evaluation environment
        env <- list(
            # Read values and convert to character
            cube = values,
            mask = mask_values
        )
        # Evaluate each expression
        for (label in names(rules)) {
            # Get expression
            expr <- rules[[label]]
            # Evaluate
            result <- eval(expr, envir = env)
            # Update values
            if (!is.logical(result)) {
                stop(.conf("messages", ".reclassify_fn_result"))
            }
            values[result] <- labels_code[[label]]
        }
        # Mask NA values
        if (exclude_mask_na) {
            values[is.na(mask_values)] <- NA
        }
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }

    # Return closure
    reclassify_fn
}

#' @title Obtain new labels on reclassification operation
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  cube            Labelled data cube
#' @param  rules           Rules to be applied
#' @return new labels to be applied to the cube

.reclassify_new_labels <- function(cube, rules) {
    # Get cube labels
    cube_labels <- .cube_labels(cube)
    # Get rules new labels
    new_labels <- setdiff(names(rules), cube_labels)
    # Does rules has new labels in the composition?
    if (.has(new_labels)) {
        # Get the next index
        next_idx <- max(as.numeric(names(cube_labels))) + 1L
        idx_values <- seq.int(
            from = next_idx, to = next_idx + length(new_labels) - 1L
        )
        names(new_labels) <- as.character(idx_values)
    }
    c(cube_labels, new_labels)
}

#' @title Reclassify a probs vector segments from a tile
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param tile     Tile of data cube
#' @param band     Spectral band
#' @param output_dir Directory where file will be saved
#' @param version  Version name
#' @return         Probs vector tile
.reclassify_vector_tile <- function(tile, rules, band, version, output_dir) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = "probs", version = version,
        output_dir = output_dir, ext = "gpkg"
    )
    # Resume feature
    if (all(.segments_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        # Create tile based on template
        class_tile <- .tile_segments_from_file(
            file = out_file,
            band = "probs",
            base_tile = tile,
            labels = .tile_labels(tile),
            vector_class = "probs_vector_cube",
            update_bbox = FALSE
        )
        # Return classified vector tile
        return(class_tile)
    }
    # Get tile labels
    tile_labels <- unname(.tile_labels(tile))
    # Read probability segments
    probs_segments <- .segments_read_vec(tile)
    # Get source labels in rules
    labels_rhs <- unlist(purrr::map(rules, function(expr) {
        eval(as.list(expr)[[3L]])
    }), use.names = FALSE)
    # Get target labels in rules
    labels_lhs <- names(rules)
    # New labels
    new_labels <- sort(c(labels_lhs, setdiff(tile_labels, labels_rhs)))
    # Create a new data
    data_new <- dplyr::select(probs_segments, -dplyr::all_of(labels_rhs))
    # Reclassify each label
    for (idx in seq_len(length(rules))) {
        # Get target and source columns
        target_col <- names(rules[idx])
        source_cols <- eval(as.list(rules[[idx]])[[3]])
        # Get current rule
        rule <- rules[[idx]]
        # Create new column based on rule
        data_new[[target_col]] <- eval(
            expr = parse(text = paste(eval(rule[[3]]), collapse = "+")),
            envir = probs_segments
        )
    }
    # Write all segments
    .vector_write_vec(v_obj = data_new, file_path = out_file)
    # Create class tile based on template and return empty vector tile
    .tile_segments_from_file(
        file = out_file,
        band = "probs",
        base_tile = tile,
        labels = new_labels,
        vector_class = "probs_vector_cube",
        update_bbox = FALSE
    )
}
