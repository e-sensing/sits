#'
#' @name .segments_tile
#' @keywords internal
#' @noRd
#' @description     Extract the segments from a tile
#'
#' @param tile       tile of regular data cube
#' @param seg_fn     Segmentation function to be used
#' @param band       Name of output band
#' @param block      Block size
#' @param roi        Region of interest
#' @param impute_fn  Imputation function to remove NA values
#' @param output_dir Directory for saving temporary segment files
#' @param version    Version of the result
#' @param progress   Show progress bar?
#' @return segments for the tile
.segments_tile <- function(tile,
                           seg_fn,
                           band,
                           block,
                           roi,
                           impute_fn,
                           output_dir,
                           version,
                           progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version,
        output_dir = output_dir, ext = "gpkg"
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
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0, block = block)
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
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        bbox <- .bbox(chunk)
        # Block file name
        hash_bundle <- digest::digest(list(block, seg_fn), algo = "md5")
        block_file <- .file_block_name(
            pattern = paste0(hash_bundle, "_segments"),
            block = block,
            output_dir = output_dir,
            ext = "gpkg"
        )
        # Resume processing in case of failure
        if (.segments_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .segments_data_read(
            tile = tile,
            block = block,
            impute_fn = impute_fn
        )
        # Apply segmentation function
        values <- seg_fn(values, block, bbox)
        # Check if the result values is a vector object
        .check_vector_object(values)
        # Prepare and save results as vector
        .vector_write_vec(
            v_obj = values,
            file_path = block_file
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = progress)
    # Merge blocks into a new segs_cube tile
    seg_tile <- .tile_segment_merge_blocks(
        block_files = block_files,
        base_tile = tile,
        band = "segments",
        vector_class = "segs_cube",
        out_file = out_file,
        update_bbox = update_bbox
    )
    # Delete segments blocks
    unlink(block_files)
    # Return segments tile
    seg_tile
}
#' @name .segments_is_valud
#' @keywords internal
#' @noRd
#' @description     Check if segments file is valid
#'
#' @param file      GKPG file containing the segments
#' @return  TRUE/FALSE
.segments_is_valid <- function(file) {
    # resume processing in case of failure
    if (!all(file.exists(file))) {
        return(FALSE)
    }
    # try to open the file
    s_obj <- .try(
        {
            .vector_read_vec(file)
        },
        .default = {
            unlink(file)
            NULL
        }
    )
    # File is not valid
    if (is.null(s_obj)) {
        return(FALSE)
    }
    TRUE
}

#' @name .segments_data_read
#' @keywords internal
#' @noRd
#' @description     Extract the segments from a tile
#'
#' @param tile       Tile of regular data cube
#' @param block      Block size
#' @param impute_fn  Imputation function for removing NA values
#' @return values of the time series in the block
.segments_data_read <- function(tile, block, impute_fn) {
    # For cubes that have a time limit to expire (MPC cubes only)
    tile <- .cube_token_generator(tile)
    # Read and preprocess values of cloud
    # Get cloud values (NULL if not exists)
    cloud_mask <- .tile_cloud_read_block(
        tile = tile,
        block = block
    )
    # Get tile bands
    tile_bands <- .tile_bands.raster_cube(
        tile = tile,
        add_cloud = FALSE
    )
    # Read and preprocess values of each band
    values <- .map_dfc(tile_bands, function(band) {
        # Get band values (stops if band not found)
        values <- .tile_read_block(
            tile = tile,
            band = band,
            block = block
        )
        # Remove cloud masked pixels
        if (.has(cloud_mask)) {
            values[cloud_mask] <- NA
        }
        # are there NA values? interpolate them
        if (anyNA(values)) {
            values <- impute_fn(values)
        }
        # Return values
        as.data.frame(values)
    })
    # Compose final values
    values <- as.matrix(values)
    # Set values features name
    colnames(values) <- .pred_features_name(
        bands = tile_bands,
        timeline = .tile_timeline(tile)
    )
    # Return values
    values
}

#' @name .segments_path
#' @keywords internal
#' @noRd
#' @description     Find the path to the GPKG file with the segments
#'
#' @param cube       Regular data cube
#' @return GPKG file name
.segments_path <- function(cube) {
    slider::slide_chr(cube, function(tile) {
        tile[["vector_info"]][[1L]][["path"]]
    })
}
#' @name .segments_read_vec
#' @keywords internal
#' @noRd
#' @description     Read the segments associated to a tile
#' @param cube      Regular data cube
#' @return segment vectors (sf object)
.segments_read_vec <- function(cube) {
    tile <- .tile(cube)
    .vector_read_vec(.segments_path(tile))
}
#' @name .segments_join_probs
#' @keywords internal
#' @noRd
#' @description     Join the probabilities of time series inside each
#'                  segment to the segments vectors
#' @param data      Classified time series
#' @param segments  Segments object (sf object)
#' @return segment vectors (sf object) with the probabilities
#'
.segments_join_probs <- function(data, segments) {
    # Select polygon_id and class for the time series tibble
    data <- data |>
        dplyr::select("polygon_id", "predicted") |>
        dplyr::mutate(polygon_id = as.numeric(.data[["polygon_id"]])) |>
        tidyr::unnest(cols = "predicted") |>
        dplyr::select(-"class") |>
        dplyr::group_by(.data[["polygon_id"]])
    # Select just probability labels
    labels <- setdiff(colnames(data), c("polygon_id", "from", "to", "class"))
    # Calculate metrics
    data <- dplyr::summarise(
        data,
        dplyr::across(
            .cols = dplyr::all_of(labels),
            .names = "{.col}_mean", mean
        )
    )
    # Summarize probabilities
    data <- data |>
        dplyr::rename_with(~ gsub("_mean$", "", .x)) |>
        dplyr::rowwise() |>
        dplyr::mutate(
            sum = sum(
                dplyr::c_across(
                    cols = dplyr::all_of(labels)
                )
            )
        ) |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::all_of(labels), ~ .x / .data[["sum"]]
            )
        ) |>
        dplyr::select(-"sum")

    # join the data_id tibble with the segments (sf objects)
    dplyr::left_join(
        x = segments,
        y = data,
        by = c(pol_id = "polygon_id")
    ) |>
        dplyr::filter(.data[["pol_id"]] %in% unique(data[["polygon_id"]]))
}
#'
#' @name .segments_data_read
#' @keywords internal
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param tile       tile of regular data cube
#' @param bands      Bands to extract time series
#' @param base_bands Base bands to extract values
#' @param chunk      A chunk to be read.
#' @param n_sam_pol  Number of samples per polygon to be read.
#' @param impute_fn  Imputation function to remove NA
#'
#' @return  samples associated to segments
.segments_poly_read <- function(tile, bands, base_bands, chunk, n_sam_pol, impute_fn) {
    # define bands variables
    ts_bands <- NULL
    ts_bands_base <- NULL
    # For cubes that have a time limit to expire (MPC cubes only)
    tile <- .cube_token_generator(cube = tile)
    # Read and preprocess values of each band
    ts_bands <- purrr::map(bands, function(band) {
        # extract band values
        .tile_read_segments(
            tile = tile,
            band = band,
            chunk = chunk,
            impute_fn = impute_fn
        )
    })
    # extract the pol_id information from the first element of the list
    pol_id <- ts_bands[[1L]][[1L]]
    # remove the first element of the each list and retain the second
    ts_bands <- purrr::map(ts_bands, function(ts_band) ts_band[[2]])
    # rename the resulting list
    names(ts_bands) <- bands
    # transform the list to a tibble
    ts_bands <- tibble::as_tibble(ts_bands)
    # retrieve the dates of the tile
    n_dates <- length(.tile_timeline(tile))
    # find how many samples have been extracted from the tile
    n_samples <- nrow(ts_bands) / n_dates
    # include sample_id information
    ts_bands[["sample_id"]] <- rep(seq_len(n_samples), each = n_dates)
    # include timeline
    ts_bands[["Index"]] <- rep(
        .tile_timeline(tile),
        times = n_samples
    )
    # nest the values by bands
    ts_bands <- tidyr::nest(
        ts_bands,
        time_series = c("Index", dplyr::all_of(bands))
    )
    # if `base_bands` is available, transform it to the same structure as
    # `time_series`
    if (.has(base_bands)) {
        # read base data values
        ts_bands_base <- purrr::map(base_bands, function(band) {
            .tile_read_segments(
                tile = .tile_base_info(tile),
                band = band,
                chunk = chunk,
                impute_fn = impute_fn
            )
        })
        # remove polygon ids
        ts_bands_base <- purrr::map(
            ts_bands_base,
            function(ts_band) ts_band[[2]]
        )
        # name band values
        names(ts_bands_base) <- base_bands
        # merge band values
        ts_bands_base <- dplyr::bind_cols(ts_bands_base)
        # include time reference in the data
        ts_bands_base[["Index"]] <- rep(
            .tile_timeline(.tile_base_info(tile)),
            times = n_samples
        )
        # include base bands data
        ts_bands <- tibble::add_column(ts_bands, ts_bands_base)
        # nest base data
        ts_bands <- tidyr::nest(
            ts_bands,
            base_data = c("Index", dplyr::all_of(base_bands))
        )
    }
    # include the ids of the polygons
    ts_bands[["polygon_id"]] <- pol_id
    # define which columns must be checked to drop na values
    drop_na_colums <- list("time_series" = bands)
    # if `base_bands` is available, to `base_data` column is used
    if (.has(base_bands)) {
        drop_na_colums[["base_data"]] <- base_bands
    }
    # drop na values
    for (colname in names(drop_na_colums)) {
        # we do the unnest again because we do not know the polygon id index
        ts_bands <- tidyr::unnest(ts_bands, colname)
        # remove pixels where all timeline was NA
        ts_bands <- tidyr::drop_na(ts_bands)
        # nest the values by bands
        ts_bands <- tidyr::nest(
            ts_bands,
            !!colname := c("Index", dplyr::all_of(drop_na_colums[[colname]]))
        )
    }
    # define columns used in the points nest
    points_nest <- c("sample_id", "time_series")
    # if `base_bands` is available, include it in the nest operation
    if (.has(base_bands)) {
        points_nest <- c(points_nest, "base_data")
    }
    # nest the values by sample_id and time_series
    ts_bands <- tidyr::nest(
        ts_bands,
        points = points_nest
    )
    # retrieve the segments
    segments <- .vector_read_vec(chunk[["segments"]][[1]])
    # include lat/long information
    segments <- segments |> dplyr::filter(
        .data[["pol_id"]] %in% unique(ts_bands[["polygon_id"]])
    )
    if (.has_column(segments, "x") && .has_column(segments, "y")) {
        lat_long <- .proj_to_latlong(
            segments[["x"]], segments[["y"]], .crs(tile)
        )
    } else {
        lat_long <- tibble::tibble(
            "longitude" = rep(0.0, nrow(segments)),
            "latitude" = rep(0.0, nrow(segments))
        )
    }

    # create metadata for the polygons
    samples <- tibble::tibble(
        longitude  = lat_long[, "longitude"],
        latitude   = lat_long[, "latitude"],
        start_date = .tile_start_date(tile),
        end_date   = .tile_end_date(tile),
        label      = "NoClass",
        cube       = tile[["collection"]],
        ts_bands
    )
    # unnest to obtain the samples.
    samples <- tidyr::unnest(
        samples,
        cols = "points"
    )
    # sample the values if n_sam_plot is not NULL
    if (.has(n_sam_pol)) {
        samples <- dplyr::slice_sample(
            samples,
            n = n_sam_pol,
            by = "polygon_id"
        )
    }
    samples <- .discard(samples, "sample_id")
    # define `sits_base` if applicable
    if (.has(base_bands)) {
        class(samples) <- c("sits_base", class(samples))
    }
    # set sits class and return
    .set_class(samples, "sits", class(samples))
}
