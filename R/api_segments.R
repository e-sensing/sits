.segments_tile <- function(tile,
                           seg_fn,
                           band,
                           block,
                           roi,
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
        if (.check_messages()) {
            message("Recovery: tile '", tile[["tile"]], "' already exists.")
            message(
                "(If you want to produce a new segmentation, please ",
                "change 'output_dir' or 'version' parameters)"
            )
        }
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
        chunks <- .chunks_filter_spatial(chunks, roi = roi)
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
        values <- .segments_data_read(tile = tile, block = block)
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Fill with zeros remaining NA pixels
        values <- C_fill_na(values, 0)
        # Used to check values (below)
        input_pixels <- nrow(values)
        # Apply segment function
        values <- seg_fn(values, block, bbox)
        # Check if the result values is a vector object
        .check_vector(values)
        # Prepare and save results as vector
        .vector_write_vec(v_obj = values, file_path = block_file)
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
    return(TRUE)
}

.segments_data_read <- function(tile, block) {
    # For cubes that have a time limit to expire (MPC cubes only)
    tile <- .cube_token_generator(tile)
    # Read and preprocess values of cloud
    # Get cloud values (NULL if not exists)
    cloud_mask <- .tile_cloud_read_block(tile = tile, block = block)
    # Get tile bands
    tile_bands <- .tile_bands(tile, add_cloud = FALSE)
    # Read and preprocess values of each band
    values <- purrr::map_dfc(tile_bands, function(band) {
        # Get band values (stops if band not found)
        values <- .tile_read_block(tile = tile, band = band, block = block)
        # Remove cloud masked pixels
        if (.has(cloud_mask)) {
            values[cloud_mask] <- NA
        }
        # use linear imputation
        impute_fn <- .impute_linear()
        # are there NA values? interpolate them
        if (any(is.na(values))) {
            values <- impute_fn(values)
        }
        # Return values
        as.data.frame(values)
    })
    # Compose final values
    values <- as.matrix(values)
    # Set values features name
    colnames(values) <- .pred_features_name(tile_bands, .tile_timeline(tile))
    # Return values
    values
}

.get_segments_from_cube <- function(cube) {
    slider::slide_dfr(cube, function(tile) {
        .segments_read_vec(tile)
    })
}

.segments_path <- function(cube) {
    slider::slide_chr(cube, function(tile) {
        tile[["vector_info"]][[1]][["path"]]
    })
}

.segments_read_vec <- function(cube) {
    tile <- .tile(cube)
    vector_seg <- .vector_read_vec(.segments_path(tile))

    return(vector_seg)
}

.segments_join_probs <- function(data, segments, aggregate) {
    # Select polygon_id and class for the time series tibble
    data_id <- data |>
        dplyr::select("polygon_id", "predicted") |>
        dplyr::mutate(polygon_id = as.numeric(.data[["polygon_id"]])) |>
        tidyr::unnest(cols = "predicted") |>
        dplyr::select(-"class") |>
        dplyr::group_by(.data[["polygon_id"]])
    # Select just probability labels
    labels <- setdiff(colnames(data_id), c("polygon_id", "from", "to", "class"))

    if (aggregate) {
        data_id <- data_id |>
            dplyr::summarise(dplyr::across(.cols = dplyr::all_of(labels), stats::median)) |>
            dplyr::rowwise() |>
            dplyr::mutate(sum = sum(dplyr::c_across(cols = dplyr::all_of(labels)))) |>
            dplyr::mutate(dplyr::across(.cols = dplyr::all_of(labels), ~ .x / .data[["sum"]])) |>
            dplyr::select(-"sum")
    }

    # join the data_id tibble with the segments (sf objects)
    dplyr::left_join(segments, data_id, by = c("pol_id" = "polygon_id"))
}
#'
#' @name .segments_extract_data
#' @keywords internal
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param tile       tile of regular data cube
#' @param bands      bands used in time series
#' @param pol_id     ID attribute for polygons.
#' @param n_sam_pol  Number of samples per polygon to be read.
#' @param multicores Number of cores to use for processing
#' @param progress   Show progress bar?
#'
.segments_extract_data <- function(tile,
                                   bands,
                                   pol_id,
                                   n_sam_pol,
                                   multicores,
                                   progress) {

    segs_tile <- .segments_read_vec(tile)
    segs_tile[["part_id"]] <- .partitions(x = seq_len(nrow(segs_tile)),
                                          n = multicores)
    segs_tile[["n_sam_pol"]] <- n_sam_pol
    # reorganize as a nested data frame
    segs_tile <- tidyr::nest(segs_tile, polygons = -"part_id")

    # prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # get the samples in parallel using tile-band combination
    samples <- .jobs_map_parallel_dfr(
        segs_tile,
        function(sf_part){
            segments <- sf_part[["polygons"]][[1]]
            values_seg <- .segments_get_ts(tile = tile,
                                           bands = bands,
                                           segments = segments)
            return(values_seg)
        })
    return(samples)
}
#' @title Extract time series from segments by tile and band
#'
#' @name .segments_get_ts
#' @noRd
#' @description     Using the segments as polygons
#'
#' @param tile        Tile of regular data cube
#' @param bands       Bands to extract time series
#' @param segments     Segments to extract time series
.segments_get_ts <- function(tile,
                             bands,
                             segments) {

    # Extract band values from
    ts_bands <- purrr::map(bands, function(band) {
        # get the scale factors, max, min and missing values
        band_params <- .tile_band_conf(tile, band)
        missing_value <- .miss_value(band_params)
        minimum_value <- .min_value(band_params)
        maximum_value <- .max_value(band_params)
        scale_factor <- .scale(band_params)
        offset_value <- .offset(band_params)
        # extract the values
        values <- .tile_extract_segments(tile, band, segments)
        pol_id <- values[,"pol_id"]
        values <- values[, -1:0]
        # adjust maximum and minimum values
        values[values == missing_value] <- NA
        values[values < minimum_value] <- NA
        values[values > maximum_value] <- NA
        # use linear imputation
        impute_fn <- .impute_linear()
        # are there NA values? interpolate them
        if (any(is.na(values))) {
            values <- impute_fn(values)
        }
        # correct the values using the scale factor
        values <- values * scale_factor + offset_value
        # Returning extracted time series
        return(list(pol_id, c(t(unname(values)))))
    })
    pol_id <- ts_bands[[1]][[1]]
    ts_bands <- purrr::map(ts_bands, function(ts_band) ts_band[[2]])
    names(ts_bands) <- bands
    ts_bands <-  tibble::as_tibble(ts_bands)
    n_dates <- length(.tile_timeline(tile))
    n_samples <- nrow(ts_bands)/n_dates
    ts_bands[["sample_id"]] <- rep(seq_len(n_samples),
                                   each = n_dates)
    ts_bands[["Index"]] <- rep(.tile_timeline(tile), times = n_samples)
    ts_bands <- tidyr::nest(ts_bands, time_series = c("Index", bands))
    ts_bands[["polygon_id"]] <- pol_id
    ts_bands <- tidyr::nest(ts_bands, points = c("sample_id", "time_series"))
    # include lat/long information
    lat_long <- .proj_to_latlong(segments$x, segments$y, .crs(tile))
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
    samples <- tidyr::unnest(samples, cols = "points")
    # sample the values
    n_sam_pol <- segments[1,]$n_sam_pol
    samples <- dplyr::slice_sample(samples,
                                   n = n_sam_pol,
                                   by = "polygon_id")
    samples <- .discard(samples, "sample_id")
    # set sits class
    class(samples) <- c("sits", class(samples))
    return(samples)
}
