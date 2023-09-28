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

#' @title Extract set of time series from supercells
#'
#' @name .segments_get_summary
#' @keywords internal
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param cube       regular data cube
#' @param bands      bands used in time series
#' @param aggreg_fn  Function to compute a summary of each segment
#' @param pol_id     ID attribute for polygons.
#' @param multicores Number of cores to use for processing
#' @param progress   Show progress bar?
#'
.segments_get_summary <- function(cube,
                                  bands,
                                  aggreg_fn,
                                  pol_id,
                                  multicores,
                                  progress) {
    # Verify if exactextractr is installed
    .check_require_packages("exactextractr")
    # Get start and end dates
    start_date <- .cube_start_date(cube)
    end_date <- .cube_end_date(cube)
    # Get chunks samples
    chunks_samples <- slider::slide(cube, function(tile) {
        # Get segments from tile
        samples <- .get_segments_from_cube(tile)
        .cube_split_chunks_samples(cube = tile, samples = samples)
    })
    chunks_samples <- unlist(chunks_samples, recursive = FALSE)

    # Set output_dir
    output_dir <- tempdir()
    if (Sys.getenv("SITS_SAMPLES_CACHE_DIR") != "") {
        output_dir <- Sys.getenv("SITS_SAMPLES_CACHE_DIR")
    }
    # To avoid open more process than chunks and samples combinations
    if (multicores > length(chunks_samples)) {
        multicores <- length(chunks_samples)
    }
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    samples_tiles_bands <- .parallel_map(chunks_samples, function(chunk) {
        tile <- sits_select(
            data = cube,
            bands = bands,
            tiles = chunk[["tile"]]
        )
        # Get chunk segments
        segs_tile <- chunk[["samples"]][[1]]
        # create hash for combination of tile and samples
        hash_bundle <- digest::digest(list(tile, segs_tile), algo = "md5")
        # create a file with a hash code
        filename <- .file_path(
            "samples", hash_bundle,
            ext = ".rds",
            output_dir = output_dir
        )
        # test if file exists
        if (file.exists(filename)) {
            tryCatch(
                {
                    # ensure that the file is not corrupted
                    timeseries <- readRDS(filename)
                    return(timeseries)
                },
                error = function(e) {
                    unlink(filename)
                    gc()
                }
            )
        }
        # build the sits tibble for the storing the points
        samples_tbl <- purrr::pmap_dfr(
            list(segs_tile$x, segs_tile$y, segs_tile[[pol_id]]),
            function(x, y, pid) {
                # convert XY to lat long
                lat_long <- .proj_to_latlong(x, y, .crs(cube))

                # create metadata for the polygons
                sample <- tibble::tibble(
                    longitude  = lat_long[1, "longitude"],
                    latitude   = lat_long[1, "latitude"],
                    start_date = start_date,
                    end_date   = end_date,
                    label      = "NoClass",
                    cube       = tile[["collection"]],
                    polygon_id = pid
                )
                # store them in the sample tibble
                sample$time_series <- list(
                    tibble::tibble(Index = .tile_timeline(tile))
                )
                # return valid row of time series
                return(sample)
            }
        )

        # extract time series per tile and band
        ts <- .segments_get_ts(
            tile = tile,
            bands = bands,
            samples_tbl = samples_tbl,
            segs_tile = segs_tile,
            aggreg_fn = aggreg_fn
        )

        ts[["tile"]] <- tile[["tile"]]
        ts[["#..id"]] <- seq_len(nrow(ts))

        return(ts)
    }, progress = progress)
    # join rows to get time series tibble
    ts_tbl <- dplyr::bind_rows(samples_tiles_bands)

    ts_tbl <- ts_tbl |>
        tidyr::unnest("time_series") |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]],
            .data[["Index"]], .data[["tile"]], .data[["#..id"]]
        )

    if ("polygon_id" %in% colnames(ts_tbl)) {
        ts_tbl <- dplyr::group_by(ts_tbl, .data[["polygon_id"]], .add = TRUE)
    }

    ts_tbl <- ts_tbl |>
        dplyr::reframe(
            dplyr::across(dplyr::all_of(bands), stats::na.omit)
        ) |>
        dplyr::arrange(.data[["Index"]]) |>
        dplyr::ungroup() |>
        tidyr::nest(time_series = !!c("Index", bands)) |>
        dplyr::select(-c("#..id"))

    # get the first point that intersect more than one tile
    # eg sentinel 2 mgrs grid
    ts_tbl <- ts_tbl |>
        dplyr::group_by(
            .data[["longitude"]], .data[["latitude"]],
            .data[["start_date"]], .data[["end_date"]],
            .data[["label"]], .data[["cube"]]
        ) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()

    hash_bundle <- purrr::map_chr(chunks_samples, function(chunk) {
        tile <- sits_select(
            data = cube,
            bands = bands,
            tiles = chunk[["tile"]]
        )
        # Get chunk samples
        samples <- chunk[["samples"]][[1]]
        digest::digest(list(tile, samples), algo = "md5")
    })

    # recreate file names to delete them
    # samples will be recycled for each hash_bundle
    temp_timeseries <- .file_path(
        "samples", hash_bundle,
        ext = "rds",
        output_dir = output_dir
    )

    # delete temporary rds
    unlink(temp_timeseries)
    gc()

    if (!inherits(ts_tbl, "sits")) {
        class(ts_tbl) <- c("sits", class(ts_tbl))
    }

    return(ts_tbl)
}

#' @title Extract many time series from each segment
#'
#' @name .segments_get_data
#' @keywords internal
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param cube       regular data cube
#' @param bands      bands used in time series
#' @param pol_id     ID attribute for polygons.
#' @param n_sam_pol  Number of samples per polygon to be read.
#' @param multicores Number of cores to use for processing
#' @param progress   Show progress bar?
#'
.segments_get_data <- function(cube,
                               bands,
                               pol_id,
                               n_sam_pol,
                               multicores,
                               progress) {
    # extract a samples data.frame from sf object
    samples <- slider::slide_dfr(cube, function(tile) {
        samples_tile  <- .sf_get_samples(
            sf_object  = .segments_read_vec(tile),
            label      = "NoClass",
            label_attr = NULL,
            start_date = .cube_start_date(tile),
            end_date   = .cube_end_date(tile),
            n_sam_pol  = n_sam_pol,
            pol_id     = pol_id
        )
        return(samples_tile)
    })

    # extract time series from a cube given a data.frame
    data <- .data_get_ts(
        cube       = cube,
        samples    = samples,
        bands      = bands,
        multicores = multicores,
        progress   = progress
    )
    return(data)
}
#' @title Extract time series from segments by tile and band
#'
#' @name .segments_get_ts
#' @noRd
#' @description     Using the segments as polygons
#'
#' @param tile        Tile of regular data cube
#' @param bands       Bands to extract time series
#' @param samples_tbl Samples tibble
#' @param segs_tile   Polygons produced by sits_supercells for the tile
#' @param aggreg_fn   Aggregation function to compute a summary of each segment
#'
.segments_get_ts <- function(tile,
                             bands,
                             samples_tbl,
                             segs_tile,
                             aggreg_fn) {

    ts_bands <- purrr::map(bands, function(band) {
        # get the scale factors, max, min and missing values
        band_params <- .tile_band_conf(tile, band)
        missing_value <- .miss_value(band_params)
        minimum_value <- .min_value(band_params)
        maximum_value <- .max_value(band_params)
        scale_factor <- .scale(band_params)
        offset_value <- .offset(band_params)
        # extract the values
        values <- .tile_extract_segments(tile, band, segs_tile, aggreg_fn)
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
        return(values)
    })
    # Now we have to transpose the data
    ts_bands <- ts_bands |>
        purrr::set_names(bands) |>
        purrr::map(function(x) t(tibble::as_tibble(x)))

    # join new time series with previous values
    samples_tbl <- slider::slide2_dfr(
        samples_tbl, seq_len(nrow(samples_tbl)),
        function(sample, i) {
            old_ts <- sample$time_series[[1]]
            new_ts <- purrr::map_depth(
                ts_bands, 1, function(x) tibble::tibble(x[, i])
            )
            new_ts <- dplyr::bind_cols(new_ts, .name_repair = ~ bands)
            sample$time_series[[1]] <- dplyr::bind_cols(old_ts, new_ts)
            return(sample)
        }
    )
    # set sits class
    class(samples_tbl) <- c("sits", class(samples_tbl))
    return(samples_tbl)
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
