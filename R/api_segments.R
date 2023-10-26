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
#' @param seg_tile_bands tibble to be processed in parallel
#' @param tile       tile of regular data cube
#' @param bands      bands used in time series
#' @param pol_id     ID attribute for polygons.
#' @param n_sam_pol  Number of samples per polygon to be read.
#' @param multicores Number of cores to use for processing
#' @param progress   Show progress bar?
#'
#' @return  samples associated to segments
#'
.segments_extract_data <- function(seg_tile_bands,
                                   tile,
                                   bands,
                                   pol_id,
                                   n_sam_pol,
                                   multicores,
                                   progress) {

    # multicores is less or equal to number of bands
    multicores <- min(multicores, length(bands))
    # prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Extract band values
    ts_bands <- .jobs_map_parallel(seg_tile_bands, function(seg_tile_band) {
        # get the scale factors, max, min and missing values
        band_params <- seg_tile_band[["params"]][[1]]
        missing_value <- .miss_value(band_params)
        minimum_value <- .min_value(band_params)
        maximum_value <- .max_value(band_params)
        scale_factor <- .scale(band_params)
        offset_value <- .offset(band_params)
        # extract the values
        values <- .tile_extract_segments(seg_tile_band)
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
    }, progress = progress)
    # extract the pol_id information from the first element of the list
    pol_id <- ts_bands[[1]][[1]]
    # remove the first element of the each list and retain the second
    ts_bands <- purrr::map(ts_bands, function(ts_band) ts_band[[2]])
    # rename the resulting list
    names(ts_bands) <- bands
    # transform the list to a tibble
    ts_bands <-  tibble::as_tibble(ts_bands)
    # retrieve the dates of the tile
    n_dates <- length(.tile_timeline(tile))
    # find how many samples have been extracted from the tile
    n_samples <- nrow(ts_bands)/n_dates
    # include sample_id information
    ts_bands[["sample_id"]] <- rep(seq_len(n_samples),
                                   each = n_dates)
    # include timeline
    ts_bands[["Index"]] <- rep(.tile_timeline(tile), times = n_samples)
    # nest the values by bands
    ts_bands <- tidyr::nest(ts_bands, time_series = c("Index", bands))
    # include the ids of the polygons
    ts_bands[["polygon_id"]] <- pol_id
    # nest the values by sample_id and time_series
    ts_bands <- tidyr::nest(ts_bands, points = c("sample_id", "time_series"))
    # retrieve the segments
    segments <- .vector_read_vec(seg_tile_bands[["segs_path"]][1])
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
    # unnest to obtain the samples.
    samples <- tidyr::unnest(samples, cols = "points")
    # sample the values
    samples <- dplyr::slice_sample(samples,
                                   n = n_sam_pol,
                                   by = "polygon_id")
    samples <- .discard(samples, "sample_id")
    # set sits class
    class(samples) <- c("sits", class(samples))
    return(samples)
}
#' @title Split tile bands for extraction of values inside segments
#' @name .segments_split_tile_bands
#' @keywords internal
#' @noRd
#' @param tile   input tile
#' @param bands  bands where data will be extracted
#'
#' @return tibble with band-files pairs
#'
.segments_split_tile_bands <- function(tile, bands){
    tile_bands <- purrr::map(bands, function(band){
        band_files <- .fi_filter_bands(.fi(tile), band)$path
        tibble::tibble(
            band = band,
            files = list(band_files),
            segs_path = .segments_path(tile),
            params = list(.tile_band_conf(tile, band))
        )
    })
    tile_bands <- dplyr::bind_rows(tile_bands)
    return(tile_bands)
}
#' @title Split tile bands for extraction of values inside segments
#' @name .segments_split_tile_bands_list
#' @keywords internal
#' @noRd
#' @param tile   input tile
#' @param bands  bands where data will be extracted
#' @param segments large set of segments
#' @param n_iterations number of parts to break the segments
#' @param output_dir directory to write the segments
#' @return list of tibbles with band-files pairs
#'
.segments_split_tile_bands_list <- function(tile, bands, segments,
                                            n_iterations, output_dir){

    segments[["group"]] <- rep(
        seq_len(n_iterations), each = ceiling(nrow(segments) / n_iterations)
    )[seq_len(nrow(segments))]

    segments_lst <- dplyr::group_split(
        dplyr::group_by(segments, .data[["group"]])
    )
    segment_files <- purrr::map_chr(segments_lst, function(seg_part){
        # Block file name
        hash_bundle <- digest::digest(seg_part, algo = "md5")
        seg_file <- .file_path(paste0(hash_bundle, "_segments"),
                               ext = "gpkg",
                               output_dir = output_dir)
        .vector_write_vec(seg_part, seg_file)
        return(seg_file)
    })
    seg_tile_band_lst <- purrr::map(segment_files, function(seg_file) {
        tile_bands <- purrr::map(bands, function(band){
            band_files <- .fi_filter_bands(.fi(tile), band)$path
            tibble::tibble(
                band = band,
                files = list(band_files),
                segs_path = seg_file,
                params = list(.tile_band_conf(tile, band))
            )
        })
        seg_tile_band <- dplyr::bind_rows(tile_bands)
        return(seg_tile_band)
    })
    return(seg_tile_band_lst)
}
#'
#'
#' @name .segments_extract_multicores
#' @keywords internal
#' @noRd
#' @description     Using the segments as polygons, get all time series
#'
#' @param tile       tile of regular data cube
#' @param bands      bands used in time series
#' @param pol_id     ID attribute for polygons.
#' @param n_sam_pol  Number of samples per polygon to be read.
#' @param multicores Number of cores to use for processing
#' @param memsize    Memory available for processing
#' @param output_dir Directory for saving temporary segment files
#' @param progress   Show progress bar?
#'
.segments_extract_multicores <- function(tile,
                                         bands,
                                         pol_id,
                                         n_sam_pol,
                                         multicores,
                                         memsize,
                                         output_dir,
                                         progress) {

    # how much memory do we need?
    req_memory <- .tile_nrows(tile)  * .tile_ncols(tile) *
        length(.tile_timeline(tile)) * length(bands) * 4 *
        .conf("processing_bloat_seg") / 1e+09

    # do we have enough memory?
    if (req_memory < memsize) {
        seg_tile_bands <- .segments_split_tile_bands(tile, bands)
        samples <- .segments_extract_data(seg_tile_bands,
                                          tile,
                                          bands,
                                          pol_id,
                                          n_sam_pol,
                                          multicores,
                                          progress)
    } else {
        # how many iterations do we need?
        n_iterations <- ceiling(req_memory / memsize)
        # retrieve the segments for the tile
        segments <- .segments_read_vec(tile)
        seg_tile_bands_lst <- .segments_split_tile_bands_list(
            tile = tile,
            bands = bands,
            segments = segments,
            n_iterations = n_iterations,
            output_dir = output_dir)
        samples <- purrr::map(seg_tile_bands_lst, function(seg_tile_bands){
            samples_part <- .segments_extract_data(seg_tile_bands,
                                              tile,
                                              bands,
                                              pol_id,
                                              n_sam_pol,
                                              multicores,
                                              progress)
            return(samples_part)
        })
        samples <- dplyr::bind_rows(samples)
    }
    return(samples)
}
