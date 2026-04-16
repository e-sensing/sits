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
    if (all(.segments_is_valid(out_file, output_dir = output_dir))) {
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
        if (all(.segments_is_valid(block_file))) {
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
        # If there is no segment to write, return NA
        if (nrow(values) == 0) {
            return(NA_character_)
        }
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
    block_files <- block_files[!is.na(block_files)]
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @param files      GKPG files containing the segments
#' @param output_dir  Where to search for cache marker files
#'
#' @return boolean vector indicating which file is missing/corrupted
.segments_is_valid <- function(files, output_dir = NULL) {
    files <- normalizePath(files, mustWork = FALSE)
    exists <- file.exists(files)
    checked_files <- NULL

    # check if files were already checked before
    checked <- rep(FALSE, length(files))
    if (!is.null(output_dir)) {
        checked_files <- .file_path(
            .file_base(files),
            ext = ".check",
            output_dir = file.path(output_dir, ".sits"),
            create_dir = TRUE
        )
        checked <- file.exists(checked_files)
    }

    validate_one <- function(i) {
        if (!exists[i]) {
            return(FALSE)
        }
        if (checked[i]) {
            return(TRUE)
        }

        f <- files[i]

        is_ok <- .try(
            {
                .vector_read_vec(f)
                TRUE
            },
            .default = FALSE
        )

        if (is_ok && !is.null(checked_files)) {
            marker <- checked_files[i]
            tmp <- paste0(marker, ".tmp_", Sys.getpid())
            cat("", file = tmp)
            file.rename(tmp, marker)
        }

        is_ok
    }

    vapply(seq_along(files), validate_one, logical(1))
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
        colnames(values) <- paste(
            colnames(values), seq_len(ncol(values)),
            sep = "_"
        )
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
        dplyr::mutate(polygon_id = as.numeric(.data[["polygon_id"]])) |>
        dplyr::group_by(.data[["polygon_id"]])
    # Select just probability labels
    labels <- setdiff(colnames(data), c("polygon_id", "x", "y", "supercells"))
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
#' @param chunk      A chunk to be read.
#' @param n_sam_pol  Number of samples per polygon to be read.
#'
#' @return  samples associated to segments
.segments_poly_read <- function(tile, chunk, n_sam_pol) {
    # Read chunk segments
    segments <- .vector_read_vec(chunk[["segments"]][[1L]])
    # Sample n points for each polygon
    samples <- .segments_sample(
        segments = segments,
        size     = n_sam_pol
    )
    # Get probabilities values
    rast <- .raster_open_rast(.tile_path(tile))
    # Add labels name for each probability layer
    names(rast) <- .tile_labels(tile)
    # Extract probability values
    samples_probs <- .raster_extract(
        rast = rast, xy = samples, bind = TRUE
    )
    # Discard geometry
    samples_probs <- tibble::as_tibble(samples_probs)
    # Rename polygon id column for further usage
    samples_probs <- dplyr::rename(samples_probs, polygon_id = "pol_id")
    return(samples_probs)
}

#'
#' @name .segments_sample
#' @keywords internal
#' @noRd
#'
#' @description Dedicated function to randomly sampling points in segments.
#'
#' @note This function adapts the \code{terra::spatSample} to process multiple
#'       polygons at once. For this, we use the powerful sampling code from
#'       terra together with the indexing capabilities of \code{sf}.
#'
#' @param segments   \code{sf} object with the segments.
#' @param size       Number of points to sample from each polygon.
#'
#' @return  samples associated to segments
.segments_sample <- function(segments, size) {
    # Important disclaimer:
    # In this function, we are porting the random sampler for vector implemented
    # in the ``terra`` R package. To keep the reference of what we developed and
    # the reasons why we added / removed things, some comments includes the
    # files / lines of the code adapted / copied from ``terra``
    # The commit we used as reference is the following:
    # > ca87f3eb638f20546011558d9266fe6ebd7e7eb4
    # > Author: rhijmans <r.hijmans@gmail.com>
    # > Date:   Sat Apr 11 19:47:05 2026 -0700
    # Now, we can continue to the source code

    # Generate random seed
    # It is ok to generate this randomly, because, if the user defined the
    # base seed, the result of this function will be reproducible.
    seed <- floor(stats::runif(1, min = 1, max = 9999))

    # Define number of segments
    n_features <- nrow(segments)

    # Define number of points per feature. In ``terra``, the code process
    # polygons in a ``lapply``, going polygon by polygon (`sample.R:1083-1084`)
    # So, to simulate this, we basically replicate the number of points the
    # user defined by the number of polygons:
    n_per_feature <- rep(as.integer(size), n_features)

    # Extract geometry column (sfc object)
    geom   <- sf::st_geometry(segments)

    # Compute per-polygon bounding boxes.
    # (``terra/src/sample.cpp:927: SpatVector ve(extent, "")``).
    bbox   <- do.call(rbind, lapply(geom, sf::st_bbox))

    # Compute polygon areas.
    # In terra: ``std::vector<double> a = area("m", true, {})``
    # (terra/src/sample.cpp:918). We use ``sf::st_area`` which also
    # calls GEOS/s2 for geodesic area on lon/lat data.
    areas  <- as.numeric(sf::st_area(segments))

    # Detect lon/lat CRS.
    # In terra: ``bool lonlat = is_lonlat()`` (terra/src/sample.cpp:833).
    lonlat <- isTRUE(sf::st_is_longlat(segments))

    # Now, we are going to start generating the points per polygon.
    # Stage 1: Generate oversampled candidates in C++
    # > Here, we are calling C++ code adapted directly from ``terra``
    candidates <- C_terra_sampling_random_candidates(
        bbox_xmin = bbox[, "xmin"],
        bbox_xmax = bbox[, "xmax"],
        bbox_ymin = bbox[, "ymin"],
        bbox_ymax = bbox[, "ymax"],
        areas     = areas,
        n_points  = n_per_feature,
        lonlat    = lonlat,
        seed      = seed
    )

    # Transform candidates to sf
    candidates_sf  <- sf::st_as_sf(
        x      = candidates,
        coords = c("x", "y"),
        crs    = sf::st_crs(segments)
    )

    # Stage 2: Generate intersection of polygons and candidates
    hit_mat <- sf::st_intersects(geom, candidates_sf)

    # Stage 3: Sample points per polygon
    # (Here, for each polygon, we get points intersecting it. If there are more
    # points in the polygon than expected, we shuffle and select)
    valid_idx <- C_terra_sampling_filter_and_trim(
        hit_mat       = hit_mat,
        poly_ids      = candidates[["pol_id"]],
        n_per_feature = n_per_feature,
        seed          = seed
    )

    # Get valid points
    result <- candidates_sf[valid_idx, ]

    # Map pol_id to original values
    result[["pol_id"]] <- segments[["pol_id"]][result[["pol_id"]]]

    # Return!
    result
}
