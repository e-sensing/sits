sits_segment <- function(cube,
                         seg_fn = sits_supercells_temp(),
                         roi = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         memsize = 8,
                         multicores = 2,
                         output_dir,
                         version = "v1",
                         progress = TRUE) {
    # Preconditions
    .check_is_raster_cube(cube)
    .check_is_regular(cube)
    .check_memsize(memsize)
    .check_output_dir(output_dir)

    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    # Temporal filter
    if (.has(start_date) || .has(end_date)) {
        cube <- .cube_filter_interval(
            cube = cube, start_date = start_date, end_date = end_date
        )
    }
    start_date <- .default(start_date, .cube_start_date(cube))
    end_date <- .default(end_date, .cube_end_date(cube))

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(cube)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_seg")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize, block = block,
        image_size = .tile_size(.tile(cube)), memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores, output_dir = output_dir)
    on.exit(.parallel_stop(), add = TRUE)
    # Segmentation
    # Process each tile sequentially
    segs_cube <- .cube_foreach_tile(cube, function(tile) {
        # Segment the data
        segs_tile <- .segment_tile(
            tile = tile,
            seg_fn = seg_fn,
            band = "segment",
            block = block,
            roi = roi,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
        return(segs_tile)
    })
    return(segs_cube)
}

.segment_tile <- function(tile,
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
        seg_tile <- .segments_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            derived_class = "segs_cube",
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
            pattern = paste0(hash_bundle, "_segment"),
            block = block,
            output_dir = output_dir,
            ext = "gpkg"
        )
        # Resume processing in case of failure
        if (.segment_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .segment_data_read(tile = tile, block = block)
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
        .vector_write_vec(v_obj = values, file = block_file)
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
        derived_class = "segs_cube",
        out_file = out_file,
        update_bbox = update_bbox
    )
    # Delete segments blocks
    unlink(block_files)
    # Return segments tile
    seg_tile
}

.tile_segment_merge_blocks <- function(block_files, base_tile, band, derived_class,
                                       out_file, update_bbox = FALSE) {
    base_tile <- .tile(base_tile)
    # Get conf band
    band_conf <- .conf_derived_band(
        derived_class = derived_class,
        band = band
    )
    # Set base tile
    base_file <- if (update_bbox) NULL else .tile_path(base_tile)
    # Read all blocks file
    vec_segments <- purrr::map_dfr(block_files, .vector_read_vec.sf)
    # Write all segments
    .vector_write_vec(v_obj = vec_segments, file = out_file)
    # Create tile based on template
    tile <- .segments_derived_from_file(
        file = file,
        band = band,
        base_tile = base_tile,
        derived_class = derived_class,
        update_bbox = update_bbox
    )
    # If all goes well, delete block files
    unlink(block_files)
    # Return derived tile
    tile
}

.segment_is_valid <- function(file) {
    # resume processing in case of failure
    if (!all(file.exists(file))) {
        return(FALSE)
    }
    # try to open the file
    s_obj <- .try(
        {
            sf::st_read(file)
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

.segment_data_read <- function(tile, block) {
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
    colnames(values) <- .features_name(tile_bands, .tile_timeline(tile))
    # Return values
    values
}

.features_name <- function(bands, timeline) {
    n <- length(timeline)
    c(vapply(
        X = bands,
        FUN = function(band) paste0(band, seq_len(n)),
        FUN.VALUE = character(n),
        USE.NAMES = FALSE
    ))
}

sits_supercells_temp <- function(data = NULL,
                                 step = 50,
                                 compactness = 1,
                                 dist_fun = "euclidean",
                                 avg_fun = "mean",
                                 iter = 10,
                                 minarea = 30,
                                 verbose = FALSE) {
    function(data, block, bbox) {
        # Create a template rast
        v_obj <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = 1, crs = bbox[["crs"]]
        )
        # Get raster dimensions
        mat <- as.integer(c(.raster_nrows(v_obj), .raster_ncols(v_obj)))
        # Get caller function and call it
        fn <- get("run_slic",
                  envir = asNamespace("supercells"),
                  inherits = FALSE
        )
        slic <- fn(
            mat = mat, vals = data, step = step, nc = compactness,
            con = TRUE, centers = TRUE, type = dist_fun,
            type_fun = function() "", avg_fun_fun = function() "",
            avg_fun_name = avg_fun, iter = iter, lims = minarea,
            input_centers = matrix(c(0L, 0L), ncol = 2),
            verbose = as.integer(verbose)
        )
        # Set values and NA value in template raster
        v_obj <- .raster_set_values(v_obj, slic[[1]])
        v_obj <- .raster_set_na(v_obj, -1)
        # Polygonize raster and convert to sf object
        v_obj <- .raster_polygonize(v_obj, dissolve = TRUE)
        v_obj <- sf::st_as_sf(v_obj)
        if (nrow(v_obj) == 0) {
            return(v_obj)
        }
        # Add an ID for each segments
        names(v_obj) <- c("supercells", "geometry")
        v_obj[["supercells"]] <- v_obj[["supercells"]] + 1
        # Get only polygons segments
        v_obj <- suppressWarnings(
            sf::st_collection_extract(v_obj, "POLYGON")
        )
        # Return the segment object
        return(v_obj)
    }
}

.segments_derived_from_file <- function(file, band, base_tile, derived_class,
                                        update_bbox = FALSE) {
    v_obj <- .vector_open_vec(file)
    base_tile <- .tile(base_tile)
    bbox <- .vector_bbox(v_obj)
    if (update_bbox) {
        # Update spatial bbox
        .xmin(base_tile) <- bbox[["xmin"]]
        .xmax(base_tile) <- bbox[["xmax"]]
        .ymin(base_tile) <- bbox[["ymin"]]
        .ymax(base_tile) <- bbox[["ymax"]]
        .crs(base_tile) <- .vector_crs(v_obj, wkt = TRUE)
    }
    # Update file_info
    .fi(base_tile) <- .fi_segment_from_file(
        file = file,
        base_tile = base_tile,
        band = band,
        start_date = .tile_start_date(base_tile),
        end_date = .tile_end_date(base_tile)
    )
    # Set tile class and return tile
    .cube_set_class(base_tile, .conf_derived_s3class(derived_class))
}

.fi_segment_from_file <- function(file, base_tile, band, start_date, end_date) {
    file <- .file_normalize(file)
    v_obj <- .vector_open_vec(file)
    bbox <- .vector_bbox(v_obj)
    .fi_derived(
        band = band,
        start_date = start_date,
        end_date = end_date,
        ncols = .tile_ncols(base_tile),
        nrows = .tile_nrows(base_tile),
        xres = .tile_xres(base_tile),
        yres = .tile_yres(base_tile),
        xmin = bbox[["xmin"]],
        xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]],
        ymax = bbox[["ymax"]],
        path = file
    )
}
