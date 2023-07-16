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
        seg_tile <- .tile_derived_from_segment(
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
        # Prepare and save results as vector
        .segment_write_block(file = block_file, values = values)
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = progress)
    # Merge blocks into a new segs_cube tile
    seg_tile <- .tile_segment_merge_blocks(
        block_files = block_files, base_tile = tile, band = "segment",
        derived_class = "seg_cube",
        out_file = out_file, update_bbox = update_bbox
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
    # Create a template raster based on the first image of the tile
    vec_segments <- purrr::map_dfr(block_files, sf::st_read)
    sf::st_write(obj = vec_segments, dsn = out_file)

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

.segment_write_block <- function(file, values) {
    sf::st_write(obj = values, dsn = file)
    file
}

sits_supercells_temp <- function(data = NULL,
                                 step = 50,
                                 compactness = 1,
                                 dist_fun = "euclidean",
                                 avg_fun = "mean",
                                 iter = 10,
                                 minarea = 30) {
    function(data, block, bbox) {
        r_obj <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = ncol(data), crs = bbox[["crs"]]
        )
        mat <- c(.raster_nrows(r_obj), .raster_ncols(r_obj))
        mode(mat) <- "integer"
        new_centers = matrix(c(0L, 0L), ncol = 2)
        avg_fun_name <- avg_fun; avg_fun_fun = function() ""
        dist_type <- dist_fun; dist_fun = function() ""
        centers <- TRUE
        verbose <- 0
        clean <- TRUE
        transform <-  NULL
        fn <- get("run_slic",
                  envir = asNamespace("supercells"),
                  inherits = FALSE
        )
        slic <- fn(mat = mat, vals = data, step = step, nc = compactness, con = clean,
                   centers = centers, type = dist_type, type_fun = dist_fun,
                   avg_fun_fun = avg_fun_fun, avg_fun_name = avg_fun_name,
                   iter = iter, lims = minarea, input_centers = new_centers,
                   verbose = verbose
        )
        ext_x <- terra::ext(r_obj)
        slic_sf <- terra::rast(slic[[1]])
        terra::NAflag(slic_sf) <- -1
        terra::crs(slic_sf) <- terra::crs(r_obj)
        terra::ext(slic_sf) <- ext_x
        slic_sf <- sf::st_as_sf(terra::as.polygons(slic_sf, dissolve = TRUE))
        if (nrow(slic_sf) > 0) {
            empty_centers = slic[[2]][,1] != 0 | slic[[2]][,2] != 0
            slic_sf = cbind(slic_sf, stats::na.omit(slic[[2]][empty_centers, ]))
            names(slic_sf) = c("supercells", "x", "y", "geometry")
            slic_sf[["supercells"]] = slic_sf[["supercells"]] + 1
            slic_sf[["x"]] = as.vector(ext_x)[[1]] + (slic_sf[["x"]] * terra::res(r_obj)[[1]]) + (terra::res(r_obj)[[1]]/2)
            slic_sf[["y"]] = as.vector(ext_x)[[4]] - (slic_sf[["y"]] * terra::res(r_obj)[[2]]) - (terra::res(r_obj)[[1]]/2)
            colnames(slic[[3]]) = names(r_obj)
            slic_sf = cbind(slic_sf, stats::na.omit(slic[[3]][empty_centers, , drop = FALSE]))
            slic_sf = suppressWarnings(sf::st_collection_extract(slic_sf, "POLYGON"))
            return(slic_sf)
        }
        return(slic_sf)
    }
}

.segments_derived_from_file <- function(file, band, base_tile, derived_class,
                                        update_bbox = FALSE) {
    v_obj <- .raster_open_rast(file)
    base_tile <- .tile(base_tile)
    bbox <- sf::st_bbox(v_obj)
    if (update_bbox) {
        # Update spatial bbox
        .xmin(base_tile) <- bbox[["xmin"]]
        .xmax(base_tile) <- bbox[["xmax"]]
        .ymin(base_tile) <- bbox[["ymin"]]
        .ymax(base_tile) <- bbox[["ymax"]]
        .crs(base_tile) <- sf::st_crs(v_obj)
    }
    # Update file_info
    .fi(base_tile) <- .fi_derived_from_file(
        file = file,
        band = band,
        start_date = .tile_start_date(base_tile),
        end_date = .tile_end_date(base_tile)
    )
    # Set tile class and return tile
    .cube_set_class(base_tile, .conf_derived_s3class(derived_class))
}

.fi_segment_from_file <- function(file, band, start_date, end_date) {
    file <- .file_normalize(file)
    r_obj <- sf::st_read(file)
    bbox <- sf::st_bbox(r_obj)
    .fi_derived(
        band = band,
        start_date = start_date,
        end_date = end_date,
        ncols = NA,
        nrows = NA,
        xres = NA,
        yres = NA,
        xmin = bbox[["xmin"]],
        xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]],
        ymax = bbox[["ymax"]],
        path = file
    )
}
