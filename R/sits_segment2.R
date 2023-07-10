sits_classify.raster_cube <- function(cube,
                                      roi = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      memsize = 8,
                                      multicores = 2,
                                      output_dir,
                                      version = "v1",
                                      verbose = FALSE,
                                      progress = TRUE) {
    # preconditions
    .check_is_raster_cube(cube)
    .check_is_regular(cube)
    .check_memsize(memsize)
    .check_output_dir(output_dir)
    .check_version(version)

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
    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(cube)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
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
    .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    on.exit(.parallel_stop(), add = TRUE)
    # Show block information
    if (verbose) {
        start_time <- Sys.time()
        message(
            "Using blocks of size (", .nrows(block),
            " x ", .ncols(block), ")"
        )
    }
    # Classification
    # Process each tile sequentially
    segs_cube <- .cube_foreach_tile(cube, function(tile) {
        # Segment the data
        segs_tile <- .segment_tile(
            tile = tile,
            band = "segment",
            seg_fn = seg_fn,
            block = block,
            roi = roi,
            output_dir = output_dir,
            version = version,
            verbose = verbose,
            progress = progress
        )
        return(segs_tile)
    })
    return(segs_cube)
}

.segment_tile <- function(tile,
                          band,
                          seg_fn,
                          block,
                          roi,
                          output_dir,
                          version,
                          verbose,
                          progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        if (.check_messages()) {
            message("Recovery: tile '", tile[["tile"]], "' already exists.")
            message(
                "(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)"
            )
        }
        # TODO: adicionar recovery a partir de um shapefile
        seg_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = .ml_labels_code(ml_model),
            derived_class = "probs_cube",
            update_bbox = TRUE
        )
        return(seg_tile)
    }
    # Show initial time for tile classification
    tile_start_time <- .tile_classif_start(tile, verbose)
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
    # Compute fractions probability
    probs_fractions <- 1/length(.ml_labels(ml_model))
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
        pattern_name <- paste0(.file_pattern(out_file), "_segments")
        block_file <- .file_block_name(
            pattern = pattern_name,
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        # TODO: criar uma função com o nome .vector_is_valid
        if (.raster_is_valid(block_file)) {
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
        # Log here
        .debug_log(
            event = "start_block_data_segmentation",
            key = "segment"
        )
        # Apply the classification model to values
        values <- seg_fn(values)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Log
        .debug_log(
            event = "end_block_data_segmentation",
            key = "segment"
        )
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "segs_cube", band = band
        )

        #
        # Log here
        #
        .debug_log(
            event = "start_block_data_save",
            key = "file",
            value = block_file
        )
        # Prepare and save results as raster
        .segment_write_block(
            files = block_file,
            block = block,
            bbox = .bbox(chunk),
            values = values,
            data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Log
        .debug_log(
            event = "end_block_data_save",
            key = "file",
            value = block_file
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = progress)
    # Merge blocks into a new probs_cube tile
    seg_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = .ml_labels_code(ml_model),
        base_tile = tile,
        block_files = block_files,
        derived_class = "probs_cube",
        multicores = .jobs_multicores(),
        update_bbox = update_bbox
    )
    # show final time for classification
    .tile_classif_end(tile, tile_start_time, verbose)
    # Return probs tile
    seg_tile
}

#' @title Read a block of values retrieved from a set of raster images
#' @name  .classify_data_read
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param  tile            Input tile to read data.
#' @param  block           Bounding box in (col, row, ncols, nrows).
#' @return A matrix with values for classification.
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
        # Log
        .debug_log(
            event = "start_block_data_process",
            key = "process",
            value = "cloud-impute-filter"
        )
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
        # Log
        .debug_log(
            event = "end_block_data_process",
            key = "band",
            value = band
        )
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

.segment_write_block <- function(files, block, bbox, values, data_type,
                                 missing_value) {
    # to support old models convert values to matrix
    values <- as.matrix(values)
    nlayers <- ncol(values)
    if (length(files) > 1) {
        if (length(files) != ncol(values)) {
            stop("number of output files does not match number of layers")
        }
        # Write each layer in a separate file
        nlayers <- 1
    }
    for (i in seq_along(files)) {
        # Get i-th file
        file <- files[[i]]
        # Get layers to be saved
        cols <- if (length(files) > 1) i else seq_len(nlayers)
        # Create a new raster
        r_obj <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = nlayers, crs = bbox[["crs"]]
        )
        # Copy values
        r_obj <- .raster_set_values(
            r_obj = r_obj,
            values = values[, cols]
        )

        .raster_write_segments(
            r_obj = r_obj, file = file
        )

    }
    # Return file path
    files
}

.raster_write_segments <- function(r_obj, file) {

    slic_sf <- sf::st_as_sf(terra::as.polygons(r_obj, dissolve = TRUE))
    # TODO: entender essas linhas
    # if (nrow(slic_sf) > 0) {
    #     empty_centers = slic[[2]][,1] != 0 | slic[[2]][,2] != 0
    #     slic_sf = cbind(slic_sf, stats::na.omit(slic[[2]][empty_centers, ]))
    #     names(slic_sf) = c("supercells", "x", "y", "geometry")
    #     slic_sf[["supercells"]] = slic_sf[["supercells"]] + 1
    #     slic_sf[["x"]] = as.vector(ext_x)[[1]] + (slic_sf[["x"]] * terra::res(x)[[1]]) + (terra::res(x)[[1]]/2)
    #     slic_sf[["y"]] = as.vector(ext_x)[[4]] - (slic_sf[["y"]] * terra::res(x)[[2]]) - (terra::res(x)[[1]]/2)
    #     colnames(slic[[3]]) = names(x)
    #     slic_sf = cbind(slic_sf, stats::na.omit(slic[[3]][empty_centers, , drop = FALSE]))
    #     slic_sf = suppressWarnings(sf::st_collection_extract(slic_sf, "POLYGON"))
    # }
    suppressWarnings(
        sf::st_write(slic_sf, file)
    )

    # was the file written correctly?
    .check_file(
        x = file,
        msg = "unable to write raster object"
    )

    return(invisible(NULL))
}

sits_supercells_temp <- function(data = NULL,
                                 step = 50,
                                 compactness = 1,
                                 dist_fun = "euclidean",
                                 avg_fun = "mean",
                                 iter = 10,
                                 minarea = 30) {
    function(data) {
        mat <- dim(data)[1:2]
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
        fn(mat = mat, vals = data, step = step, nc = compactness, con = clean,
           centers = centers, type = dist_type, type_fun = dist_fun,
           avg_fun_fun = avg_fun_fun, avg_fun_name = avg_fun_name,
           iter = iter, lims = minarea, input_centers = new_centers,
           verbose = verbose)
    }
}
