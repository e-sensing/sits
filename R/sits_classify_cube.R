#' @title Classify a chunk of raster data  using multicores
#' @name .sits_classify_tile
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data using multicores. It breaks
#' the data into horizontal blocks and divides them between the available cores.
#'
#' Reads data using terra, cleans the data for NAs and missing values.
#' The clean data is stored in a data table with the time instances
#' for all pixels of the block. The algorithm then classifies data on
#' an year by year basis. For each year, extracts the sub-blocks for each band.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  tile            Single tile of a data cube.
#' @param  ml_model        Model trained by \code{\link[sits]{sits_train}}.
#' @param  roi             Region of interest.
#' @param  filter_fn       Smoothing filter function to be applied to the data.
#' @param  impute_fn       Impute function to replace NA.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores.
#' @param  output_dir      Output directory.
#' @param  version         Version of result.
#' @param  verbose         print processing information?
#' @param  progress        Show progress bar?
#' @return List of the classified raster layers.
.sits_classify_tile  <- function(tile, ml_model, roi, filter_fn, impute_fn,
                                 memsize, multicores, output_dir, version,
                                 verbose, progress) {

    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = "probs", version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
        message("Recovery: tile '", tile[["tile"]], "' already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)")
        probs_tile <- .tile_probs_from_file(
            file = out_file, band = "probs", base_tile = tile,
            labels = .ml_labels(ml_model)
        )
        return(probs_tile)
    }
    # # Callback final tile classification
    # .callback(process = "tile_classification", event = "started",
    #           context = environment())
    # Show initial time for tile classification
    if (verbose) {
        tile_start_time <- Sys.time()
        message("Starting classification of tile '",
                tile[["tile"]], "' at ", tile_start_time)
    }
    # Create jobs
    # Get job size
    job_size <- .raster_file_blocksize(
        .raster_open_rast(.fi_path(.fi(tile)))
    )
    # Compute how many jobs to process
    jobs <- .jobs_create(
        job_size = job_size, block_overlap = 0,
        ncols = .tile_ncols(tile), nrows = .tile_nrows(tile),
        xmin = .xmin(tile), xmax = .xmax(tile),
        ymin = .ymin(tile), ymax = .ymax(tile),
        crs = .crs(tile), roi = roi
    )
    # Process jobs in parallel
    block_files <- .jobs_parallel_chr(jobs, function(job) {
        # Get job block
        block <- .block(job)
        # Output file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )

        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # for cubes that have a time limit to expire - mpc cubes only
        tile <- .cube_token_generator(tile)
        # Read and preprocess values
        values <- .sits_classify_data_read(
            tile = tile, block = block, ml_model = ml_model,
            impute_fn = impute_fn, filter_fn = filter_fn
        )
        # Used to check values (below)
        original_nrows <- nrow(values)

        #
        # Log here
        #
        .sits_debug_log(
            event = "start_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )

        # Apply the classification model to values
        values <- ml_model(values)

        # Are the results consistent with the data input?
        .check_that(
            x = nrow(values) == original_nrows,
            msg = paste(
                "number of rows of probability matrix is different",
                "from number of input pixels"
            )
        )

        #
        # Log here
        #
        .sits_debug_log(
            event = "end_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )

        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "probs_cube", band = "probs"
        )
        offset <- .band_offset(band_conf)
        if (!is.null(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .band_scale(band_conf)
        if (!is.null(scale) && scale != 1) {
            values <- values / scale
        }


        #
        # Log here
        #
        .sits_debug_log(
            event = "start_block_data_save",
            key = "file",
            value = block_file
        )


        # Prepare and save results as raster
        .raster_write_block(
            file = block_file, block = block, bbox = .bbox(job),
            values = values, data_type = .band_data_type(band_conf),
            missing_value = .band_miss_value(band_conf)
        )

        #
        # Log here
        #
        .sits_debug_log(
            event = "end_block_data_save",
            key = "file",
            value = block_file
        )

        # Returned block file
        block_file
    })
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_probs_merge_blocks(
        file = out_file, band = "probs", labels = .ml_labels(ml_model),
        base_tile = tile, block_files = block_files, multicores = multicores
    )
    # # Callback final tile classification
    # .callback(event = "tile_classification", status = "end",
    #           context = environment())
    # show final time for classification
    if (verbose) {
        tile_end_time <- Sys.time()
        message("Tile '", tile[["tile"]], "' finished at ", tile_end_time)
        message("Elapsed time of ",
                format(round(tile_end_time - tile_start_time, digits = 2)))
        message("")
    }
    # Return probs tile
    probs_tile
}
