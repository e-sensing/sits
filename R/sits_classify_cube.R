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
    out_file <- .file_probs_name(
        tile = tile, version = version, output_dir = output_dir
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
            file = out_file,
            band = "probs",
            base_tile = tile,
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
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )

        # Resume processing in case of failure
        if (file.exists(block_file)) {

            #
            # log
            #
            .sits_debug_log(
                output_dir = output_dir,
                event = "start_classification_block_validation",
                key = "block_file",
                value = block_file
            )

            # Try to open the file
            r_obj <- .try(.raster_open_rast(block_file), default = NULL)

            # If file can be opened, check if the result is correct
            # this file will not be processed again
            if (!is.null(r_obj)) {

                # Verify if the raster is corrupted
                valid_block <- .try({
                    .raster_get_values(r_obj)

                    #
                    # log
                    #
                    .sits_debug_log(
                        output_dir = output_dir,
                        event = "end_classification_block_validation",
                        key = "is_valid",
                        value = TRUE
                    )

                    # Return value
                    TRUE
                },
                default = {
                    unlink(block_file)

                    #
                    # log
                    #
                    .sits_debug_log(
                        output_dir = output_dir,
                        event = "end_classification_block_validation",
                        key = "is_valid",
                        value = FALSE
                    )

                    # Return value
                    FALSE
                })

                if (valid_block) {
                    return(block_file)
                }
            }
        }

        # for cubes that have a time limit to expire - mpc cubes only
        tile <- .cube_token_generator(tile)


        # Read and preprocess values
        values <- .sits_classify_data_read(
            tile = tile, block = block, ml_model = ml_model,
            impute_fn = impute_fn, filter_fn = filter_fn,
            output_dir = output_dir
        )

        #
        # Log here
        #
        .sits_debug_log(
            output_dir = output_dir,
            event = "start_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )

        # Apply the classification model to values
        probs <- ml_model(values)

        # Are the results consistent with the data input?
        .check_that(
            x = nrow(probs) == nrow(values),
            msg = paste(
                "number of rows of probability matrix is different",
                "from number of input pixels"
            )
        )

        #
        # Log here
        #
        .sits_debug_log(
            output_dir = output_dir,
            event = "end_block_data_classification",
            key = "model",
            value = .ml_class(ml_model)
        )

        # Prepare probability to be saved
        conf_band <- .conf_derived_band(
            derived_class = "probs_cube", band = "probs"
        )
        offset <- .band_offset(conf_band)
        if (!is.null(offset) && offset != 0) {
            probs <- probs - offset
        }
        scale <- .band_scale(conf_band)
        if (!is.null(scale) && scale != 1) {
            probs <- probs / scale
        }


        #
        # Log here
        #
        .sits_debug_log(
            output_dir = output_dir,
            event = "start_block_data_save",
            key = "file",
            value = block_file
        )


        # Prepare and save results as raster
        .raster_write_block(
            file = block_file,
            block = block,
            bbox = .bbox(job),
            values = probs,
            data_type = .band_data_type(conf_band)
        )

        #
        # Log here
        #
        .sits_debug_log(
            output_dir = output_dir,
            event = "end_block_data_save",
            key = "file",
            value = block_file
        )

        # Returned value
        block_file
    })
    # Merge blocks into a new probs_cube tile
    probs_tile <- .tile_probs_merge_blocks(
        file = out_file, band = "probs",
        labels = .ml_labels(ml_model),
        base_tile = tile, block_files = block_files,
        multicores = multicores
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

    return(probs_tile)
}
