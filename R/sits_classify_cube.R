#' @title Classify a chunk of raster data  using multicores
#' @name .sits_classify_multicores
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
.sits_classify_multicores <- function(tile,
                                      ml_model,
                                      roi,
                                      filter_fn,
                                      impute_fn,
                                      memsize,
                                      multicores,
                                      output_dir,
                                      version,
                                      verbose,
                                      progress) {

    # set caller to show in errors
    .check_set_caller(".sits_classify_multicores")

    # check documentation mode
    progress <- .check_documentation(progress)

    # some models have parallel processing built in
    if ("xgb_model" %in% class(ml_model)) {
        multicores <- 1
    }

    # retrieve the samples from the model
    samples <- .sits_ml_model_samples(ml_model)

    # get samples labels
    labels <- sits_labels(samples)

    # precondition - are the samples empty?
    .check_that(
        x = nrow(samples) > 0,
        msg = "original samples not saved"
    )

    # precondition - are the sample bands contained in the cube bands?
    tile_bands <- sits_bands(tile)
    bands <- sits_bands(samples)
    .check_chr_within(
        x = bands,
        within = tile_bands,
        msg = "some bands in samples are not in cube"
    )

    # retrieve the normalization stats from the model
    stats <- environment(ml_model)$stats

    # is there a region of interest?
    if (purrr::is_null(roi)) {
        sub_image <- .sits_raster_sub_image_default(tile)
    } else {
        sub_image <- .sits_raster_sub_image(tile = tile, roi = roi)
    }

    # divide the input data in blocks
    blocks <- .sits_raster_blocks(
        tile = tile,
        ml_model = ml_model,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )

    # get timeline
    timeline <- sits_timeline(tile)

    # create the metadata for the probability cube
    probs_cube <- .cube_derived_create(
        cube       = tile,
        cube_class = "probs_cube",
        band_name  = "probs",
        labels     = labels,
        start_date = timeline[[1]],
        end_date   = timeline[[length(timeline)]],
        bbox       = sub_image,
        output_dir = output_dir,
        version    = version
    )

    # resume feature
    # if tile already exists, return probs_cube
    if (file.exists(.file_info_path(probs_cube))) {

        message(
            paste("Recovery mode. Classified probability image detected in",
                  "the provided directory.")
        )

        return(probs_cube)
    }

    # show initial time for classification
    if (verbose) {
        message(paste0(
            "Using ", length(blocks),
            " blocks of size (", blocks[[1]][["nrows"]],
            " x ", blocks[[1]][["ncols"]], ")"
        ))

        start_time <- Sys.time()
        message(paste0(
            "Starting classification of '", tile$tile,
            "' at ", start_time
        ))
    }

    # prepare parallelization
    .sits_parallel_start(workers = multicores, log = verbose)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # log
    .sits_debug_log(
        output_dir = output_dir,
        event = "start classification",
        key = "blocks",
        value = length(blocks)
    )

    # read the blocks and compute the probabilities
    filenames <- .sits_parallel_map(blocks, function(b) {

        probs_cube_filename <- tools::file_path_sans_ext(
            basename(.file_info_path(probs_cube))
        )
        # directory where probs blocks will be save
        probs_cube_dir <- dirname(.file_info_path(probs_cube))

        # define the file name of the raster file to be written
        filename_block <- .create_filename(
            filenames = c(probs_cube_filename, "block",
                          b[["first_row"]], b[["nrows"]]),
            ext = ".tif",
            output_dir = probs_cube_dir
        )

        # resume processing in case of failure
        if (file.exists(filename_block)) {
            # try to open the file
            r_obj <-
                tryCatch({
                    .raster_open_rast(filename_block)
                },
                error = function(e) {
                    return(NULL)
                }
                )
            # if file can be opened, check if the result is correct
            # this file will not be processed again
            if (!purrr::is_null(r_obj)) {
                if (.raster_nrows(r_obj) == b[["nrows"]]) {
                    # log
                    .sits_debug_log(
                        output_dir = output_dir,
                        event = "skipping block",
                        key = "block file",
                        value = filename_block
                    )
                    return(filename_block)
                }
            }
        }
        # log
        .sits_debug_log(
            output_dir = output_dir,
            event = "before preprocess block",
            key = "block",
            value = b
        )

        # read the data
        distances <- .sits_raster_data_read(
            cube       = tile,
            samples    = samples,
            extent     = b,
            stats      = stats,
            filter_fn  = filter_fn,
            impute_fn  = impute_fn
        )
        # log
        .sits_debug_log(
            output_dir = output_dir,
            event = "before classification block"
        )

        # predict the classification values
        pred_block <- ml_model(distances)
        # log
        .sits_debug_log(
            output_dir = output_dir,
            event = "classification block",
            key = "ml_model",
            value = class(ml_model)[[1]]
        )

        # are the results consistent with the data input?
        .check_that(
            x = nrow(pred_block) == nrow(distances),
            msg = paste(
                "number of rows of probability matrix is different",
                "from number of input pixels"
            )
        )
        # log
        .sits_debug_log(
            output_dir = output_dir,
            event = "before save classified block"
        )

        # convert probabilities matrix to INT2U
        scale_factor_save <- round(1 / .config_get("probs_cube_scale_factor"))
        pred_block <- round(scale_factor_save * pred_block, digits = 0)

        # compute block spatial parameters
        params <- .cube_params_block(tile, b)

        # create a new raster
        r_obj <- .raster_new_rast(
            nrows   = params[["nrows"]],
            ncols   = params[["ncols"]],
            xmin    = params[["xmin"]],
            xmax    = params[["xmax"]],
            ymin    = params[["ymin"]],
            ymax    = params[["ymax"]],
            nlayers = length(labels),
            crs     = params[["crs"]]
        )

        # copy values
        r_obj <- .raster_set_values(
            r_obj = r_obj,
            values = pred_block
        )

        # write the probabilities to a raster file
        .raster_write_rast(
            r_obj        = r_obj,
            file         = filename_block,
            format       = "GTiff",
            data_type    = .config_get("probs_cube_data_type"),
            gdal_options = .config_gtiff_default_options(),
            overwrite    = TRUE
        )
        # log
        .sits_debug_log(
            output_dir = output_dir,
            event = "save classified block"
        )

        # call garbage collector
        gc()

        return(filename_block)
    }, progress = progress)

    # put the filenames in a vector
    filenames <- unlist(filenames)
    # log
    .sits_debug_log(
        output_dir = output_dir,
        event = "end classification"
    )

    # join predictions
    out_file <- .file_info_path(probs_cube)
    probs_cube_dt <- .config_get("probs_cube_data_type")
    .raster_merge(
        in_files = filenames,
        out_file = out_file,
        format = "GTiff",
        gdal_datatype = .raster_gdal_datatype(probs_cube_dt),
        gdal_options = .config_gtiff_default_options(),
        overwrite = TRUE
    )

    # adjust nrows and ncols
    r_obj <- .raster_open_rast(out_file)
    file_info <- .file_info(probs_cube)
    file_info$nrows <- nrow(r_obj)
    file_info$ncols <- ncol(r_obj)
    probs_cube$file_info[[1]] <- file_info

    # log
    .sits_debug_log(
        output_dir = output_dir,
        event = "merge"
    )

    # show final time for classification
    if (verbose) {
        end_time <- Sys.time()
        message(paste("Classification finished at", end_time))
        message(paste("Elapsed time of", end_time - start_time))
    }
    return(probs_cube)
}
#' @title Check classification parameters
#' @name .sits_classify_check_params
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Verify that required parameters are correct.
#'
#' @param  cube            Tibble with information about a data cube.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @return Tests succeeded?
.sits_classify_check_params <- function(cube, ml_model) {

    # set caller to show in errors
    .check_set_caller(".sits_classify_check_params")

    # ensure metadata tibble exists
    .check_that(
        x = nrow(cube) > 0,
        msg = "invalid metadata for the cube"
    )

    # ensure the machine learning model has been built
    .check_null(
        x = ml_model,
        msg = "trained ML model not available"
    )

    return(invisible(TRUE))
}
