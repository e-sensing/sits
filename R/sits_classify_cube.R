#' @title Classify a chunk of raster data  using multicores
#' @name .sits_classify_blocks_multicores
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
#' @param job           A job containing information about the block to be
#'                      processed.
#' @param tile          Single tile of a data cube.
#' @param ml_model      Model trained by \code{\link[sits]{sits_train}}.
#' @param filter_fn     Smoothing filter function to be applied to the data.
#' @param impute_fn     Impute function to replace NA.
#' @param file_pattern  Output block file name pattern.
#' @param output_dir    Output directory.
#' @return List of the classified raster layers.
.sits_classify_job <- function(job,
                               tile,
                               ml_model,
                               filter_fn,
                               impute_fn,
                               file_pattern,
                               output_dir) {
    # Select i-th block
    block <- .jobs_as_block(job)
    # Define the file name of the raster file to be written
    block_file <- .file_block_name(
        pattern = file_pattern,
        block = block,
        output_dir = output_dir
    )
    # resume processing in case of failure
    # If block already exists and is valid, return it
    if (.raster_is_valid(block_file)) {
        return(block_file)
    }
    #
    # Log here
    #
    .sits_debug_log(
        output_dir = output_dir,
        event = "start_block_data_read_preprocess",
        key = "block",
        value = block
    )
    # Read data
    distances <- .tile_read_block(
        tile = tile,
        bands = .ml_model_bands(ml_model),
        block = block,
        impute_fn = impute_fn,
        filter_fn = filter_fn,
        ml_model = ml_model
    )
    #
    # Log here
    #
    .sits_debug_log(
        output_dir = output_dir,
        event = "end_block_data_read_preprocess"
    )
    #
    # Log here
    #
    .sits_debug_log(
        output_dir = output_dir,
        event = "start_prediction",
        key = "ml_model",
        value = class(ml_model)[[1]]
    )
    # Predict values
    pred_block <- ml_model(distances)
    # Are the results consistent with the data input?
    .check_that(
        x = nrow(pred_block) == nrow(distances),
        msg = paste(
            "number of rows of probability matrix is different",
            "from number of input pixels"
        )
    )
    # Preprocess prediction
    pred_block <- .values_preprocess_probs(pred_block)
    #
    # Log here
    #
    .sits_debug_log(
        output_dir = output_dir,
        event = "end_prediction"
    )
    #
    # Log here
    #
    .sits_debug_log(
        output_dir = output_dir,
        event = "start_write_block",
        key = "block_file",
        value = block_file
    )
    # Save probs block
    .job_save_values(
        job = job,
        values = pred_block,
        data_type = .config_get("probs_cube_data_type"),
        out_file = block_file
    )
    #
    # Log here
    #
    .sits_debug_log(
        output_dir = output_dir,
        event = "end_write_block"
    )
    return(block_file)
}
#' @title Check classification parameters
#' @name .sits_classify_check_params
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Verify that required parameters are correct.
#'
#' @param  tile      Tibble with information about a data cube.
#' @param  ml_model  An R model trained by \code{\link[sits]{sits_train}}.
#' @return Tests succeeded?
.sits_classify_check_params <- function(tile, ml_model) {

    # set caller to show in errors
    .check_set_caller(".sits_classify_check_params")

    # check if tile is a cube
    .cube_check(tile)

    # ensure tile number of rows
    .check_num(
        x = nrow(tile),
        min = 1, max = 1,
        msg = "invalid number of rows in tile parameter"
    )

    # retrieve the samples from the model
    samples <- .sits_ml_model_samples(ml_model)

    # precondition - are the samples empty?
    .check_num(
        x = nrow(samples),
        min = 1,
        msg = "invalid number of samples provided by ml_model"
    )

    # precondition - are the sample bands contained in the cube bands?
    .check_chr_within(
        x = sits_bands(samples),
        within = sits_bands(tile),
        msg = "some bands in samples are not in cube"
    )

    return(invisible(TRUE))
}
