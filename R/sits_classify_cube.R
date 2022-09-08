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
.sits_classify_job <- function(job, tile, ml_model, filter_fn, impute_fn,
                               file_pattern, output_dir) {
    # function deprecated
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
