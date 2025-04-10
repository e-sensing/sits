#' @title Estimate ensemble prediction based on list of probs cubes
#'
#' @name  sits_combine_predictions
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  cubes         List of probability data cubes (class "probs_cube")
#' @param  type          Method to measure uncertainty. One of "average" or
#'                       "uncertainty"
#' @param  ...           Parameters for specific functions.
#' @param  weights       Weights for averaging (numeric vector).
#' @param  uncert_cubes  Uncertainty cubes to be used as local weights when
#'                       type = "uncertainty" is selected
#'                       (list of tibbles with class "uncertainty_cube")
#' @param  memsize       Memory available for classification in GB
#'                       (integer, min = 1, max = 16384).
#' @param  multicores    Number of cores to be used for classification
#'                       (integer, min = 1, max = 2048).
#' @param  output_dir    Valid directory for output file.
#'                       (character vector of length 1).
#' @param  version       Version of the output
#'                      (character vector of length 1).
#' @param  progress      Set progress bar?
#' @return A combined probability cube (tibble of class "probs_cube").
#'
#' @description Calculate an ensemble predictor based a list of probability
#' cubes. The function combines the output of two or more models
#' to derive a weighted average.
#' The supported types of ensemble predictors are 'average' and
#' 'uncertainty'. In the latter case, the uncertainty cubes need to
#' be provided using param \code{uncert_cubes}.
#'
#' @note
#' The distribution of class probabilities produced by machine learning
#' models such as random forest
#' is quite different from that produced by deep learning models
#' such as temporal CNN. Combining the result of two different models
#' is recommended to remove possible bias induced by a single model.
#'
#' By default, the function takes the average of the class probabilities
#' of two or more model results. If desired, users can use the uncertainty
#' estimates for each results to compute the weights for each pixel.
#' In this case, the uncertainities produced by the models for each pixel
#' are used to compute the weights for producing the combined result.
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube using rfor model
#'     probs_rfor_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir(),
#'         version = "rfor"
#'     )
#'     # create an SVM model
#'     svm_model <- sits_train(samples_modis_ndvi, sits_svm())
#'     # classify a data cube using SVM model
#'     probs_svm_cube <- sits_classify(
#'         data = cube, ml_model = svm_model, output_dir = tempdir(),
#'         version = "svm"
#'     )
#'     # create a list of predictions to be combined
#'     pred_cubes <- list(probs_rfor_cube, probs_svm_cube)
#'     # combine predictions
#'     comb_probs_cube <- sits_combine_predictions(
#'         pred_cubes,
#'         output_dir = tempdir()
#'     )
#'     # plot the resulting combined prediction cube
#'     plot(comb_probs_cube)
#' }
#' @export
sits_combine_predictions <- function(cubes, type = "average", ...) {
    # set caller for error msg
    .check_set_caller("sits_combine_predictions")
    # check if list of probs cubes have the same organization
    .check_probs_cube_lst(cubes)
    class(type) <- type
    UseMethod("sits_combine_predictions", type)
}

#' @rdname sits_combine_predictions
#' @export
sits_combine_predictions.average <- function(cubes,
                                             type = "average", ...,
                                             weights = NULL,
                                             memsize = 8L,
                                             multicores = 2L,
                                             output_dir,
                                             version = "v1",
                                             progress = FALSE) {
    # Check memsize
    .check_num_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_num_parameter(multicores, min = 1, max = 2048)
    # Check output dir
    .check_output_dir(output_dir)
    # Check version and convert to lowercase
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)
    # Get weights
    n_inputs <- length(cubes)
    weights <- .default(weights, rep(1 / n_inputs, n_inputs))
    .check_that(
        length(weights) == n_inputs,
        msg = .conf("messages", "sits_combine_predictions_weights")
    )
    .check_that(
        sum(weights) == 1,
        msg = .conf("messages", "sits_combine_predictions_sum_weights")
    )
    # Get combine function
    comb_fn <- .comb_fn_average(cubes, weights = weights)
    # Call combine predictions
    probs_cube <- .comb(
        probs_cubes = cubes,
        uncert_cubes = NULL,
        comb_fn = comb_fn,
        band = "probs",
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = progress, ...
    )
    return(probs_cube)
}

#' @rdname sits_combine_predictions
#' @export
sits_combine_predictions.uncertainty <- function(cubes,
                                                 type = "uncertainty", ...,
                                                 uncert_cubes,
                                                 memsize = 8L,
                                                 multicores = 2L,
                                                 output_dir,
                                                 version = "v1",
                                                 progress = FALSE) {
    # Check memsize
    .check_num_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_num_parameter(multicores, min = 1, max = 2048)
    # Check output dir
    .check_output_dir(output_dir)
    # Check version and convert to lowercase
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)
    # Check if list of probs cubes and uncert_cubes have the same organization
    .check_that(
        length(cubes) == length(uncert_cubes),
        msg = .conf("messages", "sits_combine_predictions_uncert_cubes")
    )
    .check_uncert_cube_lst(uncert_cubes)
    .check_cubes_same_size(cubes[[1]], uncert_cubes[[1]])
    # Get combine function
    comb_fn <- .comb_fn_uncertainty(cubes)
    # Call combine predictions
    probs_cube <- .comb(
        probs_cubes = cubes,
        uncert_cubes = uncert_cubes,
        comb_fn = comb_fn,
        band = "probs",
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = progress, ...
    )
    return(probs_cube)
}
#' @rdname sits_combine_predictions
#' @export
sits_combine_predictions.default <- function(cubes, type, ...) {
    stop(.conf("messages", "sits_combine_predictions"))
}
