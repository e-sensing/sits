#' @title Estimate ensemble prediction based on list of probs cubes
#'
#' @name  sits_combine_predictions
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cubes         List of probability data cubes.
#' @param  type          Method to measure uncertainty. See details.
#' @param  ...           Parameters for specific functions.
#' @param  weights       Weights for averaging
#' @param  uncert_cubes  Uncertainty cubes to be used as local weights.
#' @param  multicores    Number of cores to run the function.
#' @param  memsize       Maximum overall memory (in GB) to run the
#'                       function.
#' @param  output_dir    Output directory for image files.
#' @param  version       Version of resulting image.
#'                       (in the case of multiple tests)
#' @return A combined probability cube
#'
#' @description Calculate an ensemble predictor based a list of probability
#' cubes. The function combines the output of two or more classifier
#' to derive a value which is based on weights assigned to each model.
#' The supported types of ensemble predictors are 'average' and
#' 'uncertainty'.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube using rfor model
#'     probs_rfor_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, version = "rfor"
#'     )
#'     # create an XGBoost model
#'     tcnn_model <- sits_train(samples_modis_ndvi, sits_tempcnn())
#'     # classify a data cube using tempcnn model
#'     probs_tcnn_cube <- sits_classify(
#'         data = cube, ml_model = tcnn_model, version = "tcnn"
#'     )
#'     # create a list of predictions to be combined
#'     pred_cubes <- list(probs_rfor_cube, probs_tcnn_cube)
#'     # combine predictions
#'     comb_probs_cube <- sits_combine_predictions(cubes = pred_cubes)
#'     # plot the resulting combined prediction cube
#'     plot(comb_probs_cube)
#' }
#' @export
sits_combine_predictions <- function(cubes,
                                     type = "average", ...,
                                     memsize = 8,
                                     multicores = 2,
                                     output_dir = getwd(),
                                     version = "v1") {
    # check if list of probs cubes have the same organization
    .check_probs_cube_lst(cubes)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output dir
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)

    class(type) <- type
    UseMethod("sits_combine_predictions", type)
}

#' @rdname sits_combine_predictions
#' @export
sits_combine_predictions.average <- function(cubes,
                                             type = "average", ...,
                                             weights = NULL,
                                             memsize = 8,
                                             multicores = 2,
                                             output_dir = getwd(),
                                             version = "v1") {
    # Get weights
    n_inputs <- length(cubes)
    weights <- .default(weights, rep(1 / n_inputs, n_inputs))
    .check_that(
        length(weights) == n_inputs,
        msg = "number of weights does not match number of inputs",
    )
    .check_that(
        sum(weights) == 1, msg = "weigths should add up to 1.0"
    )
    # Get combine function
    comb_fn <- .comb_fn_average(cubes, weights = weights)
    # Call combine predictions
    probs_cube <- .comb(
        probs_cubes = cubes,
        uncert_cubes = NULL,
        comb_fn = comb_fn,
        band = "comb",
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = FALSE, ...
    )
    return(probs_cube)
}

#' @rdname sits_combine_predictions
#' @export
sits_combine_predictions.uncertainty <- function(cubes,
                                                 type = "uncertainty", ...,
                                                 uncert_cubes,
                                                 memsize = 8,
                                                 multicores = 2,
                                                 output_dir = getwd(),
                                                 version = "v1") {
    # Check if list of probs cubes and uncert_cubes have the same organization
    .check_that(
        length(cubes) == length(uncert_cubes),
        local_msg = "uncert_cubes must have same length of cubes",
        msg = "invalid uncert_cubes parameter"
    )
    .check_uncert_cube_lst(uncert_cubes)
    .check_cubes_match(cubes[[1]], uncert_cubes[[1]])
    # Get combine function
    comb_fn <- .comb_fn_uncertainty(cubes)
    # Call combine predictions
    probs_cube <- .comb(
        probs_cubes = cubes,
        uncert_cubes = uncert_cubes,
        comb_fn = comb_fn,
        band = "comb",
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = FALSE, ...
    )
    return(probs_cube)
}

sits_combine_predictions.default <- function(cubes,
                                             type = "default", ...,
                                             memsize = 8,
                                             multicores = 2,
                                             output_dir = getwd(),
                                             version = "v1") {
    stop("Invalid `type` parameter ",
         "(value must be one of 'average', 'uncertainty').")
}
