#' @title Estimate classification uncertainty based on probs cube
#'
#' @name  sits_uncertainty
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @param  cube         Probability data cube.
#' @param  type         Method to measure uncertainty. See details.
#' @param  window_size  Size of neighborhood to calculate uncertainty.
#' @param  multicores   Number of cores to run the function.
#' @param  memsize      Maximum overall memory (in GB) to run the function.
#' @param  output_dir   Output directory for image files.
#' @param  version      Version of resulting image (in the case of
#'                      multiple tests).
#' @return An uncertainty data cube
#'
#' @description Calculate the uncertainty cube based on the probabilities
#' produced by the classifier. Takes a probability cube as input.
#' The uncertainty measure is relevant in the context of active leaning,
#' and helps to increase the quantity and quality of training samples by
#' providing information about the confidence of the model.
#' The supported types of uncertainty are 'entropy', 'least', and 'margin'.
#' 'entropy' is the difference between all predictions expressed as
#' entropy, 'least' is the difference between 100% and most confident
#' prediction, and 'margin' is the difference between the two most confident
#' predictions.
#'
#' @references Monarch, Robert Munro. Human-in-the-Loop Machine Learning:
#' Active learning and annotation for human-centered AI. Simon and Schuster,
#' 2021.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # calculate uncertainty
#'     uncert_cube <- sits_uncertainty(probs_cube)
#'     # plot the resulting uncertainty cube
#'     plot(uncert_cube)
#' }
#' @export
sits_uncertainty <- function(cube,
                             type = "least",
                             window_size = 5,
                             memsize = 4,
                             multicores = 2,
                             output_dir = getwd(),
                             version = "v1",
                             progress = TRUE) {

    # Check if cube has probability data
    .check_is_probs_cube(cube)
    # Check window size
    .check_window_size(window_size)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # check output dir
    .check_output_dir(output_dir)
    # check version
    .check_version(version)

    # Define the class of the smoothing
    class(type) <- c(type, class(type))
    UseMethod("sits_uncertainty", type)
}

#' @rdname sits_uncertainty
#' @export
sits_uncertainty.least <- function(cube,
                                   type = "least",
                                   window_size = 5,
                                   memsize = 4,
                                   multicores = 2,
                                   output_dir = getwd(),
                                   version = "v1",
                                   progress = TRUE) {
    # Uncertainty parameters checked in smooth function creation
    # Create uncertainty function
    uncert_fn <- .uncert_fn_least(window_size = window_size)
    uncert_cube <- .uncert(
        cube = cube,
        uncert_fn = uncert_fn,
        band = "least",
        window_size = window_size,
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = progress
    )
    return(uncert_cube)
}

#' @rdname sits_uncertainty
#' @export
sits_uncertainty.entropy <- function(cube,
                                     type = "entropy",
                                     window_size = 5,
                                     memsize = 4,
                                     multicores = 2,
                                     output_dir = getwd(),
                                     version = "v1",
                                     progress = TRUE) {
    # Uncertainty parameters checked in smooth function creation
    # Create uncertainty function
    uncert_fn <- .uncert_fn_least(window_size = window_size)
    uncert_cube <- .uncert(
        cube = cube,
        uncert_fn = uncert_fn,
        band = "entropy",
        window_size = window_size,
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = progress
    )
    return(uncert_cube)
}

#' @rdname sits_uncertainty
#' @export
sits_uncertainty.margin <- function(cube,
                                    type = "margin",
                                    window_size = 5,
                                    memsize = 4,
                                    multicores = 2,
                                    output_dir = getwd(),
                                    version = "v1",
                                    progress = TRUE) {
    # Uncertainty parameters checked in smooth function creation
    # Create uncertainty function
    uncert_fn <- .uncert_fn_least(window_size = window_size)
    uncert_cube <- .uncert(
        cube = cube,
        uncert_fn = uncert_fn,
        band = "margin",
        window_size = window_size,
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = progress
    )
    return(uncert_cube)
}
