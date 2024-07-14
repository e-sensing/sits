#' @title Estimate classification uncertainty based on probs cube
#'
#' @name  sits_uncertainty
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @param  cube         Probability data cube.
#' @param  ...         Other parameters for specific functions.
#' @param  type         Method to measure uncertainty. See details.
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
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # calculate uncertainty
#'     uncert_cube <- sits_uncertainty(probs_cube, output_dir = tempdir())
#'     # plot the resulting uncertainty cube
#'     plot(uncert_cube)
#' }
#' @export
sits_uncertainty <-  function(cube, ...,
                              type = "entropy",
                              multicores = 2L,
                              memsize = 4L,
                              output_dir,
                              version = "v1") {
    # Dispatch
    UseMethod("sits_uncertainty", cube)
}
#' @rdname sits_uncertainty
#' @export
sits_uncertainty.probs_cube <- function(
        cube, ...,
        type = "entropy",
        multicores = 2,
        memsize = 4,
        output_dir,
        version = "v1") {
    # Check if cube has probability data
    .check_raster_cube_files(cube)
    # Check memsize
    .check_num_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_num_parameter(multicores, min = 1, max = 2048)
    # check output dir
    .check_output_dir(output_dir)
    # check version
    version <- .check_version(version)
    # version is case-insensitive in sits
    version <- tolower(version)
    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_labels(cube)) + 1,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Define the class of the smoothing
    uncert_fn <- switch(
        type,
        least   = .uncertainty_fn_least(),
        margin  = .uncertainty_fn_margin(),
        entropy = .uncertainty_fn_entropy()
    )
    # Compute uncertainty
    uncert_cube <- .uncertainty_raster_cube(
        cube = cube,
        band = type,
        uncert_fn = uncert_fn,
        output_dir = output_dir,
        version = version
    )
    return(uncert_cube)
}
#' @rdname sits_uncertainty
#' @export
sits_uncertainty.probs_vector_cube <- function(
        cube, ...,
        type = "entropy",
        multicores = 2,
        memsize = 4,
        output_dir,
        version = "v1") {
    # Check if cube has probability data
    .check_raster_cube_files(cube)
    # Check memsize
    .check_int_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_int_parameter(multicores, min = 1, max = 2048)
    # check output dir
    .check_output_dir(output_dir)
    # check version
    version <- .check_version(version)
    # Compute uncertainty
    uncert_cube <- .uncertainty_vector_cube(
        cube = cube,
        band = type,
        output_dir = output_dir,
        version = version
    )
    return(uncert_cube)
}
#' @rdname sits_uncertainty
#' @export
sits_uncertainty.default <- function(
        cube, ...,
        type,
        multicores,
        memsize,
        output_dir,
        version) {
    stop(.conf("messages", "sits_uncertainty_default"))
}
