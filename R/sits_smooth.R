#' @title Smooth probability cubes with spatial predictors
#'
#' @name  sits_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and applies a Bayesian smoothing function.
#'
#' @param  cube              Probability data cube.
#' @param  ...               Other parameters for specific functions.
#' @param  window_size       Size of the neighborhood
#'                           (integer, min = 3, max = 21)
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#'                           (numeric, min = 0.1, max = 1.0)
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter)
#'                           (integer vector or scalar, min = 1, max = 200).
#' @param  exclusion_mask    Areas to be excluded from the classification
#'                           process. It can be defined as a sf object or a
#'                           shapefile.
#' @param  memsize           Memory available for classification in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  output_dir        Valid directory for output file.
#'                           (character vector of length 1).
#' @param  version           Version of the output
#'                           (character vector of length 1).
#'
#' @return A data cube.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create am xgboost model
#'     xgb_model <- sits_train(samples_modis_ndvi, sits_xgboost())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = xgb_model, output_dir = tempdir()
#'     )
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         bayes_cube,
#'         output_dir = tempdir()
#'     )
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
sits_smooth <- function(cube, ...) {
    # set caller for error messages
    .check_set_caller("sits_smooth")
    UseMethod("sits_smooth", cube)
}
#' @rdname sits_smooth
#' @export
sits_smooth.probs_cube <- function(cube, ...,
                                   window_size = 9L,
                                   neigh_fraction = 0.5,
                                   smoothness = 20L,
                                   exclusion_mask = NULL,
                                   memsize = 4L,
                                   multicores = 2L,
                                   output_dir,
                                   version = "v1") {
    # Check if cube has probability data
    .check_raster_cube_files(cube)
    # check window size
    .check_int_parameter(window_size, min = 3, max = 33, is_odd = TRUE)
    # check neighborhood fraction
    .check_num_parameter(neigh_fraction, min = 0., max = 1.0)
    # Check memsize
    .check_int_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_int_parameter(multicores, min = 1, max = 2048)
    # Check output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check version
    version <- .check_version(version)
    # get nlabels
    nlabels <- length(.cube_labels(cube))
    # Check smoothness
    .check_smoothness(smoothness, nlabels)
    # Prepare smoothness parameter
    if (length(smoothness) == 1) {
        smoothness <- rep(smoothness, nlabels)
    }
    # version is case-insensitive in sits
    version <- tolower(version)
    # get nlabels
    nlabels <- length(.cube_labels(cube))
    # Prepare smoothness parameter
    if (length(smoothness) == 1) {
        smoothness <- rep(smoothness, nlabels)
    }

    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = overlap),
        npaths = length(.tile_labels(cube)) * 2,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Call the smoothing method
    .smooth(
        cube = cube,
        block = block,
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        smoothness = smoothness,
        exclusion_mask = exclusion_mask,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}
#' @rdname sits_smooth
#' @export
sits_smooth.probs_vector_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_probs_vector_cube"))
}
#' @rdname sits_smooth
#' @export
sits_smooth.raster_cube <- function(cube,...) {
    stop(.conf("messages", "sits_smooth_default"))
}
#' @rdname sits_smooth
#' @export
sits_smooth.derived_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_smooth_default"))
}
#' @rdname sits_smooth
#' @export
sits_smooth.default <- function(cube,...) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube)))
        cube <- .cube_find_class(cube)
    else
        stop(.conf("messages", "sits_smooth_default"))

    cube <- sits_smooth(cube,...)
    return(cube)
}
