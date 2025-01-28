#' @title Calculate the variance of a probability cube
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a probability cube and estimate the local variance
#'              of the logit of the probability,
#'              to support the choice of parameters for Bayesian smoothing.
#'
#' @param  cube              Probability data cube (class "probs_cube")
#' @param  window_size       Size of the neighborhood (odd integer)
#' @param  neigh_fraction    Fraction of neighbors with highest probability
#'                           for Bayesian inference (numeric from 0.0 to 1.0)
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing (integer, min = 1, max = 16384)
#' @param  multicores        Number of cores to run the smoothing function
#'                           (integer, min = 1, max = 2048)
#' @param  output_dir        Output directory for image files
#'                           (character vector of length 1)
#' @param  version           Version of resulting image
#'                           (character vector of length 1)
#'
#' @return A variance data cube.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
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
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     var_cube <- sits_variance(probs_cube, output_dir = tempdir())
#'     # plot the variance cube
#'     plot(var_cube)
#' }
#' @export
sits_variance <- function(
        cube,
        window_size = 9L,
        neigh_fraction = 0.5,
        memsize = 4L,
        multicores = 2L,
        output_dir,
        version = "v1") {
    # set caller for error messages
    .check_set_caller("sits_variance")
    # Check if cube has data and metadata
    .check_raster_cube_files(cube)
    # check window size
    .check_int_parameter(window_size, min = 3, max = 33, is_odd = TRUE)
    # check neighborhood fraction
    .check_num_parameter(neigh_fraction, min = 0., max = 1.0)
    # Check memsize
    .check_int_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_int_parameter(multicores, min = 1, max = 2048)
    # check output_dir
    .check_output_dir(output_dir)
    # check version
    version <- .check_version(version)
    # Dispatch
    UseMethod("sits_variance", cube)
}
#' @rdname sits_variance
#' @export
sits_variance.probs_cube <- function(
        cube,
        window_size = 9L,
        neigh_fraction = 0.5,
        memsize = 4L,
        multicores = 2L,
        output_dir,
        version = "v1") {

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
    # Call the variance method
    variance_cube <- .variance(
        cube = cube,
        block = block,
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
    return(variance_cube)
}
#' @rdname sits_variance
#' @export
sits_variance.raster_cube <- function(cube,
                                      window_size = 7L,
                                      neigh_fraction = 0.5,
                                      memsize = 4L,
                                      multicores = 2L,
                                      output_dir,
                                      version = "v1") {
    stop(.conf("messages", "sits_variance_raster_cube"))
}
#' @rdname sits_variance
#' @export
sits_variance.derived_cube <- function(cube,
                                       window_size = 7L,
                                       neigh_fraction = 0.5,
                                       memsize = 4L,
                                       multicores = 2L,
                                       output_dir,
                                       version = "v1") {
    stop(.conf("messages", "sits_variance_raster_cube"))
}
#' @rdname sits_variance
#' @export
sits_variance.default <- function(cube,
                                  window_size = 7L,
                                  neigh_fraction = 0.5,
                                  memsize = 4L,
                                  multicores = 2L,
                                  output_dir,
                                  version = "v1") {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube)))
        cube <- .cube_find_class(cube)
    else
        stop(.conf("messages", "sits_variance_raster_cube"))
    variance_cube <- sits_variance(cube, window_size, neigh_fraction,
        memsize, multicores, output_dir, version)
    return(variance_cube)
}
