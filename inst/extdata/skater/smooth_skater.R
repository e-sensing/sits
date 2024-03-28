#' @title Smooth probability cubes with spatial predictors
#'
#' @name  sits_smooth_skater
#'
#' @author Gabriel Bordoni, \email{gilberto.camara@@inpe.br}
#' @author Renato Assuncao, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and applies a SKATER-based smoothing function.
#'
#' @param  cube              Probability data cube.
#' @param  ncuts             Number of cuts to apply to minimum spanning tree
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
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = xgb_model, output_dir = tempdir()
#'     )
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using SKATER
#'     bayes_cube <- sits_smooth_skater(probs_cube, output_dir = tempdir())
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
sits_smooth_skater <- function(cube,
                        ncuts = 10000,
                        memsize = 4L,
                        multicores = 2L,
                        output_dir,
                        version = "v1") {
    # Check if cube has probability data
    .check_raster_cube_files(cube)
    # Check memsize
    .check_memsize(memsize, min = 1, max = 16384)
    # Check multicores
    .check_multicores(multicores, min = 1, max = 2048)
    # Check output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check version
    version <- .check_version(version)
    # get nlabels
    nlabels <- length(sits_labels(cube))
    UseMethod("sits_smooth_skater", cube)
}
#' @rdname sits_smooth
#' @export
sits_smooth_skater.probs_cube <- function(cube,
                                   ncuts = 10000,
                                   memsize = 4L,
                                   multicores = 2L,
                                   output_dir,
                                   version = "v1") {
    # version is case-insensitive in sits
    version <- tolower(version)
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = length(.tile_labels(cube)) * 2,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Call the smoothing method
    .smooth_skater(
        cube = cube,
        ncuts = ncuts,
        block = block,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}


#---- SAKTER smoothing ----
#' @title Smooth probability cubes with spatial predictors
#' @noRd
#' @param  cube              Probability data cube.
#' @param  block             Individual block that will be processed
#' @param  ncuts             Number of cuts to apply to minimum spanning tree
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#'
.smooth_skater <- function(cube,
                    ncuts,
                    block,
                    multicores,
                    memsize,
                    output_dir,
                    version) {
    # Smooth parameters checked in smooth function creation
    # Create smooth function
    smooth_fn <- .smooth_fn_skater(
        ncuts = ncuts
    )
    # Smoothing
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # Smooth the data
        .smooth_tile(
            tile = tile,
            band = "bayes",
            block = block,
            overlap = 0,
            smooth_fn = smooth_fn,
            output_dir = output_dir,
            version = version
        )
    })
}
#' @title Define smoothing function
#' @noRd
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a vector or a scalar.
#' @return Function to be applied to smoothen data
.smooth_fn_skater <- function(ncuts) {

    # Define smooth function
    smooth_fn <- function(values, block) {
        # Check values length
        input_pixels <- nrow(values)
        # Process Bayesian
        values <- skater_smoother_fraction(
            logits = values,
            nrows = .nrows(block),
            ncols = .ncols(block),
            ncuts = ncuts
        )
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return a closure
    smooth_fn
}
