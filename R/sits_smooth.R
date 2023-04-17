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
#'
#' @param  cube              Probability data cube.
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a vector or a scalar.
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#'
#' @return A data cube.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # create a ResNet model
#'     torch_model <- sits_train(samples_modis_ndvi, sits_resnet(epochs = 20))
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
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = torch_model, output_dir = tempdir()
#'     )
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         bayes_cube, output_dir = tempdir()
#'     )
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
sits_smooth <- function(cube,
                        window_size = 7,
                        neigh_fraction = 0.5,
                        smoothness = 10,
                        memsize = 4,
                        multicores = 2,
                        output_dir,
                        version = "v1") {

    # Check if cube has probability data
    .check_is_probs_cube(cube)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)
    # get nlabels
    nlabels <- length(sits_labels(cube))
    # Check smoothness
    .check_smoothness(smoothness, nlabels)
    # Prepare smoothness parameter
    if (length(smoothness == 1)) {
        smoothness <- rep(smoothness, nlabels)
    }
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = length(.tile_labels(cube)) * 2,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
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
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Call the smoothing method
    .smooth(
        cube = cube,
        block = block,
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        smoothness = smoothness,
        multicores = multicores,
        memsize = memsize,
        output_dir = output_dir,
        version = version
    )
}
