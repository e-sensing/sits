#' @title Build a labelled image from a probability cube
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              and label them based on the maximum probability for each pixel.
#'
#' @param  cube        Classified image data cube.
#' @param  multicores  Number of workers to label the classification in
#'                     parallel.
#' @param  memsize     maximum overall memory (in GB) to label the
#'                     classification.
#' @param  output_dir  Output directory for classified files.
#' @param  version     Version of resulting image
#'                     (in the case of multiple runs).
#' @return             A data cube with an image with the classified map.
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
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(bayes_cube)
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
sits_label_classification <- function(cube, memsize = 4, multicores = 2,
                                      output_dir = getwd(), version = "v1") {

    # Pre-conditions - Check parameters
    .check_is_probs_cube(cube)
    .check_memsize(memsize)
    .check_multicores(multicores)
    .check_output_dir(output_dir)
    .check_version(version)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        # npaths = input(nlayers) + output(1)
        npaths = length(.tile_labels(cube)) + 1,
        nbytes = 8, proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize, block = block,
        image_size = .tile_size(.tile(cube)), memsize = memsize,
        multicores = multicores
    )

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    UseMethod("sits_label_classification", cube)
}

#' @rdname sits_label_classification
#' @export
sits_label_classification.probs_cube <- function(cube, memsize = 4,
                                                 multicores = 2,
                                                 output_dir = getwd(),
                                                 version = "v1") {
    # Labeling parameters checked in label function
    # Create label classification function
    label_fn <- .label_fn_majority()
    # Process each tile sequentially
    class_cube <- .cube_foreach_tile(cube, function(tile) {
        # Label the data
        class_tile <- .label_tile(
            tile = tile, band = "class", label_fn = label_fn,
            output_dir = output_dir, version = version
        )
        return(class_tile)
    })
    return(class_cube)
}
