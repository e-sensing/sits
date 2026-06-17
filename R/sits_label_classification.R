#' @title Build a labelled image from a probability cube
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Souza, \email{felipe.souza@@inpe.br}
#'
#' @description
#' Takes a set of classified raster layers with probabilities,
#' and labels them based on the maximum probability for each pixel.
#' This function is the final step of the main land classification workflow.
#'
#' When the input is a \code{probs_vector_cube} (produced by
#' \code{\link[sits]{sits_classify}} from a segmented cube), this function
#' performs segment-based labeling: pixel-level probabilities are aggregated
#' inside each segment using the method specified by \code{label_method}.
#' All pixels within a segment receive the same class label in the output
#' raster. A GPKG file with segment summaries (including a \code{class}
#' column) is written automatically.
#'
#' @param  cube        Classified image data cube (\code{probs_cube} or
#'                     \code{probs_vector_cube}).
#' @param  ...         Other parameters for specific functions.
#' @param  label_method Decision method for segment-based labeling.
#'                     One of "mean" (default), "median", or "majority".
#'                     Only used when input is a \code{probs_vector_cube}.
#' @param  multicores  Number of workers to label the classification in
#'                     parallel.
#' @param  memsize     maximum overall memory (in GB) to label the
#'                     classification.
#' @param  output_dir  Output directory for classified files.
#' @param  version     Version of resulting image
#'                     (in the case of multiple runs).
#' @param  progress    Show progress bar?
#' @return             A data cube with an image with the classified map.
#'                     When input is a \code{probs_vector_cube}, the output
#'                     is a \code{class_vector_cube} with \code{vector_info}
#'                     preserved.
#'
#' @note
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies an ARD image collection
#'          from a cloud provider to a local directory for faster processing.}
#'      \item{\code{\link[sits]{sits_regularize}}: create a regular data cube
#'          from an ARD image collection.}
#'      \item{\code{\link[sits]{sits_apply}}: create new indices by combining
#'          bands of a  regular data cube (optional).}
#'      \item{\code{\link[sits]{sits_get_data}}: extract time series
#'          from a regular data cube based on user-provided labelled samples.}
#'      \item{\code{\link[sits]{sits_train}}: train a machine learning
#'          model based on image time series.}
#'      \item{\code{\link[sits]{sits_classify}}: classify a data cube
#'          using a machine learning model and obtain a probability cube.}
#'      \item{\code{\link[sits]{sits_smooth}}: post-process a probability cube
#'          using a spatial smoother to remove outliers and
#'          increase spatial consistency.}
#'      \item{\code{\link[sits]{sits_label_classification}}: produce a
#'          classified map by selecting the label with the highest probability
#'          from a smoothed cube.}
#' }
#'
#' The OBIA workflow adds segmentation before classification:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_segment}}: segment the raster cube to
#'          produce a vector_cube.}
#'      \item{\code{\link[sits]{sits_classify}}: classify pixel-level
#'          probabilities, preserving vector support.}
#'      \item{\code{\link[sits]{sits_label_classification}}: aggregate
#'          probabilities per segment and assign class labels.}
#' }
#'
#' Please refer to the sits documentation available in
#' \url{https://e-sensing.github.io/sitsbook/} for detailed examples.
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
sits_label_classification <- function(cube, ...) {
    .check_set_caller("sits_label_classification")
    # Dispatch
    UseMethod("sits_label_classification", cube)
}

#' @rdname sits_label_classification
#' @export
sits_label_classification.probs_cube <- function(cube, ...,
                                                 memsize = 4L,
                                                 multicores = 2L,
                                                 output_dir,
                                                 version = "v1",
                                                 progress = TRUE) {
    # Pre-conditions - Check parameters
    .check_raster_cube_files(cube)
    .check_num_parameter(memsize, min = 1L, max = 16384L)
    .check_num_parameter(multicores, min = 1L, max = 2048L)
    .check_output_dir(output_dir)
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)

    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0L),
        npaths = length(.cube_labels(cube)) + 1L,
        nbytes = 8L,
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
    if (.parallel_start(workers = multicores)) {
        on.exit(.parallel_stop(), add = TRUE)
    }
    # Create label classification function
    label_fn <- .label_fn_majority()
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        # Label the data
        .label_tile(
            tile = tile,
            band = "class",
            label_fn = label_fn,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    })
}

#' @rdname sits_label_classification
#' @param  label_method  Decision method for segment-based labeling.
#'                        One of "mean" (default), "median", or "majority".
#'                        Only used when input is a probs_vector_cube.
#' @export
sits_label_classification.probs_vector_cube <- function(cube, ...,
                                                        label_method = "mean",
                                                        memsize = 4L,
                                                        multicores = 2L,
                                                        output_dir,
                                                        version = "v1",
                                                        progress = TRUE) {
    # Deprecation warning
    warning(.conf("messages",
                   "sits_label_classification_probs_vector_cube_deprecated"),
        call. = FALSE
    )
    # Pre-conditions
    .check_raster_cube_files(cube)
    .check_chr_parameter(label_method, len_min = 1L, len_max = 1L)
    .check_that(
        label_method %in% c("mean", "median", "majority"),
        msg = "label_method must be one of 'mean', 'median', or 'majority'"
    )
    .check_num_parameter(memsize, min = 1L, max = 16384L)
    .check_num_parameter(multicores, min = 1L, max = 2048L)
    .check_output_dir(output_dir)
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)
    # Process each tile sequentially
    .cube_foreach_tile(cube, function(tile) {
        .label_segment_tile(
            tile = tile,
            band = "class",
            label_method = label_method,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    })
}

#' @rdname sits_label_classification
#' @export
sits_label_classification.raster_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_label_classification"))
}

#' @rdname sits_label_classification
#' @export
sits_label_classification.derived_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_label_classification"))
}

#' @rdname sits_label_classification
#' @export
sits_label_classification.default <- function(cube, ...) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        cube <- .cube_find_class(cube)
    } else {
        stop(.conf("messages", "sits_label_classification"))
    }
    sits_label_classification(cube, ...)
}
