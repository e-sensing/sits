#' @title Smooth probability cubes with spatial predictors
#'
#' @name  sits_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @note
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies the ARD image collection
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
#' Machine learning algorithms rely on training samples that are
#' derived from “pure” pixels, hand-picked by users to represent
#' the desired output classes.
#' Given the presence of mixed pixels in images regardless of resolution,
#' and the considerable data variability within each class,
#' these classifiers often produce results with misclassified pixels.
#'
#' Post-processing the results of \code{\link[sits]{sits_classify}}
#' using \code{sits_smooth} reduces salt-and-pepper and border effects.
#' By minimizing noise, \code{sits_smooth} brings a significant gain
#' in the overall accuracy and interpretability of the final output.
#'
#' @references
#' Gilberto Camara, Renato Assunção, Alexandre Carvalho, Rolf Simões,
#' Felipe Souza, Felipe Carlos, Anielli Souza, Ana Rorato,
#' Ana Paula Del’Asta, “Bayesian inference
#' for post-processing of remote sensing image classification”.
#' Remote Sensing, 16(23), 4572, 2024. DOI: https://doi.org/10.3390/rs16234572.
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
