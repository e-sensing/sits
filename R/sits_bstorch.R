#' @title Smooth probability cubes with the torch Bayesian smoother
#'
#' @name sits_smooth_torch
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' Torch-backed alternative to \code{\link[sits]{sits_smooth}}.  Applies the
#' same spatial Bayesian smoothing algorithm to a probability cube but
#' delegates the per-block computation to torch tensor operations, enabling
#' GPU acceleration when CUDA or Apple MPS is available.
#'
#' The interface and parameters are identical to \code{sits_smooth()}.
#' Both functions can be run on the same \code{probs_cube} (use different
#' \code{version} strings to avoid cache collisions) and their outputs
#' compared directly.
#'
#' @param  cube              Probability data cube.
#' @param  ...               Other parameters for specific functions.
#' @param  window_size       Size of the neighbourhood
#'                           (integer, min = 3, max = 33, must be odd).
#' @param  neigh_fraction    Fraction of neighbours with high probabilities
#'                           used in Bayesian inference
#'                           (numeric, min = 0.0, max = 1.0).
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter)
#'                           (integer vector or scalar, min = 1, max = 200).
#' @param  exclusion_mask    Areas to exclude from smoothing.  Can be an
#'                           \code{sf} object or a path to a shapefile.
#' @param  memsize           Memory available for processing in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to use
#'                           (integer, min = 1, max = 2048).
#' @param  output_dir        Valid directory for output files
#'                           (character vector of length 1).
#' @param  version           Version string for output files
#'                           (character vector of length 1).
#' @param  progress          Show a progress bar? (logical)
#'
#' @return A probability data cube of the same class as the input.
#'
#' @references
#' Gilberto Camara, Renato Assunção, Alexandre Carvalho, Rolf Simões,
#' Felipe Souza, Felipe Carlos, Anielli Souza, Ana Rorato,
#' Ana Paula Dal'Asta, "Bayesian inference
#' for post-processing of remote sensing image classification".
#' Remote Sensing, 16(23), 4572, 2024. \doi{10.3390/rs16234572}.
#'
#' @examples
#' if (sits_run_examples()) {
#'     xgb_model <- sits_train(samples_modis_ndvi, sits_xgboost())
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = xgb_model, output_dir = tempdir()
#'     )
#'     # Smooth with the torch backend
#'     bayes_cube_torch <- sits_smooth_torch(
#'         probs_cube,
#'         output_dir = tempdir(),
#'         version    = "v1-torch"
#'     )
#'     plot(bayes_cube_torch)
#' }
#' @export
sits_smooth_torch <- function(cube, ...) {
    .check_set_caller("sits_smooth_torch")
    UseMethod("sits_smooth_torch", cube)
}

#' @rdname sits_smooth_torch
#' @export
sits_smooth_torch.probs_cube <- function(cube,
                                         ...,
                                         window_size    = 9L,
                                         neigh_fraction = 0.5,
                                         smoothness     = 20.0,
                                         exclusion_mask = NULL,
                                         memsize        = 4L,
                                         multicores     = 2L,
                                         output_dir,
                                         version        = "v1",
                                         progress       = TRUE) {
    # Require torch
    .check_require_packages("torch")
    # Check probability cube files
    .check_raster_cube_files(cube)
    # Check window size
    .check_int_parameter(window_size, min = 3L, max = 33L, is_odd = TRUE)
    # Check neighbourhood fraction
    .check_num_parameter(neigh_fraction, min = 0.0, max = 1.0)
    # Check memsize
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    # Check multicores
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    # Check output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check version
    version <- .message_version(version)
    # Normalise smoothness
    nlabels <- length(.cube_labels(cube))
    .check_smoothness(smoothness, nlabels)
    if (length(smoothness) == 1L)
        smoothness <- rep(smoothness, nlabels)
    version <- tolower(version)

    # Block / memory sizing  (same logic as sits_smooth.probs_cube)
    block   <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    overlap <- ceiling(window_size / 2L) - 1L
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = overlap),
        npaths     = length(.tile_labels(cube)) * 2L,
        nbytes     = 8L,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize           = memsize,
        multicores        = multicores
    )
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block             = block,
        image_size        = .tile_size(.tile(cube)),
        memsize           = memsize,
        multicores        = multicores
    )
    if (.parallel_start(workers = multicores))
        on.exit(.parallel_stop(), add = TRUE)

    # Call torch-backed orchestrator
    .smooth_torch(
        cube           = cube,
        block          = block,
        window_size    = window_size,
        neigh_fraction = neigh_fraction,
        smoothness     = smoothness,
        exclusion_mask = exclusion_mask,
        multicores     = multicores,
        memsize        = memsize,
        output_dir     = output_dir,
        version        = version,
        progress       = progress
    )
}

#' @rdname sits_smooth_torch
#' @export
sits_smooth_torch.raster_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_smooth_default"))
}

#' @rdname sits_smooth_torch
#' @export
sits_smooth_torch.derived_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_smooth_default"))
}

#' @rdname sits_smooth_torch
#' @export
sits_smooth_torch.default <- function(cube, ...) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        cube <- .cube_find_class(cube)
    } else {
        stop(.conf("messages", "sits_smooth_default"))
    }
    sits_smooth_torch(cube, ...)
}
