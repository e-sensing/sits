#' @title Apply a set of texture measures on a data cube.
#'
#' @name sits_texture
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description A set of texture measures based on the Grey Level Co-occurrence
#' Matrix (GLCM) described by Haralick. Our implementation
#' follows the guidelines and equations described by Hall-Beyer
#' (both are referenced below). At the image border, the window is filled
#' by mirroring the image without repeating the edge pixel.
#'
#' With a GPU that torch can use (CUDA or MPS), each block runs with torch
#' on the GPU; otherwise it runs with the C++ code. Either way the blocks
#' run on \code{multicores} workers, fewer if \code{memsize} (and, on the
#' GPU, \code{gpu_memsize}) does not allow them. Set the environment variable
#' \code{SITS_FORCE_CPU=TRUE} to use the C++ code on a GPU machine.
#'
#' @references
#' Robert M. Haralick, K. Shanmugam, Its'Hak Dinstein,
#' "Textural Features for Image Classification",
#' IEEE Transactions on Systems, Man, and Cybernetics,
#' SMC-3, 6, 610-621, 1973, \doi{10.1109/TSMC.1973.4309314}.
#'
#' Hall-Beyer, M., "GLCM Texture Tutorial",
#' 2007, \doi{10.13140/RG.2.2.12424.21767}.
#'
#' Hall-Beyer, M., "Practical guidelines for choosing GLCM textures to use
#' in landscape classification tasks over a range of moderate spatial scales",
#' International Journal of Remote Sensing, 38, 1312–1338, 2017,
#' \doi{10.1080/01431161.2016.1278314}.
#'
#' A. Baraldi and F. Panniggiani, "An investigation of the textural
#' characteristics associated with gray level co-occurrence matrix statistical
#' parameters," IEEE Transactions on Geoscience and Remote Sensing, 33, 2,
#' 293-304, 1995, \doi{10.1109/TGRS.1995.8746010}.
#'
#' Shokr, M. E., "Evaluation of second-order texture parameters for sea ice
#' classification from radar images",
#' J. Geophys. Res., 96, 10625–10640, 1991, \doi{10.1029/91JC00693}.
#'
#' Peng Gong, Danielle J. Marceau, Philip J. Howarth, "A comparison of
#' spatial feature extraction algorithms for land-use classification
#' with SPOT HRV data", Remote Sensing of Environment, 40, 2, 1992, 137-151,
#' \doi{10.1016/0034-4257(92)90011-8}.
#'
#' @param cube          Valid sits cube
#' @param window_size   An odd number representing the size of the
#'                      sliding window (at least 3).
#' @param angles        The direction angles in radians related to the
#'                      central pixel and its neighbor (See details).
#'                      Default is 0.
#' @param memsize       Memory the call may add to the R session (in GB).
#'                      The call itself takes about 2 GB on the CPU and
#'                      1.7 GB on the GPU (main process and merges);
#'                      each worker takes the rest.
#' @param multicores    Number of workers. A cluster is opened when none is
#'                      open; an open cluster is limited to this number of
#'                      workers for the call; a cluster smaller than
#'                      \code{multicores} is not enlarged. NULL uses the open cluster,
#'                      else 2. A warning tells when memory allows fewer.
#' @param gpu_memsize   Memory of the GPU (in GB), used only when torch runs
#'                      on a GPU; it limits the workers and the block size.
#' @param block_size    Size of the blocks (a named vector with \code{nrows}
#'                      and \code{ncols}); NULL sizes them from the memory.
#' @param output_dir    Directory where files will be saved.
#' @param progress      Show progress bar?
#' @param ...           GLCM function (see details).
#'
#' @details
#' The spatial relation between the central pixel and its neighbor is expressed
#' in radians values, where:
#' #' \itemize{
#' \item{\code{0}:      corresponds to the neighbor on right-side}
#' \item{\code{pi/4}:   corresponds to the neighbor on the top-right diagonals}
#' \item{\code{pi/2}:   corresponds to the neighbor on above}
#' \item{\code{3*pi/4}: corresponds to the neighbor on the top-left diagonals}
#' }
#'
#' Our implementation relies on a symmetric co-occurrence matrix, which
#' considers the opposite directions of an angle. For example, the neighbor
#' pixels based on \code{0} angle rely on the left and right direction; the
#' neighbor pixels of \code{pi/2} are above and below the central pixel, and
#' so on. If more than one angle is provided, we compute their average.
#'
#' @section Available texture functions:
#' \itemize{
#' \item{\code{glcm_contrast()}: measures the contrast or the amount of local
#' variations present in an image. Low contrast values indicate regions with
#' low spatial frequency.}
#' \item{\code{glcm_homogeneity()}: also known as the Inverse Difference
#' Moment, it measures image homogeneity by assuming larger values for
#' smaller gray tone differences in pair elements.}
#' \item{\code{glcm_asm()}: the Angular Second Moment (ASM) measures textural
#' uniformity. High ASM values indicate a constant or a periodic form in the
#' window values.}
#' \item{\code{glcm_energy()}: measures textural uniformity. Energy is
#' defined as the square root of the ASM. }
#' \item{\code{glcm_mean()}: measures the mean of the probability of
#' co-occurrence of specific pixel values within the neighborhood.}
#' \item{\code{glcm_variance()}: measures the heterogeneity and is strongly
#' correlated to first order statistical variables such as standard deviation.
#' Variance values increase as the gray-level values deviate from their mean.}
#' \item{\code{glcm_std()}: measures the heterogeneity and is strongly
#' correlated to first order statistical variables such as standard deviation.
#' STD is defined as the square root of the variance.}
#' \item{\code{glcm_correlation()}: measures the gray-tone linear dependencies
#' of the image. Low correlation values indicate homogeneous region edges.}
#' }
#'
#' @return A sits cube with new bands, produced according to the requested
#' measure.
#'
#' @examples
#' if (sits_run_examples()) {
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'
#'     # Compute the NDVI variance
#'     cube_texture <- sits_texture(
#'         cube = cube,
#'         NDVIVAR = glcm_variance(NDVI),
#'         window_size = 5,
#'         output_dir = tempdir()
#'     )
#' }
#' @rdname sits_texture
#' @family data cube transformation
#' @export
sits_texture <- function(cube, ...) {
    .check_set_caller("sits_texture")
    .check_na_null_parameter(cube)
    UseMethod("sits_texture", cube)
}

#' @rdname sits_texture
#' @export
sits_texture.raster_cube <- function(cube, ...,
                                     window_size = 3L,
                                     angles = 0.0,
                                     memsize = 8L,
                                     multicores = NULL,
                                     gpu_memsize = 4L,
                                     block_size = NULL,
                                     output_dir,
                                     progress = TRUE) {
    .debug_log(event = "start_call", key = "tiles", value = nrow(cube))
    # registered first, so it also pairs start_call on an early return
    on.exit(
        .debug_log(event = "end_call", key = "tiles", value = nrow(cube)),
        add = TRUE
    )
    # Check cube
    .check_is_raster_cube(cube)
    .check_that(.cube_is_regular(cube))
    # Check window size
    .check_int_parameter(window_size, min = 3L, is_odd = TRUE)
    # Check normalized index
    .check_num_parameter(angles, len_min = 1L, len_max = 4L)
    # Check memsize
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    # Check multicores
    .check_int_parameter(
        multicores, min = 1L, max = 2048L, allow_null = TRUE
    )
    # Check block_size
    .check_block_size(block_size, cube)
    # Check gpu_memsize
    .check_num_parameter(gpu_memsize, min = 1.0)
    # Check output_dir
    .check_output_dir(output_dir)
    # show progress bar?
    progress <- .message_progress(progress)

    # Get cube bands
    bands <- .cube_bands(cube)
    # Get output band expression
    expr <- .apply_capture_expression(...)
    out_band <- names(expr)
    # Check if band already exists in cube
    if (out_band %in% bands) {
        if (.message_warnings()) {
            warning(.conf("messages", "sits_texture_out_band"),
                call. = FALSE
            )
        }
        return(cube)
    }
    # Get all input bands in cube data
    in_bands <- .apply_input_bands(
        cube = cube,
        bands = bands,
        expr = expr
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Get block size
    block <- .texture_blocksize(cube)
    # the reflect mirror skips the edge pixel, so a chunk needs more than
    # overlap rows and columns; a chunk smaller than its tile has overlap on
    # one side at least, so only the tile size can fail
    .check_that(
        min(.cube_nrows(cube), .cube_ncols(cube)) > overlap,
        msg = .conf("messages", "sits_texture_block_size")
    )
    # the request: the given workers, else the open cluster, else 2
    multicores <- .jobs_request_workers(multicores)
    # torch on a GPU, C++ on the CPU
    backend <- .texture_backend(
        gpu_memsize = gpu_memsize, metrics = all.names(expr)
    )
    # the workers are sized for the block of the caller, if given
    sizing <- backend[["size"]](
        block = .default(block_size, block),
        overlap = overlap,
        npaths = length(in_bands) + 1L,
        window_size = window_size,
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    block <- .default(block_size, sizing[["block"]])
    .jobs_warn_workers(
        asked = multicores, workers = sizing[["multicores"]],
        limit = sizing[["limit"]]
    )
    multicores <- sizing[["multicores"]]
    # Prepare parallel processing
    # keep a debug log the caller turned on, also in the workers
    started <- .parallel_start(
        workers = multicores, log = .debug(), output_dir = output_dir
    )
    # runs before end_call, so the call holds the stop of the cluster
    on.exit(.parallel_stop(started), add = TRUE, after = FALSE)
    # an open cluster larger than multicores runs with multicores nodes
    .parallel_force_multicores(multicores)
    on.exit(.parallel_force_multicores(), add = TRUE)
    # Create features as jobs
    features_cube <- .cube_split_features(cube)
    jobs <- .texture_jobs(
        features_cube, out_band = out_band, block = block,
        overlap = overlap, output_dir = output_dir
    )
    # One queue over every feature: the merge of a feature runs in a worker
    # while the others run the blocks of the next feature
    features <- .jobs_map_queue(
        jobs, fn = .texture_job, window_size = window_size, angles = angles,
        expr = expr, out_band = out_band, in_bands = in_bands,
        texture_fn = backend[["fns"]],
        on_start = .texture_log_start, on_last = .texture_log_last,
        on_end = .texture_log_end, progress = progress
    )
    features_band <- dplyr::bind_rows(features)
    # Join output features as a cube and return it
    .cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_band)))
}

#' @rdname sits_texture
#' @export
sits_texture.derived_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_texture_derived_cube"))
}
#' @rdname sits_texture
#' @export
sits_texture.default <- function(cube, ...) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        cube <- .cube_find_class(cube)
    } else {
        stop(.conf("messages", "sits_texture_default"))
    }
    sits_texture(cube, ...)
}
