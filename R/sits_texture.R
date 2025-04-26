#' @title Apply a set of texture measures on a data cube.
#'
#' @name sits_texture
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description A set of texture measures based on the Grey Level Co-occurrence
#' Matrix (GLCM) described by Haralick. Our implementation
#' follows the guidelines and equations described by Hall-Beyer
#' (both are referenced below).
#'
#' @references
#' Robert M. Haralick, K. Shanmugam, Its'Hak Dinstein,
#' "Textural Features for Image Classification",
#' IEEE Transactions on Systems, Man, and Cybernetics,
#' SMC-3, 6, 610-621, 1973, DOI: 10.1109/TSMC.1973.4309314.
#'
#' Hall-Beyer, M., "GLCM Texture Tutorial",
#' 2007, http://www.fp.ucalgary.ca/mhallbey/tutorial.htm.
#'
#' Hall-Beyer, M., "Practical guidelines for choosing GLCM textures to use
#' in landscape classification tasks over a range of moderate spatial scales",
#' International Journal of Remote Sensing, 38, 1312–1338, 2017,
#' DOI: 10.1080/01431161.2016.1278314.
#'
#' A. Baraldi and F. Panniggiani, "An investigation of the textural
#' characteristics associated with gray level cooccurrence matrix statistical
#' parameters," IEEE Transactions on Geoscience and Remote Sensing, 33, 2,
#' 293-304, 1995, DOI: 10.1109/TGRS.1995.8746010.
#'
#' Shokr, M. E., "Evaluation of second-order texture parameters for sea ice
#' classification from radar images",
#' J. Geophys. Res., 96, 10625–10640, 1991, DOI:10.1029/91JC00693.
#'
#' Peng Gong, Danielle J. Marceau, Philip J. Howarth, "A comparison of
#' spatial feature extraction algorithms for land-use classification
#' with SPOT HRV data", Remote Sensing of Environment, 40, 2, 1992, 137-151,
#' DOI: 10.1016/0034-4257(92)90011-8.
#'
#' @param cube          Valid sits cube
#' @param window_size   An odd number representing the size of the
#'                      sliding window.
#' @param angles        The direction angles in radians related to the
#'                      central pixel and its neighbor (See details).
#'                      Default is 0.
#' @param memsize       Memory available for classification (in GB).
#' @param multicores    Number of cores to be used for classification.
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
                                     memsize = 4L,
                                     multicores = 2L,
                                     output_dir,
                                     progress = TRUE) {
    # Check cube
    .check_is_raster_cube(cube)
    .check_that(.cube_is_regular(cube))
    # Check window size
    .check_int_parameter(window_size, min = 1L, is_odd = TRUE)
    # Check normalized index
    .check_num_parameter(angles, len_min = 1L, len_max = 4L)
    # Check memsize
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    # Check multicores
    .check_int_parameter(multicores, min = 1L, max = 2048L)
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
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = overlap),
        npaths = length(in_bands) + 1L,
        nbytes = 8L,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Create features as jobs
    features_cube <- .cube_split_features(cube)

    # Process each feature in parallel
    features_band <- .jobs_map_sequential_dfr(features_cube, function(feature) {
        # Process the data
        .texture_feature(
            feature = feature,
            block = block,
            expr = expr,
            window_size = window_size,
            angles = angles,
            out_band = out_band,
            in_bands = in_bands,
            overlap = overlap,
            output_dir = output_dir,
            progress = progress
        )
    })
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
    } else if (all(.conf("sits_tibble_cols") %in% colnames(cube))) {
        class(cube) <- c("sits", class(cube))
    } else {
        stop(.conf("messages", "sits_texture_default"))
    }
    sits_texture(cube, ...)
}
