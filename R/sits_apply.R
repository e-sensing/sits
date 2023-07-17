#' @title Apply a function on a set of time series
#'
#' @name sits_apply
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Apply a named expression to a sits cube or a sits tibble
#' to be evaluated and generate new bands (indices). In the case of sits
#' cubes, it materializes a new band in \code{output_dir} using
#' \code{gdalcubes}.
#'
#' @param data          Valid sits tibble or cube
#' @param window_size   An odd number representing the size of the
#'                      sliding window of sits kernel functions
#'                      used in expressions (for a list of supported
#'                      kernel functions, please see details).
#' @param memsize       Memory available for classification (in GB).
#' @param multicores    Number of cores to be used for classification.
#' @param output_dir    Directory where files will be saved.
#' @param progress      Show progress bar?
#' @param ...           Named expressions to be evaluated (see details).
#'
#' @details
#' \code{sits_apply()} allow any valid R expression to compute new bands.
#' Use R syntax to pass an expression to this function.
#' Besides arithmetic operators, you can use virtually any R function
#' that can be applied to elements of a matrix (functions that are
#' unaware of matrix sizes, e.g. \code{sqrt()}, \code{sin()},
#' \code{log()}).
#'
#' Also, \code{sits_apply()} accepts a predefined set of kernel functions
#' (see below) that can be applied to pixels considering its
#' neighborhood. \code{sits_apply()} considers a neighborhood of a
#' pixel as a set of pixels equidistant to it (including itself)
#' according the Chebyshev distance. This neighborhood form a
#' square window (also known as kernel) around the central pixel
#' (Moore neighborhood). Users can set the \code{window_size}
#' parameter to adjust the size of the kernel window.
#' The image is conceptually mirrored at the edges so that neighborhood
#' including a pixel outside the image is equivalent to take the
#' 'mirrored' pixel inside the edge.
#'
#' \code{sits_apply()} applies a function to the kernel and its result
#' is assigned to a corresponding central pixel on a new matrix.
#' The kernel slides throughout the input image and this process
#' generates an entire new matrix, which is returned as a new band
#' to the cube. The kernel functions ignores any \code{NA} values
#' inside the kernel window. Central pixel is \code{NA} just only
#' all pixels in the window are \code{NA}.
#'
#' @section Summarizing kernel functions:
#' \itemize{
#' \item{\code{w_median()}: returns the median of the neighborhood's values.}
#' \item{\code{w_sum()}: returns the sum of the neighborhood's values.}
#' \item{\code{w_mean()}: returns the mean of the neighborhood's values.}
#' \item{\code{w_sd()}: returns the standard deviation of the neighborhood's
#'   values.}
#' \item{\code{w_min()}: returns the minimum of the neighborhood's values.}
#' \item{\code{w_max()}: returns the maximum of the neighborhood's values.}
#' \item{\code{w_var()}: returns the variance of the neighborhood's values.}
#' \item{\code{w_modal()}: returns the modal of the neighborhood's values.}
#' }
#'
#' @return A sits tibble or a sits cube with new bands, produced
#'         according to the requested expression.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Get a time series
#'     # Apply a normalization function
#'
#'     point2 <-
#'         sits_select(point_mt_6bands, "NDVI") |>
#'         sits_apply(NDVI_norm = (NDVI - min(NDVI)) / (max(NDVI) - min(NDVI)))
#'
#'     # Example of generation texture band with variance
#'     # Create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'
#'     # Generate a texture images with variance in NDVI images
#'     cube_texture <- sits_apply(
#'         data = cube,
#'         NDVITEXTURE = w_median(NDVI),
#'         window_size = 5,
#'         output_dir = tempdir()
#'     )
#' }
#' @rdname sits_apply
#' @export
sits_apply <- function(data, ...) {
    .check_na(data)
    .check_null(data)
    UseMethod("sits_apply", data)
}

#' @rdname sits_apply
#' @export
sits_apply.sits <- function(data, ...) {
    .check_samples(data)
    .check_set_caller("sits_apply.sits")

    .apply(data, col = "time_series", fn = dplyr::mutate, ...)
}

#' @rdname sits_apply
#' @export
sits_apply.raster_cube <- function(data, ...,
                                   window_size = 3,
                                   memsize = 1,
                                   multicores = 2,
                                   output_dir,
                                   progress = FALSE) {
    # Check cube
    .check_is_raster_cube(data)
    .check_is_regular(data)
    # Check window size
    .check_window_size(window_size)
    # Check memsize
    .check_memsize(memsize, min = 1, max = 16384)
    # Check multicores
    .check_multicores(multicores, min = 1, max = 2048)
    # Check output_dir
    .check_output_dir(output_dir)

    # Get output band expression
    expr <- .apply_capture_expression(...)
    out_band <- names(expr)
    # Get all input bands in cube data
    in_bands <- .apply_input_bands(data, expr = expr)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = length(in_bands) + 1,
        nbytes = 8, proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Create features as jobs
    features_cube <- .cube_split_features(data)

    # Process each feature in parallel
    features_band <- .jobs_map_parallel_dfr(features_cube, function(feature) {
        # Process the data
        output_feature <- .apply_feature(
            feature = feature,
            block = block,
            expr = expr,
            window_size = window_size,
            out_band = out_band,
            in_bands = in_bands,
            overlap = overlap,
            output_dir = output_dir
        )
        return(output_feature)
    }, progress = progress)
    # Join output features as a cube and return it
    .cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_band)))
}
