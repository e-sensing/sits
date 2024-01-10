#' @title Reduces a cube from a summarization function
#'
#' @name sits_reduce
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Apply a temporal reduction from a named expression in cube or sits tibble.
#' In the case of cubes, it materializes a new band in \code{output_dir}.
#' The result will be a cube with only one date with the raster reduced
#' from the function.
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
#' \code{sits_reduce()} allow any valid R expression to compute new bands.
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
#' @rdname sits_reduce
#' @export
sits_reduce <- function(data, ...) {
    .check_na(data)
    .check_null(data)
    UseMethod("sits_reduce", data)
}

#' @rdname sits_reduce
#' @export
sits_reduce.sits <- function(data, ...) {
    return(NULL)
}


#' @rdname sits_reduce
#' @export
sits_reduce.raster_cube <- function(data, ...,
                                    window_size = 3L,
                                    memsize = 4L,
                                    multicores = 2L,
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
    # Check if band already exists in cube
    if (out_band %in% .cube_bands(data)) {
        if (.check_messages()) {
            warning(
                paste0("The provided band '", out_band,
                       "' already exists in cube."),
                call. = FALSE
            )
        }
        return(data)
    }
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
        npaths = length(in_bands) * length(.tile_timeline(data)),
        nbytes = 8, proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(data)), memsize = memsize,
        multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Reducing
    # Process each tile sequentially
    reduce_cube <- .cube_foreach_tile(data, function(tile) {
        # Reduce the data
        probs_tile <- .reduce_tile(
            tile = tile,
            block = block,
            expr = expr,
            window_size = window_size,
            out_band = out_band,
            in_bands = in_bands,
            overlap = overlap,
            output_dir = output_dir,
            progress = progress
        )
        return(probs_tile)
    })
    # Return the reduced cube
    return(reduce_cube)
}
