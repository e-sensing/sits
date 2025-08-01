#' @title Apply a function on a set of time series
#'
#' @name sits_apply
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Apply a named expression to a sits cube or a sits tibble
#' to be evaluated and generate new bands (indices). In the case of sits
#' cubes, it creates a new band in \code{output_dir}.
#'
#' @param data          Valid sits tibble or cube
#' @param window_size   An odd number representing the size of the
#'                      sliding window of sits kernel functions
#'                      used in expressions (for a list of supported
#'                      kernel functions, please see details).
#' @param memsize       Memory available for classification (in GB).
#' @param multicores    Number of cores to be used for classification.
#' @param output_dir    Directory where files will be saved.
#' @param normalized    Does the expression produces a normalized band?
#' @param progress      Show progress bar?
#' @param ...           Named expressions to be evaluated (see details).
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
#' \code{sits_apply()} allows any valid R expression to compute new bands.
#' Use R syntax to pass an expression to this function.
#' Besides arithmetic operators, you can use virtually any R function
#' that can be applied to elements of a matrix (functions that are
#' unaware of matrix sizes, e.g. \code{sqrt()}, \code{sin()},
#' \code{log()}).
#'
#' Examples of valid expressions:
#' \enumerate{
#' \item \code{NDVI = (B08 - B04/(B08 + B04))} for Sentinel-2 images.
#' \item \code{EVI = 2.5 * (B05 – B04) / (B05 + 6 * B04 – 7.5 * B02 + 1)} for
#' Landsat-8/9 images.
#' \item \code{VV_VH_RATIO = VH/VV} for Sentinel-1 images. In this case,
#' set the \code{normalized} parameter to FALSE.
#' \item \code{VV_DB = 10 * log10(VV)} to convert Sentinel-1 RTC images
#' available in Planetary Computer to decibels. Also, set the
#' \code{normalized} parameter to FALSE.
#' }
#'
#' \code{sits_apply()} accepts a predefined set of kernel functions
#' (see below) that can be applied to pixels considering its
#' neighborhood. \code{sits_apply()} considers a neighborhood of a
#' pixel as a set of pixels equidistant to it (including itself).
#' This neighborhood forms a square window (also known as kernel)
#' around the central pixel
#' (Moore neighborhood). Users can set the \code{window_size}
#' parameter to adjust the size of the kernel window.
#' The image is conceptually mirrored at the edges so that neighborhood
#' including a pixel outside the image is equivalent to take the
#' 'mirrored' pixel inside the edge.
#' \code{sits_apply()} applies a function to the kernel and its result
#' is assigned to a corresponding central pixel on a new matrix.
#' The kernel slides throughout the input image and this process
#' generates an entire new matrix, which is returned as a new band
#' to the cube. The kernel functions ignores any \code{NA} values
#' inside the kernel window. If all pixels in the window are \code{NA}
#' the result will be \code{NA}.
#'
#' By default, the indexes generated by \code{sits_apply()} function are
#' normalized between -1 and 1, scaled by a factor of 0.0001.
#' Normalized indexes are saved as INT2S (Integer with sign).
#' If the \code{normalized} parameter is FALSE, no scaling factor will be
#' applied and the index will be saved as FLT4S (signed float) and
#' the values will vary between -3.4e+38 and 3.4e+38.
#'
#' @section Kernel functions available:
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
#'     # get a time series
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
#'         collection = "MOD13Q1-6.1",
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
    .check_set_caller("sits_apply")
    .check_na_null_parameter(data)
    UseMethod("sits_apply", data)
}

#' @rdname sits_apply
#' @export
sits_apply.sits <- function(data, ...) {
    .check_samples(data)
    .apply(data, col = "time_series", fn = dplyr::mutate, ...)
}

#' @rdname sits_apply
#' @export
sits_apply.raster_cube <- function(data, ...,
                                   window_size = 3L,
                                   memsize = 4L,
                                   multicores = 2L,
                                   normalized = TRUE,
                                   output_dir,
                                   progress = TRUE) {
    # check cube
    .check_is_raster_cube(data)
    .check_cube_is_regular(data)
    # check window size
    .check_int_parameter(window_size, min = 1L, is_odd = TRUE)
    # check normalized index
    .check_lgl_parameter(normalized)
    # check memsize
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    # check multicores
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    # check output_dir
    .check_output_dir(output_dir)
    # show progress bar?
    progress <- .message_progress(progress)

    # get cube bands
    bands <- .cube_bands(data)
    # get output band expression
    expr <- .apply_capture_expression(...)
    out_band <- names(expr)
    # check if band already exists in cube
    if (out_band %in% bands) {
        if (.message_warnings()) {
            warning(.conf("messages", "sits_apply_out_band"),
                call. = FALSE
            )
        }
        return(data)
    }
    # get all input bands in cube data
    in_bands <- .apply_input_bands(
        cube = data,
        bands = bands,
        expr = expr
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = overlap),
        npaths = length(in_bands) + 1L,
        nbytes = 8L,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # update block parameter
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(data)),
        memsize = memsize,
        multicores = multicores
    )

    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Create features as jobs
    features_cube <- .cube_split_features(data)

    # Process each feature in parallel
    features_band <- .jobs_map_parallel_dfr(features_cube, function(feature) {
        # Process the data
        .apply_feature(
            feature = feature,
            block = block,
            expr = expr,
            window_size = window_size,
            out_band = out_band,
            in_bands = in_bands,
            overlap = overlap,
            normalized = normalized,
            output_dir = output_dir
        )
    }, progress = progress)
    # Join output features as a cube and return it
    .cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_band)))
}
#' @rdname sits_apply
#' @export
sits_apply.derived_cube <- function(data, ...) {
    stop(.conf("messages", "sits_apply_derived_cube"))
}
#' @rdname sits_apply
#' @export
sits_apply.default <- function(data, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_apply_default"))
    }
    sits_apply(data, ...)
}
