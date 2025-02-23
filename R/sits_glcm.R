#' @title Apply a GLCM metric on a data cube
#'
#' @name sits_glcm
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description ...
#'
#' @param data          Valid sits tibble or cube
#' @param window_size   An odd number representing the size of the
#'                      sliding window of sits kernel functions
#'                      used in expressions (for a list of supported
#'                      kernel functions, please see details).
#' @param angles        ...
#' @param memsize       Memory available for classification (in GB).
#' @param multicores    Number of cores to be used for classification.
#' @param output_dir    Directory where files will be saved.
#' @param progress      Show progress bar?
#' @param ...           Named expressions to be evaluated (see details).
#'
#' @section Summarizing GLCM functions:
#' \itemize{
#' \item{\code{glcm_contrast()}: ...}
#' \item{\code{glcm_dissimilarity()}: ...}
#' \item{\code{glcm_homogeneity()}: ...}
#' \item{\code{glcm_energy()}: ...}
#' \item{\code{glcm_asm()}: ...}
#' \item{\code{glcm_mean()}: ...}
#' \item{\code{glcm_variance()}: ...}
#' \item{\code{glcm_std()}: ...}
#' \item{\code{glcm_correlation()}: ...}
#' }
#'
#' @return A sits cube with new bands, produced
#'         according to the requested expression.
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
#'     # Generate a texture images with variance in NDVI images
#'     cube_texture <- sits_glcm(
#'         data = cube,
#'         NDVIMEAN = glcm_mean(NDVI),
#'         window_size = 5,
#'         angles = 0,
#'         output_dir = tempdir()
#'     )
#' }
#' @rdname sits_glcm
#' @export
sits_glcm <- function(data, ...) {
    .check_set_caller("sits_glcm")
    .check_na_null_parameter(data)
    UseMethod("sits_glcm", data)
}

#' @rdname sits_glcm
#' @export
sits_glcm.raster_cube <- function(data, ...,
                                  window_size = 3L,
                                  angles = 0,
                                  memsize = 4L,
                                  multicores = 2L,
                                  output_dir,
                                  progress = FALSE) {
    # Check cube
    .check_is_raster_cube(data)
    .check_that(.cube_is_regular(data))
    # Check window size
    .check_int_parameter(window_size, min = 1, is_odd = TRUE)
    # Check normalized index
    .check_num_parameter(angles, len_min = 1, len_max = 4)
    # Check memsize
    .check_int_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_int_parameter(multicores, min = 1, max = 2048)
    # Check output_dir
    .check_output_dir(output_dir)

    # Get cube bands
    bands <- .cube_bands(data)
    # Get output band expression
    expr <- .apply_capture_expression(...)
    out_band <- names(expr)
    # Check if band already exists in cube
    if (out_band %in% bands) {
        if (.check_messages()) {
            warning(.conf("messages", "sits_apply_out_band"),
                    call. = FALSE
            )
        }
        return(data)
    }
    # Get all input bands in cube data
    in_bands <- .apply_input_bands(
        cube = data,
        bands = bands,
        expr = expr
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = length(in_bands) + 1,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(data)),
        memsize = memsize,
        multicores = multicores
    )
    # adjust for blocks of size 1
    block <- .block_regulate_size(block)
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
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
        output_feature <- .glcm_feature(
            feature = feature,
            block = block,
            expr = expr,
            window_size = window_size,
            angles = angles,
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

#' @rdname sits_glcm
#' @export
sits_glcm.derived_cube <- function(data, ...) {
    stop(.conf("messages", "sits_glcm_derived_cube"))
}
#' @rdname sits_glcm
#' @export
sits_glcm.default <- function(data, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_glcm_default"))
    }

    acc <- sits_glcm(data, ...)
    return(acc)
}
