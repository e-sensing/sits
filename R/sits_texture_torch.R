#' @title Apply a set of texture measures on a data cube using torch.
#'
#' @name sits_texture_torch
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Torch-accelerated variant of \code{\link[sits]{sits_texture}}.
#' Computes the same Grey Level Co-occurrence Matrix (GLCM) texture measures
#' but routes each block's GLCM calculation through torch tensor operations,
#' enabling CPU, CUDA, or MPS acceleration.
#'
#' The function falls back to the C++ backend if torch is unavailable (i.e.
#' \code{\link[torch]{torch_is_installed}} returns \code{FALSE}).
#' Set the environment variable \code{SITS_TEXTURE_FORCE_CPP=TRUE} to force
#' the C++ backend regardless of torch availability.
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
#' International Journal of Remote Sensing, 38, 1312-1338, 2017,
#' \doi{10.1080/01431161.2016.1278314}.
#'
#' @param cube          Valid sits cube.
#' @param window_size   An odd number representing the size of the
#'                      sliding window.
#' @param angles        The direction angles in radians related to the
#'                      central pixel and its neighbor (See details).
#'                      Default is 0.
#' @param memsize       Memory available for classification (in GB).
#' @param multicores    Number of cores to be used for classification.
#' @param output_dir    Directory where files will be saved.
#' @param progress      Show progress bar?
#' @param ...           GLCM function (see details and
#'                      \code{\link[sits]{sits_texture}}).
#'
#' @details
#' The spatial relation between the central pixel and its neighbor is expressed
#' in radians values, where:
#' \itemize{
#' \item{\code{0}:      corresponds to the neighbor on right-side}
#' \item{\code{pi/4}:   corresponds to the neighbor on the bottom-right diagonal}
#' \item{\code{pi/2}:   corresponds to the neighbor below}
#' \item{\code{3*pi/4}: corresponds to the neighbor on the bottom-left diagonal}
#' }
#'
#' Our implementation relies on a symmetric co-occurrence matrix.
#' If more than one angle is provided, we compute their average.
#'
#' @section Available texture functions:
#' \itemize{
#' \item{\code{glcm_contrast()}}
#' \item{\code{glcm_dissimilarity()}}
#' \item{\code{glcm_homogeneity()}}
#' \item{\code{glcm_asm()}}
#' \item{\code{glcm_energy()}}
#' \item{\code{glcm_mean()}}
#' \item{\code{glcm_variance()}}
#' \item{\code{glcm_std()}}
#' \item{\code{glcm_correlation()}}
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
#'     # Compute the NDVI variance using the torch backend
#'     cube_texture <- sits_texture_torch(
#'         cube = cube,
#'         NDVIVAR = glcm_variance(NDVI),
#'         window_size = 5,
#'         output_dir = tempdir()
#'     )
#' }
#' @rdname sits_texture_torch
#' @export
sits_texture_torch <- function(cube, ...) {
    .check_set_caller("sits_texture_torch")
    .check_na_null_parameter(cube)
    UseMethod("sits_texture_torch", cube)
}

#' @rdname sits_texture_torch
#' @export
sits_texture_torch.raster_cube <- function(cube, ...,
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
    # Check angles
    .check_num_parameter(angles, len_min = 1L, len_max = 4L)
    # Check memsize
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    # Check multicores
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    # Check output_dir
    .check_output_dir(output_dir)
    # Show progress bar?
    progress <- .message_progress(progress)

    # Select implementation: torch when available, C++ as fallback
    use_torch <- .texture_torch_available()
    feature_fn <- if (use_torch) {
        .texture_torch_feature
    } else {
        .texture_feature
    }

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
    # Get all input bands consumed by the expression
    in_bands <- .apply_input_bands(
        cube  = cube,
        bands = bands,
        expr  = expr
    )
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Get block size
    block <- .texture_blocksize(cube)
    # Choose processing bloat: GPU path serialises to one core
    use_gpu <- use_torch && .torch_gpu_available()
    proc_bloat <- if (use_gpu) {
        .conf("processing_bloat_gpu")
    } else {
        .conf("processing_bloat_cpu")
    }
    # Number of pixels in a block including overlap
    block_size <- .block_size(block = block, overlap = overlap)
    # Estimate memory for one block (I/O path)
    job_block_memsize <- .jobs_block_memsize(
        block_size = block_size,
        npaths     = length(in_bands) + 1L,
        nbytes     = 8L,
        proc_bloat = proc_bloat
    )
    # Add memory for the wins tensor [npix, ws, ws] materialised by torch
    if (use_torch) {
        job_block_memsize <- job_block_memsize +
            .texture_torch_block_memsize(
                block_size  = block_size,
                window_size = window_size
            )
    }
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize           = memsize,
        multicores        = multicores
    )
    # Resize block so it fits within memsize / multicores
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block             = block,
        image_size        = .tile_size(.tile(cube)),
        memsize           = memsize,
        multicores        = multicores
    )
    # A single GPU must not be shared across workers
    # (done after block adjustment so the full memsize budget sizes the block)
    if (use_gpu) {
        multicores <- 1L
    }
    # Prepare parallel processing
    started <- .parallel_start(workers = multicores)
    on.exit(.parallel_stop(started), add = TRUE)
    # Split cube into individual tile–date features
    features_cube <- .cube_split_features(cube)

    # Process each feature sequentially (features are independent)
    features_band <- .jobs_map_sequential_dfr(features_cube, function(feature) {
        feature_fn(
            feature    = feature,
            block      = block,
            expr       = expr,
            window_size= window_size,
            angles     = angles,
            out_band   = out_band,
            in_bands   = in_bands,
            overlap    = overlap,
            output_dir = output_dir,
            progress   = progress
        )
    })
    # Merge original and new band tiles into a single cube
    .cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_band)))
}

#' @rdname sits_texture_torch
#' @export
sits_texture_torch.derived_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_texture_derived_cube"))
}

#' @rdname sits_texture_torch
#' @export
sits_texture_torch.default <- function(cube, ...) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        cube <- .cube_find_class(cube)
    } else {
        stop(.conf("messages", "sits_texture_default"))
    }
    sits_texture_torch(cube, ...)
}
