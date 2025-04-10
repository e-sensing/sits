#' @title Multiple endmember spectral mixture analysis
#'
#' @name sits_mixture_model
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Create a multiple endmember spectral mixture analyses fractions
#' images. We use the non-negative least squares (NNLS) solver to calculate the
#' fractions of each endmember. The NNLS was implemented by Jakob
#' Schwalb-Willmann in RStoolbox package (licensed as GPL>=3).
#'
#' @references \code{RStoolbox} package (https://github.com/bleutner/RStoolbox/)
#'
#' @param data        A sits data cube or a sits tibble.
#' @param endmembers  Reference spectral endmembers.
#'                    (see details below).
#' @param  ...        Parameters for specific functions.
#' @param rmse_band   A boolean indicating whether the error associated
#'                    with the linear model should be generated.
#'                    If true, a new band with errors for each pixel
#'                    is generated using the root mean square
#'                    measure (RMSE). Default is TRUE.
#' @param memsize     Memory available for the mixture model (in GB).
#' @param multicores  Number of cores to be used for generate the
#'                    mixture model.
#' @param output_dir  Directory for output images.
#' @param progress    Show progress bar? Default is TRUE.
#'
#' @return In case of a cube, a sits cube with the fractions of each endmember
#'         will be returned. The sum of all fractions is restricted
#'         to 1 (scaled from 0 to 10000), corresponding to the abundance of
#'         the endmembers in the pixels.
#'         In case of a sits tibble, the time series will be returned with the
#'         values corresponding to each fraction.
#'
#' @note
#' Many pixels in images of medium-resolution satellites
#' such as Landsat or Sentinel-2 contain a mixture of
#' spectral responses of different land cover types.
#' In many applications, it is desirable to obtain the proportion
#' of a given class inside a mixed pixel. For this purpose,
#' the literature proposes mixture models; these models represent
#' pixel values as a combination of multiple pure land cover types.
#' Assuming that the spectral response of pure land cover classes
#' (called endmembers) is known, spectral mixture analysis
#' derives new bands containing the proportion of each endmember inside a pixel.
#'
#' The \code{endmembers} parameter should be a tibble, csv or
#' a shapefile. \code{endmembers} parameter must have the following columns:
#' \code{type}, which defines the endmembers that will be
#' created and the columns corresponding to the bands that will be used in the
#' mixture model. The band values must follow the product scale.
#' For example, in the case of sentinel-2 images the bands should be in the
#' range 0 to 1. See the \code{example} in this documentation for more details.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Create a sentinel-2 cube
#'     s2_cube <- sits_cube(
#'         source = "AWS",
#'         collection = "SENTINEL-2-L2A",
#'         tiles = "20LKP",
#'         bands = c("B02", "B03", "B04", "B8A", "B11", "B12", "CLOUD"),
#'         start_date = "2019-06-13",
#'         end_date = "2019-06-30"
#'     )
#'     # create a directory to store the regularized file
#'     reg_dir <- paste0(tempdir(), "/mix_model")
#'     dir.create(reg_dir)
#'     # Cube regularization for 16 days and 160 meters
#'     reg_cube <- sits_regularize(
#'         cube = s2_cube,
#'         period = "P16D",
#'         res = 160,
#'         roi = c(
#'             lon_min = -65.54870165,
#'             lat_min = -10.63479162,
#'             lon_max = -65.07629670,
#'             lat_max = -10.36046639
#'         ),
#'         multicores = 2,
#'         output_dir = reg_dir
#'     )
#'
#'     # Create the endmembers tibble
#'     em <- tibble::tribble(
#'         ~class, ~B02, ~B03, ~B04, ~B8A, ~B11, ~B12,
#'         "forest", 0.02, 0.0352, 0.0189, 0.28, 0.134, 0.0546,
#'         "land", 0.04, 0.065, 0.07, 0.36, 0.35, 0.18,
#'         "water", 0.07, 0.11, 0.14, 0.085, 0.004, 0.0026
#'     )
#'
#'     # Generate the mixture model
#'     mm <- sits_mixture_model(
#'         data = reg_cube,
#'         endmembers = em,
#'         memsize = 4,
#'         multicores = 2,
#'         output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_mixture_model <- function(data, endmembers, ...) {
    # set caller for error msg
    .check_set_caller("sits_mixture_model")
    # Pre-conditions
    .check_endmembers_parameter(endmembers)

    UseMethod("sits_mixture_model", data)
}
#' @rdname sits_mixture_model
#' @export
sits_mixture_model.sits <- function(data, endmembers, ...,
                                    rmse_band = TRUE,
                                    multicores = 2,
                                    progress = TRUE) {
    # Pre-conditions
    .check_samples_train(data)
    .check_lgl_parameter(rmse_band)
    .check_int_parameter(multicores, min = 1, max = 2048)
    progress <- .message_progress(progress)

    # Transform endmembers to tibble
    em <- .endmembers_as_tbl(endmembers)
    # Check endmember format
    .check_endmembers_tbl(em)
    # Get endmembers bands
    bands <- setdiff(.samples_bands(data), .endmembers_fracs(em))
    # The samples is filtered here in case some fraction
    # is added as a band
    data <- .samples_select_bands(samples = data, bands = bands)
    # Pre-condition
    .check_endmembers_bands(em = em, bands = .samples_bands(data))
    # Fractions to be produced
    out_fracs <- .endmembers_fracs(em = em, include_rmse = rmse_band)
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Create mixture processing function
    mixture_fn <- .mixture_fn_nnls(em = em, rmse = rmse_band)
    # Create groups of samples as jobs
    samples_groups <- .samples_split_groups(
        samples = data, multicores = multicores
    )
    # Process each group of samples in parallel
    samples_fracs <- .parallel_map(samples_groups, function(samples) {
        # Process the data
        output_samples <- .mixture_samples(
            samples = samples, em = em,
            mixture_fn = mixture_fn, out_fracs = out_fracs
        )
        return(output_samples)
    }, progress = progress)
    # Join groups samples as a sits tibble and return it
    ts <- .samples_merge_groups(samples_fracs)
    class(ts) <- c("sits", class(ts))
    return(ts)
}

#' @rdname sits_mixture_model
#' @export
sits_mixture_model.raster_cube <- function(data, endmembers, ...,
                                           rmse_band = TRUE,
                                           memsize = 4,
                                           multicores = 2,
                                           output_dir,
                                           progress = TRUE) {
    # Pre-conditions
    .check_is_raster_cube(data)
    .check_lgl_parameter(rmse_band)
    .check_int_parameter(memsize, min = 1, max = 16384)
    .check_output_dir(output_dir)
    .check_lgl_parameter(progress)
    # Transform endmembers to tibble
    em <- .endmembers_as_tbl(endmembers)
    # Check endmember format
    .check_endmembers_tbl(em)
    # Get endmembers bands
    bands <- setdiff(.cube_bands(data), .endmembers_fracs(em))
    # The cube is filtered here in case some fraction
    # is added as a band
    data <- .cube_filter_bands(cube = data, bands = bands)
    # Check if cube is regular
    .check_cube_is_regular(data)
    # Pre-condition
    .check_endmembers_bands(
        em = em,
        bands = .cube_bands(data, add_cloud = FALSE)
    )
    # Fractions to be produced
    out_fracs <- .endmembers_fracs(em = em, include_rmse = rmse_band)

    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0),
        npaths = length(bands) + length(out_fracs),
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
        image_size = .tile_size(.tile(data)), memsize = memsize,
        multicores = multicores
    )

    # Prepare parallelization
    .parallel_start(workers = multicores, output_dir = output_dir)
    on.exit(.parallel_stop(), add = TRUE)
    # Create mixture processing function
    mixture_fn <- .mixture_fn_nnls(em = em, rmse = rmse_band)
    # Create features as jobs
    features_cube <- .cube_split_features(data)
    # Process each feature in parallel
    features_fracs <- .jobs_map_parallel_dfr(features_cube, function(feature) {
        # Process the data
        output_feature <- .mixture_feature(
            feature = feature,
            block = block,
            em = em,
            mixture_fn = mixture_fn,
            out_fracs = out_fracs,
            output_dir = output_dir
        )
        return(output_feature)
    }, progress = progress)
    # Join output features as a cube and return it
    cube <- .cube_merge_tiles(dplyr::bind_rows(list(features_cube,
                                                    features_fracs))
    )
    # Join groups samples as a sits tibble and return it
    class(cube) <- c("raster_cube", class(cube))
    return(cube)
}
#' @rdname sits_mixture_model
#' @export
sits_mixture_model.derived_cube <- function(data, endmembers, ...) {
    stop(.conf("messages", "sits_mixture_model_derived_cube"))
}
#' @rdname sits_mixture_model
#' @export
sits_mixture_model.tbl_df <- function(data, endmembers, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data)))
        data <- .cube_find_class(data)
    else if (all(.conf("sits_tibble_cols") %in% colnames(data)))
        class(data) <- c("sits", class(data))
    else
        stop(.conf("messages", "sits_mixture_model_derived_cube"))
    data <- sits_mixture_model(data, endmembers, ...)
    return(data)
}
#' @rdname sits_mixture_model
#' @export
sits_mixture_model.default <- function(data, endmembers, ...) {
    data <- tibble::as_tibble(data)
    data <- sits_mixture_model(data, endmembers, ...)
    return(data)
}
