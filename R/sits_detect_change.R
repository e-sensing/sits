#' @title Detect changes in time series
#' @name sits_detect_change
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Given a set of time series or an image, this function compares
#' each time series with a set of change/no-change patterns, and indicates
#' places and dates where change has been detected.
#'
#' @param  data              Set of time series.
#' @param  dc_method         Detection change method (with parameters).
#' @param  ...               Other parameters for specific functions.
#' @param  roi               Region of interest (either an sf object, shapefile,
#'                           or a numeric vector with named XY values
#'                           ("xmin", "xmax", "ymin", "ymax") or
#'                           named lat/long values
#'                           ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param  filter_fn         Smoothing filter to be applied - optional
#'                           (closure containing object of class "function").
#' @param  impute_fn         Imputation function to remove NA.
#' @param  start_date        Start date for the classification
#'                           (Date in YYYY-MM-DD format).
#' @param  end_date          End date for the classification
#'                           (Date in YYYY-MM-DD format).
#' @param  memsize           Memory available for classification in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  output_dir        Valid directory for output file.
#'                           (character vector of length 1).
#' @param  version           Version of the output
#'                           (character vector of length 1).
#' @param  verbose           Logical: print information about processing time?
#' @param  progress          Logical: Show progress bar?
#' @return                   Time series with detection labels for
#'                           each point (tibble of class "sits")
#'                           or a data cube indicating detections in each pixel
#'                           (tibble of class "detections_cube").
#' @noRd
sits_detect_change <- function(data, dc_method, ...) {
    UseMethod("sits_detect_change", data)
}

#' @rdname sits_detect_change
#' @export
#' @noRd
sits_detect_change.sits <- function(data,
                                    dc_method,
                                    ...,
                                    filter_fn = NULL,
                                    multicores = 2L,
                                    progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_detect_change_sits")
    # preconditions
    .check_samples_ts(data)
    .check_is_sits_model(dc_method)
    .check_int_parameter(multicores, min = 1, max = 2048)
    progress <- .message_progress(progress)
    # preconditions - impute and filter functions
    if (!is.null(filter_fn)) {
        .check_function(filter_fn)
    }
    # Detect changes
    .detect_change_ts(
        samples = data,
        dc_method = dc_method,
        filter_fn = filter_fn,
        multicores = multicores,
        progress = progress
    )
}

#' @rdname sits_detect_change
#' @export
#' @noRd
sits_detect_change.raster_cube <- function(data,
                                           dc_method, ...,
                                           roi = NULL,
                                           filter_fn = NULL,
                                           start_date = NULL,
                                           end_date = NULL,
                                           impute_fn = identity,
                                           memsize = 8L,
                                           multicores = 2L,
                                           output_dir,
                                           version = "v1",
                                           verbose = FALSE,
                                           progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_detect_change_raster")
    # preconditions
    .check_is_raster_cube(data)
    .check_cube_is_regular(data)
    .check_int_parameter(memsize, min = 1, max = 16384)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_output_dir(output_dir)
    # preconditions - impute and filter functions
    .check_function(impute_fn)
    # Smoothing filter
    .check_filter_fn(filter_fn)
    # version is case-insensitive in sits
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)
    # Get default proc bloat
    proc_bloat <- .conf("processing_bloat_cpu")
    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        data <- .cube_filter_spatial(cube = data, roi = roi)
    }
    # Temporal filter
    start_date <- .default(start_date, .cube_start_date(data))
    end_date <- .default(end_date, .cube_end_date(data))
    data <- .cube_filter_interval(
        cube = data, start_date = start_date, end_date = end_date
    )

    # The following functions define optimal parameters for parallel processing
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    # '2' stands for forest and non-forest
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(data)) + 2,
        nbytes = 8,
        proc_bloat = proc_bloat
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
        image_size = .tile_size(.tile(data)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    on.exit(.parallel_stop(), add = TRUE)
    # Show block information
    start_time <- .classify_verbose_start(verbose, block)
    on.exit(.classify_verbose_end(verbose, start_time))
    # Process each tile sequentially
    .cube_foreach_tile(data, function(tile) {
        # Detect changes
        .detect_change_tile(
            tile = tile,
            band = "detection",
            dc_method = dc_method,
            block = block,
            roi = roi,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            output_dir = output_dir,
            version = version,
            verbose = verbose,
            progress = progress
        )
    })
}

#' @rdname sits_detect_change
#' @export
#' @noRd
sits_detect_change.default <- function(data, dc_method, ...) {
    stop(.conf("messages", "sits_detect_change_default"))
}
