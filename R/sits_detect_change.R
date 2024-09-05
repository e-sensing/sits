#' @title Detect changes in time series
#' @name sits_detect_change
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Given a set of time series or an image, this function compares
#' each time series with a set of change/no-change patterns, and indicates
#' places and dates where change has been detected.
#'
#' @param  data              Set of time series.
#' @param  cd_method         Change detection method (with parameters).
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
sits_detect_change <- function(data,
                               cd_method,
                               ...,
                               filter_fn = NULL,
                               multicores = 2L,
                               progress = TRUE) {
    UseMethod("sits_detect_change", data)
}

#' @rdname sits_detect_change
sits_detect_change.sits <- function(data,
                                    cd_method,
                                    ...,
                                    filter_fn = NULL,
                                    multicores = 2L,
                                    progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_detect_change_sits")
    # Pre-conditions
    data <- .check_samples_ts(data)
    .check_is_sits_model(cd_method)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_progress(progress)
    # Detect changes
    .detect_change_ts(
        samples = data,
        cd_method = cd_method,
        filter_fn = filter_fn,
        multicores = multicores,
        progress = progress
    )
}

#' @rdname sits_detect_change
sits_detect_change.raster_cube <- function(data,
                                           cd_method, ...,
                                           roi = NULL,
                                           filter_fn = NULL,
                                           impute_fn = identity,
                                           start_date = NULL,
                                           end_date = NULL,
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
    .check_that(.cube_is_regular(data))
    .check_is_sits_model(cd_method)
    .check_int_parameter(memsize, min = 1, max = 16384)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_output_dir(output_dir)
    # version is case-insensitive in sits
    version <- .check_version(version)
    .check_progress(progress)
    # Get default proc bloat
    proc_bloat <- .conf("processing_bloat_cpu")
    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        data <- .cube_filter_spatial(cube = data, roi = roi)
    }
    # Temporal filter
    if (.has(start_date) || .has(end_date)) {
        data <- .cube_filter_interval(
            cube = data, start_date = start_date, end_date = end_date
        )
    }
    if (.has(filter_fn))
        .check_filter_fn(filter_fn)
    # Retrieve the samples from the model
    samples <- .ml_samples(cd_method)
    # Do the samples and tile match their timeline length?
    .check_samples_tile_match_timeline(samples = samples, tile = data)
    # Do the samples and tile match their bands?
    .check_samples_tile_match_bands(samples = samples, tile = data)
    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(data)) + length(.ml_labels(cd_method)),
        nbytes = 8,
        proc_bloat = proc_bloat
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(data)),
        memsize = memsize,
        multicores = multicores
    )
    # Terra requires at least two pixels to recognize an extent as valid
    # polygon and not a line or point
    block <- .block_regulate_size(block)
    # Prepare parallel processing
    .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    on.exit(.parallel_stop(), add = TRUE)
    # Show block information
    start_time <- .classify_verbose_start(verbose, block)
    # Process each tile sequentially
    detections_cube <- .cube_foreach_tile(data, function(tile) {
        # Detect changes
        detections_tile <- .detect_change_tile(
            tile = tile,
            band = "detection",
            cd_method = cd_method,
            block = block,
            roi = roi,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            output_dir = output_dir,
            version = version,
            verbose = verbose,
            progress = progress
        )
        return(detections_tile)
    })
    # Show block information
    .classify_verbose_end(verbose, start_time)
    return(detections_cube)
}

#' @rdname sits_detect_change
sits_detect_change.default <- function(data, cd_method, ...) {
    stop("Input should be a sits tibble or a data cube")
}
