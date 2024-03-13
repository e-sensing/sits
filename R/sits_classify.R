#' @title Classify time series or data cubes
#' @name sits_classify
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' This function classifies a set of time series or data cube given
#' a trained model prediction model created by \code{\link[sits]{sits_train}}.
#'
#' SITS supports the following models:
#' (a) support vector machines:  \code{\link[sits]{sits_svm}};
#' (b) random forests:  \code{\link[sits]{sits_rfor}};
#' (c) extreme gradient boosting: \code{\link[sits]{sits_xgboost}};
#' (d) multi-layer perceptrons: \code{\link[sits]{sits_mlp}};
#' (e) 1D CNN: \code{\link[sits]{sits_tempcnn}};
#' (f) deep residual networks: \code{\link[sits]{sits_resnet}};
#' (g) self-attention encoders: \code{\link[sits]{sits_lighttae}}.
#'
#' @param  data              Data cube (tibble of class "raster_cube")
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}
#'                           (closure of class "sits_model")
#' @param  ...               Other parameters for specific functions.
#' @param  roi               Region of interest (either an sf object, shapefile,
#'                           or a numeric vector with named XY values
#'                           ("xmin", "xmax", "ymin", "ymax") or
#'                           named lat/long values
#'                           ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param  filter_fn         Smoothing filter to be applied - optional
#'                           (clousure containing object of class "function").
#' @param  start_date        Start date for the classification
#'                           (Date in YYYY-MM-DD format).
#' @param  end_date          End date for the classification
#'                           (Date im YYYY-MM-DD format).
#' @param  memsize           Memory available for classification in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  gpu_memory        Memory available in GPU in GB (default = 16)
#' @param  n_sam_pol         Number of time series per segment to be classified
#'                           (integer, min = 10, max = 50).
#' @param  output_dir        Valid directory for output file.
#'                           (character vector of length 1).
#' @param  version           Version of the output
#'                           (character vector of length 1).
#' @param  verbose           Logical: print information about processing time?
#' @param  progress          Logical: Show progress bar?
#'
#' @return                   Time series with predicted labels for
#'                           each point (tibble of class "sits")
#'                           or a data cube with probabilities for each class
#'                           (tibble of class "probs_cube").
#'
#' @note
#'    The \code{roi} parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values (\code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}) or
#'    named lat/long values (\code{lon_min}, \code{lon_max},
#'    \code{lat_min}, \code{lat_max})
#'
#'    Parameter \code{filter_fn} parameter specifies a smoothing filter
#'    to be applied to each time series for reducing noise. Currently, options
#'    are Savitzky-Golay (see \code{\link[sits]{sits_sgolay}}) and Whittaker
#'    (see \code{\link[sits]{sits_whittaker}}) filters.
#'
#'    Parameter \code{memsize} controls the amount of memory available
#'    for classification, while \code{multicores}  defines the number of cores
#'    used for processing. We recommend using as much memory as possible.
#'
#'    When using a GPU for deep learning, \code{gpu_memory} indicates the
#'    memory of available in the graphics card.
#'
#'    For classifying vector data cubes created by
#'    \code{\link[sits]{sits_segment}},
#'    \code{n_sam_pol} controls is the number of time series to be
#'    classified per segment.
#'
#'    Please refer to the sits documentation available in
#'    <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of classification of a time series
#'     # Retrieve the samples for Mato Grosso
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#'
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = c("NDVI"))
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = rf_model
#'     )
#'     plot(point_class)
#'
#'     # Example of classification of a data cube
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir(),
#'         version = "ex_classify"
#'     )
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir(),
#'         version = "ex_classify"
#'     )
#'     # plot the classified image
#'     plot(label_cube)
#'     # segmentation
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         seg_fn = sits_slic(step = 5,
#'                        compactness = 1,
#'                        dist_fun = "euclidean",
#'                        avg_fun = "median",
#'                        iter = 50,
#'                        minarea = 10,
#'                        verbose = FALSE
#'                        ),
#'         output_dir = tempdir()
#'     )
#'     # Create a classified vector cube
#'     probs_segs <- sits_classify(
#'         data = segments,
#'         ml_model = rf_model,
#'         output_dir = tempdir(),
#'         multicores = 4,
#'         version = "segs"
#'     )
#'     # Create a labelled vector cube
#'     class_segs <- sits_label_classification(
#'         cube = probs_segs,
#'         output_dir = tempdir(),
#'         multicores = 2,
#'         memsize = 4,
#'         version = "segs_classify"
#'     )
#'     # plot class_segs
#'     plot(class_segs)
#' }
#'
#' @export
sits_classify <- function(data, ml_model, ...,
                          filter_fn = NULL,
                          multicores = 2L,
                          progress = TRUE) {

    UseMethod("sits_classify", data)
}
#' @rdname sits_classify
#' @export
sits_classify.sits <- function(data,
                               ml_model,
                               ...,
                               filter_fn = NULL,
                               multicores = 2L,
                               gpu_memory = 16,
                               progress = TRUE) {
    # Pre-conditions
    data <- .check_samples_ts(data)
    .check_is_sits_model(ml_model)
    .check_multicores(multicores, min = 1, max = 2048)
    .check_progress(progress)
    # Update multicores: xgb model does its own parallelization
    if (inherits(ml_model, "xgb_model")) {
        multicores <- 1
    }
    # Do classification
    classified_ts <- .classify_ts(
        samples = data,
        ml_model = ml_model,
        filter_fn = filter_fn,
        multicores = multicores,
        gpu_memory = gpu_memory,
        progress = progress
    )
    return(classified_ts)
}
#' @rdname sits_classify
#' @export
sits_classify.raster_cube <- function(data,
                                      ml_model, ...,
                                      roi = NULL,
                                      filter_fn = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      memsize = 8L,
                                      multicores = 2L,
                                      gpu_memory = 16,
                                      output_dir,
                                      version = "v1",
                                      verbose = FALSE,
                                      progress = TRUE) {
    # preconditions
    .check_is_raster_cube(data)
    .check_is_regular(data)
    .check_is_sits_model(ml_model)
    .check_memsize(memsize, min = 1, max = 16384)
    .check_multicores(multicores, min = 1, max = 2048)
    .check_output_dir(output_dir)
    version <- .check_version(version)
    .check_progress(progress)
    # If we using the GPU, gpu_memory parameter needs to be specified
    if ("torch_model" %in% class(ml_model) && torch::cuda_is_available()) {
        .check_int_parameter(gpu_memory, min = 1, max = 16384,
                             msg = "Using GPU: gpu_memory must be informed")

        proc_bloat <- .conf("processing_bloat_gpu")
    } else
        proc_bloat <- .conf("processing_bloat_cpu")

    # version is case-insensitive in sits
    version <- tolower(version)

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
    if (!purrr::is_null(filter_fn)) {
        .check_that(is.function(filter_fn),
            local_msg = "Please use sits_whittaker() or sits_sgolay()",
            msg = "Invalid filter_fn parameter"
        )
    }
    # Retrieve the samples from the model
    samples <- .ml_samples(ml_model)
    # Do the samples and tile match their timeline length?
    .check_samples_tile_match(samples = samples, tile = data)
    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_paths(data)) + length(.ml_labels(ml_model)),
        nbytes = 8,
        proc_bloat = proc_bloat
    )
    # If we using the GPU, gpu_memory parameter needs to be specified
    if ("torch_model" %in% class(ml_model) && torch::cuda_is_available()) {
        .check_int_parameter(gpu_memory, min = 1, max = 16384,
                             msg = "Using GPU: gpu_memory must be informed")

        memsize <-  gpu_memory
        multicores  <- 1
    } else {
        # Update multicores parameter
        multicores <- .jobs_max_multicores(
            job_memsize = job_memsize, memsize = memsize, multicores = multicores
        )
    }
    # Update multicores parameter
    if ("xgb_model" %in% .ml_class(ml_model)) {
        multicores <- 1
    }
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(data)), memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    on.exit(.parallel_stop(), add = TRUE)
    # Show block information
    if (verbose) {
        start_time <- Sys.time()
        message(
            "Using blocks of size (", .nrows(block),
            " x ", .ncols(block), ")"
        )
    }
    # Classification
    # Process each tile sequentially
    probs_cube <- .cube_foreach_tile(data, function(tile) {
        # Classify the data
        probs_tile <- .classify_tile(
            tile = tile,
            band = "probs",
            ml_model = ml_model,
            block = block,
            roi = roi,
            filter_fn = filter_fn,
            output_dir = output_dir,
            version = version,
            verbose = verbose,
            progress = progress
        )
        return(probs_tile)
    })
    # Show block information
    if (verbose) {
        end_time <- Sys.time()
        message("")
        message("Classification finished at ", end_time)
        message(
            "Elapsed time of ",
            format(round(end_time - start_time, digits = 2))
        )
    }
    return(probs_cube)
}
#' @rdname sits_classify
#' @export
sits_classify.segs_cube <- function(data,
                                    ml_model, ...,
                                    filter_fn = NULL,
                                    start_date = NULL,
                                    end_date = NULL,
                                    memsize = 8L,
                                    multicores = 2L,
                                    gpu_memory = 16,
                                    output_dir,
                                    version = "v1",
                                    n_sam_pol = NULL,
                                    verbose = FALSE,
                                    progress = TRUE) {

    # preconditions
    .check_is_vector_cube(data)
    .check_is_sits_model(ml_model)
    .check_int_parameter(n_sam_pol, min = 5, allow_null = TRUE)
    .check_memsize(memsize, min = 1, max = 16384)
    .check_multicores(multicores, min = 1, max = 2048)
    .check_output_dir(output_dir)
    version <- .check_version(version)
    .check_progress(progress)
    # version is case-insensitive in sits
    version <- tolower(version)
    # If we using the GPU, gpu_memory parameter needs to be specified
    if ("torch_model" %in% class(ml_model) && torch::cuda_is_available()) {
        .check_int_parameter(gpu_memory, min = 1, max = 16384,
                             msg = "Using GPU: gpu_memory must be informed")
    }
    # Temporal filter
    if (.has(start_date) || .has(end_date)) {
        data <- .cube_filter_interval(
            cube = data, start_date = start_date, end_date = end_date
        )
    }
    if (!purrr::is_null(filter_fn)) {
        .check_that(is.function(filter_fn),
                    local_msg = "Please use sits_whittaker() or sits_sgolay()",
                    msg = "Invalid filter_fn parameter"
        )
    }
    # Retrieve the samples from the model
    samples <- .ml_samples(ml_model)
    # Do the samples and tile match their timeline length?
    .check_samples_tile_match(samples = samples, tile = data)
    # Update multicores parameter
    if ("xgb_model" %in% .ml_class(ml_model)) {
        multicores <- 1
    }
    # Prepare parallel processing
    .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    on.exit(.parallel_stop(), add = TRUE)
    # Classification
    # Process each tile sequentially
    probs_vector_cube <- .cube_foreach_tile(data, function(tile) {
        # Classify all the segments for each tile
        class_vector <- .classify_vector_tile(
            tile = tile,
            ml_model = ml_model,
            filter_fn = filter_fn,
            n_sam_pol = n_sam_pol,
            multicores = multicores,
            memsize = memsize,
            gpu_memory = gpu_memory,
            version = version,
            output_dir = output_dir,
            progress = progress
        )
    })
    return(probs_vector_cube)
}
#' @rdname sits_classify
#' @export
sits_classify.default <- function(data, ml_model, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else
        stop("Input should be a sits tibble or a data cube")
    result <- sits_classify(data, ml_model, ...)
    return(result)
}
