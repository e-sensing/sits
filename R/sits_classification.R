#' @title Classify time series or data cubes
#'
#' @name sits_classify
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' This function classifies a set of time series or data cube given
#' a trained model prediction model created by \code{\link[sits]{sits_train}}.
#'
#' SITS supports the following models:
#' \itemize{
#'  \item{support vector machines: } {see \code{\link[sits]{sits_svm}}}
#'  \item{random forests: }          {see \code{\link[sits]{sits_rfor}}}
#'  \item{extreme gradient boosting: } {see \code{\link[sits]{sits_xgboost}}}
#'  \item{multi-layer perceptrons: } {see \code{\link[sits]{sits_mlp}}}
#'  \item{1D CNN: } {see \code{\link[sits]{sits_tempcnn}}}
#'  \item{deep residual networks:}{see \code{\link[sits]{sits_resnet}}}
#'  \item{self-attention encoders:}{see \code{\link[sits]{sits_lighttae}}}
#'  }
#'
#'
#' @param  data              Data cube.
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}.
#' @param  ...               Other parameters for specific functions.
#' @param  roi               Region of interest (see below)
#' @param  filter_fn         Smoothing filter to be applied (if desired).
#' @param  impute_fn         Impute function to replace NA.
#' @param  start_date        Start date for the classification.
#' @param  end_date          End date for the classification.
#' @param  memsize           Memory available for classification (in GB).
#' @param  multicores        Number of cores to be used for classification.
#' @param  output_dir        Directory for output file.
#' @param  version           Version of the output (for multiple
#'                           classifications).
#' @param  verbose           Print information about processing time?
#' @param  progress          Show progress bar?
#'
#' @return                   Predicted data (classified time series)
#'                           or a data cube with probabilities for each class.
#'
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lon_min", "lat_min", "lon_max", "lat_max")
#'
#'    The "filter_fn" parameter specifies a smoothing filter to be applied to
#'    time series for reducing noise. Currently, options include
#'    Savitzky-Golay (see \code{\link[sits]{sits_sgolay}}) and Whittaker
#'    (see \code{\link[sits]{sits_whittaker}}).
#'
#'    The "impute_fn" function is used to remove invalid or cloudy pixels
#'    from time series. The default is a linear interpolator, available
#'    in \code{\link[sits]{sits_impute_linear}}. Users can add their custom
#'    functions.
#'
#'    The "memsize" and "multicores" parameters are used for multiprocessing.
#'    The "multicores" parameter defines the number of cores used for
#'    processing. The "memsize" parameter  controls the amount of memory
#'    available for classification. We recommend using a 4:1 relation between
#'    "memsize" and "multicores".
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of classification of a time series
#'     # Retrieve the samples for Mato Grosso
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#'
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = c("NDVI"))
#'     point_class <- sits_classify(point_ndvi, rf_model)
#'     plot(point_class)
#'
#'     # Example of classification of a data cube
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rf_model)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(probs_cube)
#'     # plot the classified image
#'     plot(label_cube)
#' }
#'
#' @export
sits_classify <- function(data, ml_model, ...,
                          filter_fn = NULL,
                          multicores = 2,
                          progress = TRUE) {

    # Pre-conditions
    data <- .conf_data_meta_type(data)
    .check_is_sits_model(ml_model)
    .check_multicores(multicores)
    .check_progress(progress)

    UseMethod("sits_classify", data)
}
#' @rdname sits_classify
#' @export
sits_classify.sits <- function(data,
                               ml_model,
                               ...,
                               filter_fn = NULL,
                               multicores = 2,
                               progress = TRUE) {

    # Pre-conditions
    .check_samples(data)

    # Update multicores: xgb model do its own parallelization
    if (inherits(ml_model, "xgb_model")) {
        multicores <- 1
    }

    # Do classification
    classified_ts <- .sits_classify_ts(
        samples = data,
        ml_model = ml_model,
        filter_fn = filter_fn,
        multicores = multicores,
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
                                      impute_fn = sits_impute_linear(),
                                      start_date = NULL,
                                      end_date = NULL,
                                      memsize = 8,
                                      multicores = 2,
                                      output_dir = getwd(),
                                      version = "v1",
                                      verbose = TRUE,
                                      progress = TRUE) {
    # preconditions
    .check_is_raster_cube(data)
    .check_is_regular(data)
    .check_memsize(memsize)
    .check_output_dir(output_dir)
    .check_version(version)

    # Spatial filter
    if (.has(roi)) {
        data <- .cube_filter_spatial(cube = data, roi = .roi_as_sf(roi))
    }
    # Temporal filter
    if (.has(start_date) || .has(end_date)) {
        data <- .cube_filter_interval(
            cube = data, start_date = start_date, end_date = end_date
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
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Update multicores parameter
    if ("xgb_model" %in% .ml_class(ml_model)) {
        multicores <- 1
    }
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize, block = block,
        image_size = .tile_size(.tile(data)), memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .sits_parallel_start(
        workers = multicores, log = verbose, output_dir = output_dir
    )
    on.exit(.sits_parallel_stop(), add = TRUE)

    # # Callback final tile classification
    # .callback(process = "cube_classification", event = "started",
    #           context = environment())
    # Show block information
    if (verbose) {
        start_time <- Sys.time()
        message("Using blocks of size (", .nrows(block),
                " x ", .ncols(block), ")")
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
            impute_fn = impute_fn,
            output_dir = output_dir,
            version = version,
            verbose = verbose,
            progress = progress
        )
        return(probs_tile)
    })
    # # Callback final tile classification
    # .callback(event = "cube_classification", status = "end",
    #           context = environment())
    # Show block information
    if (verbose) {
        end_time <- Sys.time()
        message("")
        message("Classification finished at ", end_time)
        message("Elapsed time of ",
                format(round(end_time - start_time, digits = 2)))
    }
    return(probs_cube)
}
