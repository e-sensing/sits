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
#'     # select the NDVI band
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # train a random forest model
#'     rf_model <- sits_train(samples_ndvi, ml_method = sits_rfor)
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
sits_classify <- function(data, ml_model, ...) {

    # set caller to show in errors
    .check_set_caller("sits_classify")

    # check data type
    data <- .config_data_meta_type(data)

    # dispatch
    UseMethod("sits_classify", data)
}
#' @rdname sits_classify
#'
#' @export
sits_classify.sits <- function(data,
                               ml_model,
                               ...,
                               filter_fn = NULL,
                               multicores = 2) {

    # precondition: verify that the data is correct
    .sits_tibble_test(data)

    # precondition: ensure the machine learning model has been built
    .check_null(
        x = ml_model,
        msg = "please provide a trained ML model"
    )

    # Apply filter
    if (!purrr::is_null(filter_fn)) {
        data <- .apply_across(data = data, fn = filter_fn)
    }

    # precondition - are the samples valid?
    samples <- .sits_ml_model_samples(ml_model)
    .check_that(
        x = nrow(samples) > 0,
        msg = "missing original samples"
    )
    # check band order is the same
    bands_samples <- sits_bands(samples)
    bands_data <- sits_bands(data)
    if (!all(bands_samples == bands_data))
        data <- sits_select(data, sits_bands(samples))

    # get normalization params
    stats <- environment(ml_model)$stats
    # has the training data been normalized?
    if (!purrr::is_null(stats)) {
        # yes, then normalize the input data
        distances <- .sits_distances(.sits_ml_normalize_data(
            data = data,
            stats = stats
        ))
    } else {
        # no, input data does not need to be normalized
        distances <- .sits_distances(data)
    }


    # post condition: is distance data valid?
    .check_that(
        x = nrow(distances) > 0,
        msg = "problem with normalization"
    )

    # calculate the breaks in the time for multi-year classification
    class_info <- .sits_timeline_class_info(
        data = data,
        samples = samples
    )

    # retrieve the the predicted results
    prediction <- .sits_distances_classify(
        distances = distances,
        class_info = class_info,
        ml_model = ml_model,
        multicores = multicores
    )

    # Store the result in the input data
    data_pred <- .sits_tibble_prediction(
        data = data,
        class_info = class_info,
        prediction = prediction
    )
    class(data_pred) <- c("predicted", class(data))
    return(data_pred)
}
#' @rdname sits_classify
#'
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
                                      verbose = FALSE,
                                      progress = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_classify.raster_cube")
    # precondition - test if cube is regular
    .check_that(
        x = .cube_is_regular(data),
        local_msg = "Please use sits_regularize()",
        msg = "sits can only classify regular cubes"
    )
    # precondition - checks if the cube and ml_model are valid
    .sits_classify_check_params(data, ml_model)
    # precondition - roi
    if (!is.null(roi)) {
        roi <- .sits_parse_roi_cube(roi)
    }
    # update multicores parameter
    if ("xgb_model" %in% class(ml_model)) {
        multicores <- 1
    }
    # precondition - multicores
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )
    # precondition - memory
    .check_num(
        x = memsize,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'memsize' parameter"
    )
    # precondition - output dir
    .check_file(
        x = output_dir,
        msg = "invalid 'output_dir' parameter"
    )
    # precondition - version
    .check_chr(
        x = version,
        len_min = 1,
        msg = "invalid 'version' parameter"
    )
    # check documentation mode
    progress <- .check_documentation(progress)

    # spatial filter
    if (!is.null(roi)) {
        data <- .cube_spatial_filter(
            cube = data,
            roi = .roi_as_sf(roi)
        )
    }
    # temporal filter
    if (!is.null(start_date) || !is.null(end_date)) {
        data <- .cube_temporal_filter(
            cube = data,
            start_date = start_date,
            end_date = end_date
        )
    }
    # Get block size
    block_size <- .raster_file_blocksize(
        r_obj = .raster_open_rast(.file_info_path(data[1,]))
    )
    # Check minimum memory needed to process one block
    multicores <- .jobs_max_multicores(
        job_nrows = block_size[["block_nrows"]],
        job_ncols = block_size[["block_ncols"]],
        npaths = length(.file_info_paths(data[1,])),
        nbytes = 8,
        proc_bloat = .config_processing_bloat(),
        memsize = memsize,
        multicores = multicores,
        overlap = 0
    )
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = verbose)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Classification
    # Process each tile sequentially
    tiles_blocks <- slider::slide(data, function(tile) {
        # Get timeline
        timeline <- sits_timeline(tile)
        # Retrieve the samples from the model
        samples <- .sits_ml_model_samples(ml_model)
        # create the metadata for the probability cube

        # output filename
        out_file <- .file_path(
            tile[["satellite"]],
            tile[["sensor"]],
            tile[["tile"]],
            start_date,
            end_date,
            band_name,
            version,
            ext = "tif",
            output_dir = output_dir
        )


        probs_cube <- .cube_derived_create(
            cube       = tile,
            cube_class = "probs_cube",
            band_name  = "probs",
            labels     = sits_labels(samples),
            start_date = timeline[[1]],
            end_date   = timeline[[length(timeline)]],
            bbox       = .sits_raster_sub_image_default(tile),
            output_dir = output_dir,
            version    = version
        )
        probs_tile <- .tile_create_probs(
            tile = tile,
            labels = sits_labels(samples),
            output_dir = output_dir,
            version    = version
        )

        # check timelines of samples and tile
        .check_that(
            length(sits_timeline(samples)) == length(sits_timeline(tile)),
            msg = "number of instances of samples and cube differ"
        )
        # Compute how many jobs to process
        jobs <- .jobs_create(
            block_nrows = block_size[["block_nrows"]],
            block_ncols = block_size[["block_ncols"]],
            block_overlap = 0,
            ncols = .file_info_ncols(tile),
            nrows = .file_info_nrows(tile),
            xmin = tile[["xmin"]],
            xmax = tile[["xmax"]],
            ymin = tile[["ymin"]],
            ymax = tile[["ymax"]],
            crs = tile[["crs"]],
            roi = roi
        )
        # If no jobs to process return NULL
        if (nrow(jobs) == 0) {
            return(NULL)
        }
        # classify the data
        blocks_files <- .sits_classify_blocks_multicores(
            tile = tile,
            ml_model = ml_model,
            roi = roi,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            memsize = memsize,
            multicores = multicores,
            output_dir = output_dir,
            progress = progress
        )

        # Merge
        # Create a template raster based on the first image of the tile
        .raster_template(
            file = .file_info_path(tile),
            out_file = out_file,
            data_type = probs_cube_dt,
            nlayers = length(samples_labels),
            missing_value = .config_get("probs_cube_missing_value")
        )
        # Merge blocks
        .raster_merge(
            in_files = blocks_files,
            out_file = out_file,
            format = "GTiff",
            gdal_datatype = .raster_gdal_datatype(probs_cube_dt),
            multicores = original_multicores,
            overwrite = 0
        )
        return(blocks_files)
    })

    return(probs_cube)
}
