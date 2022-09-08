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
    .check_samples(data)
    # precondition - are the samples form the model valid?
    samples <- .sits_ml_model_samples(ml_model)
    .check_samples(samples)

    # Apply filter
    if (!purrr::is_null(filter_fn)) {
        data <- .apply_across(data = data, fn = filter_fn)
    }
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
    .check_distances(distances, data)

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

    # precondition - test if cube is regular
    .check_is_regular(data)
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - memsize
    .check_memsize(memsize)
    # precondition - output dir
    .check_output_dir(output_dir)
    # precondition - version
    .check_version(version)

    # Update multicores parameter
    if ("xgb_model" %in% class(ml_model)) {
        multicores <- 1
    }
    # Spatial filter
    if (!is.null(roi)) {
        data <- .cube_spatial_filter(
            cube = data,
            roi = .roi_as_sf(roi)
        )
        # precondition - checks if data is empty
        .check_that(
            x = nrow(data) > 0,
            local_msg = "informed ROI does not interesect cube",
            msg = "invalid 'roi' parameter"
        )
    }
    # Temporal filter
    if (!is.null(start_date) || !is.null(end_date)) {
        data <- .cube_temporal_filter(
            cube = data,
            start_date = start_date,
            end_date = end_date
        )
        # precondition - checks if data is empty
        .check_that(
            x = nrow(data) > 0,
            local_msg = "informed interval does not interesect cube",
            msg = "invalid 'start_date' and 'end_date' parameters"
        )
    }
    # Retrieve the samples from the model
    samples <- .sits_ml_model_samples(ml_model)
    # precondition - are the samples valid?
    .check_samples(samples)
    # Check timelines of samples and tile
    .check_that(
        length(sits_timeline(samples)) == length(sits_timeline(data)),
        msg = "number of instances of samples and cube differ"
    )
    # Get job size
    job_size <- .raster_file_blocksize(
        .raster_open_rast(.file_info_path(data[1,]))
    )
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = job_size, npaths = length(.file_info_paths(data[1,])) +
            length(.ml_model_labels(ml_model)), nbytes = 8,
        proc_bloat = .config_processing_bloat(), overlap = 0
    )
    # Check if memsize is above minimum needed to process one block
    .check_that(
        x = job_memsize < memsize,
        local_msg = paste("minimum memsize needed is", job_memsize, "GB"),
        msg = "provided 'memsize' is insufficient for processing"
    )
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = verbose)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # # Callback final tile classification
    # .callback(process = "cube_classification", event = "started",
    #           context = environment())
    # Show block information
    if (verbose) {
        start_time <- Sys.time()
        message("Using blocks of size (", block_size[["nrows"]],
                " x ", block_size[["ncols"]], ")")
    }
    # Classification
    # Process each tile sequentially
    probs_cube <- .cube_foreach_tile(data, function(tile) {
        # Output file
        out_file <- .file_probs_name(tile = tile, version = version,
                                     output_dir = output_dir)
        # Resume feature
        if (file.exists(out_file)) {
            # # Callback final tile classification
            # .callback(process = "tile_classification", event = "recovery",
            #           context = environment())
            message("Recovery: Tile '", tile[["tile"]], "' already exists.")
            message("(If you want to produce a new image, please ",
                    "change 'output_dir' or 'version' parameters)")
            probs_tile <- .tile_probs_from_file(
                file = out_file,
                band = "probs",
                base_tile = tile,
                labels = .ml_model_labels(ml_model)
            )
            return(probs_tile)
        }
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "started",
        #           context = environment())
        # Show initial time for tile classification
        if (verbose) {
            tile_start_time <- Sys.time()
            message("Starting classification of tile '",
                    tile[["tile"]], "' at ", start_time)
        }
        # Create jobs
        # Compute how many jobs to process
        jobs <- .jobs_create(
            job_size = job_size, block_overlap = 0,
            ncols = .tile_ncols(tile), nrows = .tile_nrows(tile),
            xmin = .tile_xmin(tile), xmax = .tile_xmax(tile),
            ymin = .tile_ymin(tile), ymax = .tile_ymax(tile),
            crs = .tile_crs(tile), roi = roi
        )
        # Process jobs in parallel
        block_files <- .jobs_parallel_chr(jobs, function(job) {
            # Get job block
            block <- .jobs_as_block(job)
            # Get file_info to read files
            fi <- .fi(tile)
            #
            # Log here
            #
            .sits_debug_log(
                output_dir = output_dir,
                event = "start_block_data_read_preprocess",
                key = "block",
                value = block
            )
            # Get model bands
            bands <- .ml_model_bands(ml_model)
            # Read and preprocess values from rasters
            if (.band_cloud() %in% bands) {


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "start_block_data_read_cloud",
                    key = "block",
                    value = block
                )


                # Get cloud values
                cloud_mask <- .fi_read_block(fi = fi, band = , block = block)


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "end_block_data_read_cloud"
                )


                # Get cloud parameters
                interp_values <- .tile_cloud_interp_values(tile)
                is_bit_mask <- .tile_cloud_bit_mask(tile)


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "start_block_data_process_cloud",
                    key = "bitmask",
                    value = is_bit_mask
                )


                # Prepare cloud_mask
                # Identify values to be removed
                cloud_mask <- if (!is_bit_mask) cloud_mask %in% interp_values
                else matrix(bitwAnd(cloud_mask, sum(2^interp_values)) > 0,
                            nrow = length(cloud_mask))


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "end_block_data_process_cloud"
                )


            }
            # For each band (except cloud) get values from files
            values <- purrr::map(.bands(bands, cloud = FALSE), function(band) {


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "start_block_data_read_band",
                    key = "band",
                    value = band
                )


                # Get band values
                values <- .fi_read_block(fi = fi, band = band, block = block)


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "end_block_data_read_band"
                )


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "start_block_data_process_band",
                    key = "band",
                    value = band
                )


                # Remove cloud masked pixels
                if (.band_cloud() %in% bands) {
                    values[cloud_mask] <- NA
                }
                # Correct missing, minimum, and maximum values; and scale values
                # Get band defaults for derived cube from config
                conf_band <- .conf_eo_band(
                    source = .tile_source(tile),
                    collection = .tile_collection(tile),
                    band = band
                )
                # This can be ignored as it is already process by gdal
                # miss_value = .band_miss_value(conf_band)
                # if (!is.null(miss_value)) {
                #     values[values == miss_value] <- NA
                # }
                min_value <- .band_min_value(conf_band)
                if (!is.null(min_value)) {
                    values[values < min_value] <- NA
                }
                max_value <- .band_max_value(conf_band)
                if (!is.null(max_value)) {
                    values[values > max_value] <- NA
                }
                # Remove NA pixels
                if (!is.null(impute_fn)) {
                    values <- impute_fn(values)
                }
                offset <- .band_offset(conf_band)
                if (!is.null(offset)) {
                    values <- values - offset
                }
                scale <- .band_scale(conf_band)
                if (!is.null(scale)) {
                    values <- values * scale
                }
                # Filter the time series
                if (!(is.null(filter_fn))) {
                    values <- filter_fn(values)
                }
                # Normalize values
                stats <- .ml_model_stats(ml_model)
                if (!is.null(stats$quant_2[[band]]) &&
                    !is.null(stats$quant_98[[band]])) {
                    values <- normalize_data(values, stats$quant_2[[band]],
                                             stats$quant_98[[band]])
                }


                #
                # Log here
                #
                .sits_debug_log(
                    output_dir = output_dir,
                    event = "end_block_data_process_band"
                )


                values
            })
            # Compose final data
            values <- do.call(cbind, values)
            colnames(values) <- .ml_model_attr_names(ml_model)
            # Apply the classification model to values


            #
            # Log here
            #
            .sits_debug_log(
                output_dir = output_dir,
                event = "start_block_data_classification",
                key = "model",
                value = class(ml_model)[[1]]
            )


            probs <- ml_model(values)


            #
            # Log here
            #
            .sits_debug_log(
                output_dir = output_dir,
                event = "end_block_data_classification"
            )

            # Prepare probability to be saved
            probs <- probs * round(1 / .conf_probs_band_scale())
            # Output file name
            block_file <- .file_block_name(
                pattern = .file_pattern(out_file),
                block = block,
                output_dir = output_dir
            )


            #
            # Log here
            #
            .sits_debug_log(
                output_dir = output_dir,
                event = "start_block_data_save",
                key = "file",
                value = block_file
            )


            # Prepare and save results as raster
            .raster_write_block(
                file = block_file,
                block = block,
                bbox = .bbox(job),
                values = probs,
                data_type = .conf_probs_band_data_type()
            )

            #
            # Log here
            #
            .sits_debug_log(
                output_dir = output_dir,
                event = "end_block_data_classification"
            )

            block_file
        })
        # # Callback final tile classification
        # .callback(event = "merge_blocks", status = "begin",
        #           context = environment())
        # Merge blocks into a new probs_cube tile
        probs_tile <- .tile_probs_merge_blocks(
            file = out_file, band = "probs",
            labels = .ml_model_labels(ml_model),
            base_tile = tile, block_files = block_files,
            multicores = multicores
        )
        # # Callback final tile classification
        # .callback(event = "tile_classification", status = "end",
        #           context = environment())
        # show final time for classification
        if (verbose) {
            tile_end_time <- Sys.time()
            message("Tile '", tile[["tile"]], "' finished at ", tile_end_time)
            message("Elapsed time of ",
                    format(round(tile_end_time - tile_start_time, digits = 2)))
            message("")
        }
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
