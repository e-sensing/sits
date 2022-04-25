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
#' # Example of classification of a time series
#' # Retrieve the samples for Mato Grosso
#' # select an extreme gradient boosting model
#' samples_2bands <- sits_select(samples_modis_4bands,
#'   bands = c("EVI", "NDVI")
#' )
#' rf_model <- sits_train(samples_2bands,
#'   ml_method = sits_rfor(num_trees = 50)
#' )
#' # classify the point
#' point_2bands <- sits_select(point_mt_6bands,
#'   bands = c("EVI", "NDVI")
#' )
#' point_class <- sits_classify(point_2bands, rf_model)
#' plot(point_class)
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
        data <- .apply_across(data, fn = filter_fn)
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
    .check_that(all(bands_samples == bands_data),
                msg = "Order of the bands must be the same in samples and in data"
    )

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
sits_classify.raster_cube <- function(data, ml_model, ...,
                                      roi = NULL,
                                      filter_fn = NULL,
                                      impute_fn = sits_impute_linear(),
                                      start_date = NULL,
                                      end_date = NULL,
                                      memsize = 8,
                                      multicores = 2,
                                      output_dir = ".",
                                      version = "v1",
                                      verbose = FALSE,
                                      progress = FALSE) {

    # precondition - checks if the cube and ml_model are valid
    .sits_classify_check_params(data, ml_model)

    # precondition - test if cube is regular
    if (!.cube_is_regular(data)) {
        stop("sits can only classify regular cubes. \n
             Please use sits_regularize()")
    }

    # precondition - multicores
    .check_num(
        x = multicores,
        len_max = 1,
        min = 1,
        allow_zero = FALSE,
        msg = "multicores must be at least 1"
    )

    # precondition - memory
    .check_num(
        x = memsize,
        len_max = 1,
        min = 1,
        allow_zero = FALSE,
        msg = "memsize must be positive"
    )

    # precondition - output dir
    .check_file(
        x = output_dir,
        msg = "invalid output dir"
    )

    # precondition - version
    .check_chr(x = version, len_min = 1, msg = "invalid version")

    # filter only intersecting tiles
    intersects <- slider::slide_lgl(
        data, .sits_raster_sub_image_intersects,
        roi = roi
    )

    # retrieve only intersecting tiles
    data <- data[intersects, ]

    # retrieve the samples from the model
    samples <- .sits_ml_model_samples(ml_model)

    # deal with the case where the cube has multiple rows
    probs_cube <- slider::slide_dfr(data, function(tile) {

        # find out what is the row subset that is contained
        # inside the start_date and end_date
        if (!purrr::is_null(start_date) && !purrr::is_null(end_date)) {
            old_timeline <- sits_timeline(tile)
            new_timeline <- .sits_timeline_during(
                old_timeline,
                start_date,
                end_date
            )

            # filter the cube by start and end dates
            tile$file_info[[1]] <- dplyr::filter(
                .file_info(tile),
                .data[["date"]] >= new_timeline[1] &
                    .data[["date"]] <= new_timeline[length(new_timeline)]
            )
        }

        # check
        samples_timeline_length <- length(sits_timeline(samples))
        tiles_timeline_length <- length(sits_timeline(tile))

        .check_that(
            samples_timeline_length == tiles_timeline_length,
            msg = "number of instances of samples and cube differ"
        )

        # classify the data
        probs_row <- .sits_classify_multicores(
            tile       = tile,
            ml_model   = ml_model,
            roi        = roi,
            filter_fn  = filter_fn,
            impute_fn  = impute_fn,
            memsize    = memsize,
            multicores = multicores,
            output_dir = output_dir,
            version    = version,
            verbose    = verbose,
            progress   = progress
        )

        return(probs_row)
    })

    return(probs_cube)
}
