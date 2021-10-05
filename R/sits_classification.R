#' @title Classify time series or data cube using machine learning models
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
#'  \item{multinomial logit: }       {see \code{\link[sits]{sits_mlr}}}
#'  \item{extreme gradient boosting: } {see \code{\link[sits]{sits_xgboost}}}
#'  \item{multi-layer perceptrons: } {see \code{\link[sits]{sits_mlp}}}
#'  \item{mixed 1D CNN and MLP networks: } {see \code{\link[sits]{sits_TempCNN}}}
#'  \item{deep residual netwroks:}{see \code{\link[sits]{sits_ResNet}}}
#'  }
#'
#' @param  data      Tibble with time series metadata and data.
#' @param  ml_model  Pre-built machine learning model
#'                   (see \code{\link[sits]{sits_train}}).
#' @param  ...       Other parameters to be passed to specific functions
#'
#' @param  data              data cube
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}.
#' @param  ...               other parameters to be passed to specific functions
#' @param  roi               a region of interest (see above)
#' @param  filter_fn         smoothing filter to be applied (if desired).
#' @param  impute_fn         impute function to replace NA
#' @param  interp_fn         function to interpolate points from cube to match
#'                           samples
#' @param  compose_fn        function to compose points from cube to match
#'                           samples
#' @param  start_date        starting date for the classification
#' @param  end_date          end date for the classification
#' @param  memsize           memory available for classification (in GB).
#' @param  multicores        number of cores to be used for classification.
#' @param  output_dir        directory for output file
#' @param  version           version of the output (for multiple
#'                           classifications)
#' @param  verbose           print information about processing time?
#' @param  progress          a logical value indicating if a progress bar
#' should be shown
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
#'    Savtizky-Golay (see \code{\link[sits]{sits_sgolay}}) and Whittaker
#'    (see \code{\link[sits]{sits_whittaker}}).
#'
#'    The "impute_fn" function is used to remove invalid or cloudy pixels
#'    from time series. The default is a linear interpolator, available
#'    in \code{\link[sits]{sits_impute_linear}}. Users can add their custom
#'    functions.
#'
#'    The "interp_fn" function is used when the training samples which
#'    were used to generate the classification model have a larger number of
#'    time instances than the data cube. In this case, pixel time series
#'    of the data cube will have to be interpolated to fit that of the samples.
#'
#'    The "compose_fn" function is used when the training samples which
#'    were used to generate the classification model have a small number of
#'    time instances than the data cube. In this case, pixel time series
#'    of the data cube will have to be composed and/or merged
#'    to fit that of the samples.
#'
#'    The "memsize" and "multicores" parameters are used for multiprocessing.
#'    The "multicores" parameter defines the number of cores used for
#'    processing. The "memsize" parameter  controls the amount of memory
#'    available for classification.
#'
#' @examples
#' \donttest{
#' # Example of classification of a time series
#' # Retrieve the samples for Mato Grosso
#' # select an extreme gradient boosting model
#' samples_2bands <- sits_select(samples_modis_4bands,
#'                             bands = c("NDVI", "EVI"))
#' xgb_model <- sits_train(samples_2bands,
#'     ml_method = sits_xgboost(verbose = FALSE)
#' )
#' # classify the point
#' point_2bands <- sits_select(samples_mt_6bands,
#'                             bands = c("NDVI", "EVI"))
#' point_class <- sits_classify(point_2bands, xgb_model)
#' plot(point_class)
#'
#' # create a data cube based on files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "sinop-2014",
#'     origin = "BDC",
#'     collection = "MOD13Q1-6",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "tile", "band", "date")
#' )
#'
#' # classify the raster image
#' probs_cube <- sits_classify(cube,
#'     ml_model = xgb_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classified image
#' label_cube <- sits_label_classification(probs_cube, output_dir = tempdir())
#' plot(label_cube)
#' }
#'
#' @export
sits_classify <- function(data, ml_model, ...) {

    # set caller to show in errors
    .check_set_caller("sits_classify")

    # check data type
    data <- .config_data_meta_type(data)

    # Dispatch
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

    # Precondition: only savitsky-golay and whittaker filters are supported
    if (!purrr::is_null(filter_fn)) {
        call_names <- deparse(sys.call())
        .check_that(
            x = any(grepl("sgolay", (call_names))) ||
                any(grepl("whittaker", (call_names))),
            msg = "only savitsky-golay and whittaker filters are supported"
        )
        data <- filter_fn(data)
    }

    # precondition - are the samples valid?
    samples <- .sits_ml_model_samples(ml_model)
    .check_that(
        x = nrow(samples) > 0,
        msg = "missing original samples"
    )

    # get normalization params
    stats <- environment(ml_model)$stats
    # has the training data been normalized?
    if (!purrr::is_null(stats))
        # yes, then normalize the input data
        distances <- .sits_distances(.sits_ml_normalize_data(
            data = data,
            stats = stats
        ))
    else
        # no, input data does not need to be normalized
        distances <- .sits_distances(data)


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
    data <- .sits_tibble_prediction(
        data = data,
        class_info = class_info,
        prediction = prediction
    )
    return(data)
}
#' @rdname sits_classify
#'
#' @export
sits_classify.raster_cube <- function(data, ml_model, ...,
                                      roi = NULL,
                                      filter_fn = NULL,
                                      impute_fn = sits_impute_linear(),
                                      interp_fn = NULL,
                                      compose_fn = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      memsize = 8,
                                      multicores = 2,
                                      output_dir = tempdir(),
                                      version = "v1",
                                      verbose = FALSE,
                                      progress = FALSE) {

    # precondition - checks if the cube and ml_model are valid
    .sits_classify_check_params(data, ml_model)

    # filter only intersecting tiles
    intersects <- slider::slide_lgl(data,
                                    .sits_raster_sub_image_intersects,
                                    roi)

    # retrieve only intersecting tiles
    data <- data[intersects, ]

    # fix cube names
    data <- .sits_cube_fix_name(data)

    # retrieve the samples from the model
    samples <- .sits_ml_model_samples(ml_model)

    # deal with the case where the cube has multiple rows
    probs_rows <- slider::slide(data, function(tile) {

        # find out what is the row subset that is contained
        # inside the start_date and end_date
        if (!purrr::is_null(start_date) && !purrr::is_null(end_date)) {
            old_timeline <- sits_timeline(tile)
            new_timeline <- .sits_timeline_during(old_timeline,
                                                  start_date,
                                                  end_date)

            # filter the cube by start and end dates
            tile$file_info[[1]] <- dplyr::filter(
                tile$file_info[[1]],
                date >= new_timeline[1] &
                    date <= new_timeline[length(new_timeline)]
            )
        }

        # temporary fix
        n_samples <- length(sits_timeline(samples))
        n_tile <- length(sits_timeline(tile))

        .check_that(
            x = n_samples == n_tile,
            msg = "number of instances of samples and cube differ"
        )

        # # The user can provide both interpolation and compositions functions
        # if (!purrr::is_null(interp_fn))
        #     interp_fn <- .sits_match_lin_interp()
        #
        # if (!purrr::is_null(compose_fn))
        #     compose_fn <- .sits_match_compose()

        # classify the data
        probs_row <- .sits_classify_multicores(
            tile       = tile,
            ml_model   = ml_model,
            roi        = roi,
            filter_fn  = filter_fn,
            impute_fn  = impute_fn,
            compose_fn = compose_fn,
            interp_fn  = interp_fn,
            memsize    = memsize,
            multicores = multicores,
            output_dir = output_dir,
            version    = version,
            verbose    = verbose,
            progress   = progress
        )

        return(probs_row)
    })

    probs_cube <- dplyr::bind_rows(probs_rows)

    class(probs_cube) <- .cube_s3class(probs_cube)

    return(probs_cube)
}
