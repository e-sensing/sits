#' @title Classify time series or data cube using machine learning models
#' @name sits_classify
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series or data cube given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#' This is a generic function. The following specific functions are available:
#' \itemize{
#'  \item{"sits tibble": }{see \code{\link{sits_classify.sits}}}
#'  \item{"cube": }{see \code{\link{sits_classify.raster_cube}}}
#' }
#' SITS supports the following models:
#' \itemize{
#'  \item{support vector machines: }         {see \code{\link[sits]{sits_svm}}}
#'  \item{random forests: }                  {see \code{\link[sits]{sits_rfor}}}
#'  \item{linear discriminant analysis: }    {see \code{\link[sits]{sits_lda}}}
#'  \item{quadratic discriminant analysis: } {see \code{\link[sits]{sits_qda}}}
#'  \item{multinomial logit: }               {see \code{\link[sits]{sits_mlr}}}
#'  \item{extreme gradient boosting: }       {see \code{\link[sits]{sits_xgboost}}}
#'  \item{multi-layer perceptrons: }         {see \code{\link[sits]{sits_deeplearning}}}
#'  \item{1D convolutional neural networks: }{see \code{\link[sits]{sits_FCN}}}
#'  \item{mixed 1D and MLP networks: }       {see \code{\link[sits]{sits_TempCNN}}}
#'  \item{1D version of ResNet: }            {see \code{\link[sits]{sits_ResNet}}}
#'  \item{combined LSTM-FCN model: }         {see \code{\link[sits]{sits_LSTM_FCN}}}
#'  }
#' The model should be precomputed using \code{\link[sits]{sits_train}}
#' and then passed to the "sits_classify" function using parameter "ml_model".
#'
#' @param  data              Tibble with time series metadata and data.
#' @param  ml_model          Pre-built machine learning model
#'                             (see \code{\link[sits]{sits_train}}).
#' @param  ...               Other parameters to be passed to specific functions
#' @return                   Predicted data
#'
#' @export
sits_classify <- function(data, ml_model, ...) {

    # is the data a sits tibble? If not, it must be a cube
    if (!("sits" %in% class(data))) {
        # find out the generic cube class it belongs to
        class_data <- .sits_config_cube_class(.sits_cube_source(data))
        class(data) <- c(class_data, class(data))
    }

    # Dispatch
    UseMethod("sits_classify", data)
}

#' @title Classify a set of time series using machine learning models
#' @name sits_classify.sits
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series, given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#'
#' The model should be precomputed by the user. This model should be
#' passed to the function using the parameter "ml_model".
#'
#' @param  data              Tibble with time series metadata and data.
#' @param  ml_model          Pre-built machine learning model
#'                             (see \code{\link[sits]{sits_train}}).
#' @param  ...               Other parameters to be passed to specific functions
#' @param  filter_fn         Smoothing filter to be applied (if desired).
#' @param  multicores        Number of cores to be used for classification.
#' @return A tibble with the predicted labels for each input segment.
#'
#' @examples
#'
#' # Retrieve the samples for Mato Grosso
#' # select band "NDVI"
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi,
#'     ml_method = sits_rfor(num_trees = 300)
#' )
#'
#' # classify the point
#' point_class <- sits_classify(point_ndvi, rfor_model)
#' @export
#'
sits_classify.sits <- function(data, ml_model, ...,
                               filter_fn = NULL,
                               multicores = 2) {

    # check if we are running in Windows
    if (.Platform$OS.type != "unix") {
          multicores <- 1
      }

    # backward compatibility
    data <- .sits_tibble_rename(data)

    # precondition: verify that the data is correct
    .sits_test_tibble(data)

    # precondition: ensure the machine learning model has been built
    assertthat::assert_that(
        !purrr::is_null(ml_model),
        msg = "sits_classify_ts: please provide a trained ML model"
    )

    # Precondition: only savitsky-golay and whittaker filters are supported
    if (!purrr::is_null(filter_fn)) {
        call_names <- deparse(sys.call())
        assertthat::assert_that(
            any(grepl("sgolay", (call_names))) ||
                any(grepl("whittaker", (call_names))),
            msg = paste("sits_classify_cube: only savitsky-golay and",
                        "whittaker filters are supported")
        )
        data <- filter_fn(data)
    }

    # precondition - are the samples valid?
    samples <- .sits_ml_model_samples(ml_model)
    assertthat::assert_that(
        nrow(samples) > 0,
        msg = "sits_classify_ts: missing original samples"
    )

    # get normalization params
    stats <- environment(ml_model)$stats
    # has the training data been normalized?
    if (!purrr::is_null(stats))
          # yes, then normalize the input data
          distances <- .sits_distances(.sits_normalize_data
          (
              data = data,
              stats = stats,
              multicores = multicores
          ))
     else
          # no, input data does not need to be normalized
          distances <- .sits_distances(data)


    # post condition: is distance data valid?
    assertthat::assert_that(
        nrow(distances) > 0,
        msg = "sits_classify.sits: problem with normalization"
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


#' @title Classify a data cube using multicore machines
#' @name sits_classify.raster_cube
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a data cube, whose metadata is
#'    described by tibble (created by \code{\link[sits]{sits_cube}}),
#'    a set of samples used for training a classification model,
#'    a prediction model (created by \code{\link[sits]{sits_train}}),
#'    and produces a classified set of RasterLayers.
#'
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lat_min", "lat_max", "long_min", "long_max")
#'
#'    The "memsize" and "multicores" parameters define the
#'    The "multicores" parameter defines the
#'    number of cores used for processing. The "memsize" parameter  controls
#'    the amount of memory available for classification.
#'
#' @param  data              data cube
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}.
#' @param  ...               other parameters to be passed to specific functions
#' @param  roi               a region of interest (see above)
#' @param  filter_fn         smoothing filter to be applied (if desired).
#' @param  impute_fn         impute function to replace NA
#' @param  interp_fn         function to interpolate points from cube to match samples
#' @param  compose_fn        function to compose points from cube to match samples
#' @param  start_date        starting date for the classification
#' @param  end_date          end date for the classification
#' @param  memsize           memory available for classification (in GB).
#' @param  multicores        number of cores to be used for classification.
#' @param  output_dir        directory for output file
#' @param  version           version of the output (for multiple classifications)
#' @return                   cube with the metadata of a brick of probabilities.
#'
#' @examples
#' \dontrun{
#'
#' # create a data cube based on files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "sinop-2014",
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#'
#' # select band "NDVI"
#' samples <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples, ml_method = sits_rfor(num_trees = 300))
#'
#' # classify the raster image
#' probs_cube <- sits_classify(cube,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classified image
#' label_cube <- sits_label_classification(probs_cube, output_dir = tempdir())
#' }
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
                                      output_dir = "./",
                                      version = "v1") {

    # precondition - checks if the cube and ml_model are valid
    .sits_classify_check_params(data, ml_model)

    # filter only intersecting tiles
    intersects <- slider::slide_lgl(data,
                                    .sits_raster_sub_image_intersects,
                                    roi)

    # retrieve only intersecting tiles
    data <- data[intersects,]

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

        #  There are three possible situations:
        # (a) All samples have same number of time instances as the tile
        #     in this case there is nothing to do
        # (b) All samples have more time instances than tile
        #     In this case we have to interpolate the time series of the tile
        # (c) All samples have less time instances than tile
        #     In this case we have to compose the tile time series to
        #     match the sample size

        # # The user can provide both interpolation and compositions functions
        # if (!purrr::is_null(interp_fn))
        #     interp_fn <- .sits_match_lin_interp()
        #
        # if (!purrr::is_null(compose_fn))
        #     compose_fn <- .sits_match_compose()

        # temporary fix
        n_samples <- length(sits_timeline(samples))
        n_tile <- length(sits_timeline(tile))

        assertthat::assert_that(
            n_samples == n_tile,
            msg = paste("sits_classify: number of instances of",
                        "samples and cube differ")
        )

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
            version    = version
        )

        return(probs_row)
    })

    probs_cube <- dplyr::bind_rows(probs_rows)
    class(probs_cube) <- c("probs_cube", "raster_cube", class(probs_cube))
    return(probs_cube)
}
