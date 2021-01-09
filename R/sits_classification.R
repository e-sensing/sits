#' @title Classify time series or data cube using machine learning models
#' @name sits_classify
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
        class_data <- .sits_config_cube_class(data[1, ]$type)
        class(data) <- c(class_data, class(data))
    }

    # Dispatch
    UseMethod("sits_classify", data)
}

#' @title Classify a set of time series using machine learning models
#' @name sits_classify.sits
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
#' @param  filter            Smoothing filter to be applied (if desired).
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
                               filter = NULL,
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
    assertthat::assert_that(!purrr::is_null(ml_model),
        msg = "sits_classify_ts: please provide a trained ML model"
    )

    # Precondition: only savitsky-golay and whittaker filters are supported
    if (!purrr::is_null(filter)) {
        call_names <- deparse(sys.call())
        assertthat::assert_that(any(grepl("sgolay", (call_names))) ||
            any(grepl("whittaker", (call_names))),
        msg = "sits_classify_cube: only savitsky-golay and whittaker filters
                            are supported"
        )
        data <- filter(data)
    }

    # precondition - are the samples valid?
    samples <- environment(ml_model)$data
    assertthat::assert_that(NROW(samples) > 0,
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
    assertthat::assert_that(NROW(distances) > 0,
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
#' @param  data            data cube
#' @param  ml_model        R model trained by \code{\link[sits]{sits_train}}.
#' @param  ...             other parameters to be passed to specific functions
#' @param  roi             a region of interest (see above)
#' @param  filter          smoothing filter to be applied (if desired).
#' @param  impute_fn       impute function to replace NA
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of cores to be used for classification.
#' @param  output_dir      directory for output file
#' @param  version         version of the output (for multiple classifications)
#' @param  .verbose        print detailed information on processing steps
#' @return                 cube with the metadata of a brick of probabilities.
#'
#' @examples
#'
#' \dontrun{
#' # Classify a raster file with 23 instances for one year
#' ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
#' package = "sits"))
#'
#' # create a data cube based on the information about the files
#' sinop_2014 <- sits_cube(
#'     type = "BRICK",
#'     name = "sinop-2014",
#'     timeline = timeline_2013_2014,
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     bands = c("ndvi"),
#'     files = c(ndvi_file)
#' )
#'
#' # select band "NDVI"
#' samples <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples, ml_method = sits_rfor(num_trees = 300))
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop_2014,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 1
#' )
#'
#' # label the classified image
#' sinop_label <- sits_label_classification(sinop_probs, output_dir = tempdir())
#' }
#' @export
sits_classify.raster_cube <- function(data, ml_model, ...,
                                      roi = NULL,
                                      filter = NULL,
                                      impute_fn = sits_impute_linear(),
                                      memsize = 8,
                                      multicores = 2,
                                      output_dir = "./",
                                      version = "v1",
                                      .verbose = FALSE) {

    # precondition - checks if the cube and ml_model are valid
    .sits_classify_check_params(data, ml_model)

    # filter only intersecting tiles
    intersects <- slider::slide(data, function(row) {

        .sits_raster_sub_image_intersects(row, roi)
    }) %>% unlist()

    data <- data[intersects,]

    # deal with the case where the cube has multiple rows
    probs_rows <- slider::slide(data, function(row) {

        # set the name of the cube
        if (!is.na(row$tile)) {
              row$name <- paste0(row$name, "_", row$tile)
          }

        # classify the data
        probs_row <- .sits_classify_multicores(
            cube = row,
            ml_model = ml_model,
            name = row$name,
            roi = roi,
            filter = filter,
            impute_fn = impute_fn,
            memsize = memsize,
            multicores = multicores,
            output_dir = output_dir,
            version = version,
            .verbose = .verbose
        )
        return(probs_row)
    })
    cube_probs <- dplyr::bind_rows(probs_rows)
    return(cube_probs)
}
