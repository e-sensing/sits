#' @title Classify a distances tibble using machine learning models
#' @name .sits_classify_ts
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  samples    a tibble with sits samples
#' @param  ml_model   model trained by \code{\link[sits]{sits_train}}.
#' @param  filter_fn  Smoothing filter to be applied (if desired).
#' @param  multicores number of threads to process the time series.
#' @param  progress   Show progress bar?
#' @return A tibble with the predicted labels.
.sits_classify_ts <- function(samples,
                              ml_model,
                              filter_fn,
                              multicores,
                              progress) {

    # Start parallel workers
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Get bands from model
    bands <- .ml_bands(ml_model)

    # Update samples bands order
    if (any(bands != .sits_bands(samples))) {
        samples <- .sits_select_bands(samples = samples, bands = bands)
    }

    # Apply time series filter
    if (.has(filter_fn)) {
        samples <- .apply_across(data = samples, fn = filter_fn)
    }

    # Compute the breaks in time for multiyear classification
    class_info <- .timeline_class_info(
        data = samples, samples = .ml_samples(ml_model)
    )

    # Split long time series of samples in a set of small time series
    if (length(class_info[["dates_index"]][[1]]) > 1) {
        splitted <- .sits_split(
            samples = samples,
            split_intervals = class_info[["dates_index"]][[1]]
        )
        pred <- .predictors(samples = splitted, ml_model = ml_model)
        # Post condition: is predictor data valid?
        .check_predictors(pred, splitted)
    } else {
        # Convert samples time series in predictors and preprocess data
        pred <- .predictors(samples = samples, ml_model = ml_model)
    }

    # Divide samples predictors in chunks to parallel processing
    parts <- .pred_create_partition(pred = pred, partitions = multicores)
    # Do parallel process
    prediction <- .jobs_map_parallel_dfr(parts, function(part) {
        # Get predictors of a given partition
        pred_part <- .part_predictors(part)
        # Get predictors features to classify
        values <- .pred_features(pred_part)
        # Classify
        values <- ml_model(values)
        # Return classification
        values <- tibble::tibble(data.frame(values))
        values
    }, progress = progress)

    # Store the result in the input data
    if (length(class_info[["dates_index"]][[1]]) > 1) {
        prediction <- .tibble_prediction_multiyear(
            data = samples,
            class_info = class_info,
            prediction = prediction
        )
    } else {
        prediction <- .tibble_prediction(
            data = samples,
            prediction = prediction
        )
    }
    # Set result class and return it
    .set_class(x = prediction, "predicted", class(samples))
}

#' @title Shows the predicted labels for a classified tibble
#' @name sits_show_prediction
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a tibble with a classified time series
#' by a machine learning method and displays the result.
#'
#' @param  class    A SITS tibble that has been classified.
#' @return          Tibble with the columns "from", "to", "class"
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train a tempCNN model
#'     ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_tempcnn)
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#'     sits_show_prediction(point_class)
#' }
#'
#' @export
sits_show_prediction <- function(class) {

    # set caller to show in errors
    .check_set_caller("sits_show_prediction")
    .check_predicted(class)

    return(dplyr::select(
        dplyr::bind_rows(class$predicted),
        c("from", "to", "class")
    ))
}
