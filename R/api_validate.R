.validate_sits <- function(samples, samples_validation,
                           validation_split, ml_method) {

    # Are there samples for validation?
    if (is.null(samples_validation)) {
        samples <- .tibble_samples_split(
            samples = samples,
            validation_split = validation_split
        )
        samples_validation <- dplyr::filter(samples, !.data[["train"]])
        samples <- dplyr::filter(samples, .data[["train"]])
    }
    # create a machine learning model
    ml_model <- ml_method(samples)
    # Convert samples time series in predictors and preprocess data
    predictors <- .predictors(samples = samples_validation, ml_model = ml_model)
    # Get predictors features to classify
    values <- .pred_features(predictors)
    # Classify
    values <- ml_model(values)
    # Get the labels of the data
    labels <- .samples_labels(samples)
    # Extract classified labels (majority probability)
    predicted_labels <- labels[C_label_max_prob(as.matrix(values))]
    # Call caret to provide assessment
    predicted <- factor(predicted_labels, levels = labels)
    reference <- factor(.pred_references(predictors), levels = labels)
    # Call caret package to the classification statistics
    acc_obj <- caret::confusionMatrix(predicted, reference)
    # Set result class and return it
    .set_class(x = acc_obj, "sits_accuracy", class(acc_obj))
    acc_obj
}
