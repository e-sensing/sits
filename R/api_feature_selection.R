#' @title Perform feature selection for a given samples
#' @param samples    Time series with the training samples
#'                   (tibble of class "sits").
#' @param ml_method  Machine learning method (default = RandomForest)
#' @param filter_fn  Smoothing filter to be applied - optional
#' @param impute_fn  Imputation function to remove NA.
#' @param folds      Number of partitions to create.
#' @param metric     Importance metric. Use \code{"mda"} for mean decrease accuracy
#'  and \code{"mdg"} for mean decrease gini.
#' @param n_iter     Number of iterations for feature selection.
#' @param acc_loss   Maximum accuracy loss accepted.
#' @param multicores Number of cores to be used for cross-validation.
#'                   (integer, min = 1, max = folds).
#' @keywords internal
#' @noRd
.feature_selection <- function(samples,
                               ml_method,
                               impute_fn,
                               filter_fn,
                               folds,
                               metric,
                               n_iter,
                               acc_loss,
                               multicores) {
    # Get samples bands
    bands <- .samples_bands(samples)
    # Get the base accuracy with all features
    base_acc <- .samples_kfold(
        samples    = samples,
        folds      = folds,
        ml_method  = ml_method,
        filter_fn  = filter_fn,
        impute_fn  = impute_fn,
        gpu_memory = NULL,
        multicores = multicores,
        progress   = FALSE
    )
    # Get the base accuracy for the new iterations
    base_acc <- base_acc[["overall"]][["Accuracy"]]
    cli::cli_alert_info("Base accuracy report: {round(base_acc, 4)}")
    # Vector with removed bands
    removed_bands <- c()
    # For each iteration
    for (i in seq_len(n_iter)) {
        # Train a model and get model object
        ml_model <- .ml_model(ml_method(samples))
        # Get importance
        importance <- .feature_importance(
            ml_model = ml_model, metric = metric, bands = bands
        )
        # Get first because all values are the same
        importance <- importance |>
            dplyr::mutate(dplyr::across(dplyr::all_of(bands), sum)) |>
            dplyr::slice_head(n = 1)
        # Get the band with the smallest metric value
        band_idx <- which.min(t(importance[, bands]))
        # Set the band to remove
        band_to_remove <- bands[band_idx]
        # Remove the least representative band from samples
        bands <- setdiff(bands, band_to_remove)
        samples <- .samples_select_bands(samples, bands)
        # Evaluate CV accuracy after dropping
        new_acc <- .samples_kfold(
            samples    = samples,
            folds      = folds,
            ml_method  = ml_method,
            filter_fn  = filter_fn,
            impute_fn  = impute_fn,
            multicores = multicores,
            gpu_memory = NULL,
            progress   = FALSE
        )
        # Get the new accuracy
        new_acc <- new_acc$overall[["Accuracy"]]
        # Compare the new accuracy with the base one
        if ((base_acc - new_acc) <= acc_loss) {
            base_acc <- new_acc
            removed_bands <- c(removed_bands, band_to_remove)
        } else {
            if (length(removed_bands) == 0) {
                cli::cli_alert_info("No bands were removed.")
            }
            break
        }
        cli::cli_alert_info(
            paste0("Removed {band_to_remove} band and the new accuracy is",
                   "{round(new_acc, 4)}.")
        )
    }
    # Retun selected samples
    return(samples)
}

#' @title Get the feature importance for a given metric
#' @param ml_model Machine learning model object
#' @param metric   Importance metric. Use \code{"mda"} for mean decrease
#'  accuracy and \code{"mdg"} for mean decrease gini.
#' @param bands    Character
#' @keywords internal
#' @noRd
.feature_importance <- function(ml_model, metric, bands) {
    metric_column <- switch(
        metric,
        "mda" = "MeanDecreaseAccuracy",
        "mdg" = "MeanDecreaseGini"
    )

    importance <- randomForest::importance(ml_model)
    importance <- importance[, metric_column]
    # Transform to tibble
    importance <- tibble::as_tibble(t(importance))
    # Transpose to longer mode
    importance <- tidyr::pivot_longer(
        importance,
        dplyr::everything(),
        cols_vary = "slowest",
        names_to = c(".value", "date"),
        names_pattern = paste0(
            "(", paste0(bands, collapse = "|"), ")", "(\\d{1,3})"
        )
    )
    return(importance)
}
