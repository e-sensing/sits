#' @title Dynamic Time Warping for Detect changes.
#' @name sits_dtw
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Create a Dynamic Time Warping (DTW) method for the
#' \code{\link[sits]{sits_detect_change_method}}.
#'
#' @param  samples       Time series with the training samples.
#' @param ...            Other relevant parameters.
#' @param  threshold     Threshold used to define if an event was detected.
#' @param  window        ISO8601-compliant time period used to define the
#'                       DTW moving window, with number and unit,
#'                       where "D", "M" and "Y" stands for days, month and
#'                       year; e.g., "P16D" for 16 days. This parameter is not
#'                       used in operations with data cubes.
#' @param  start_date    Initial date of the interval used to extract the
#'                       patterns from the samples.
#' @param  end_date      Final date of the interval used to extract the
#'                       patterns from the samples.
#' @param  patterns      Temporal patterns of the each label available in
#'                       `samples`.
#' @return               Change detection method prepared to be passed to
#'                       \code{\link[sits]{sits_detect_change_method}}
#' @noRd
sits_dtw <- function(samples = NULL,
                     ...,
                     threshold = NULL,
                     start_date = NULL,
                     end_date = NULL,
                     window = NULL,
                     patterns = NULL) {
    .check_set_caller("sits_dtw")
    train_fun <-
        function(samples) {
            # Check parameters
            .check_period(window)
            .check_null_parameter(threshold)
            .check_date_parameter(start_date, allow_null = TRUE)
            .check_date_parameter(end_date, allow_null = TRUE)
            # Generate patterns (if not defined by the user)
            if (!.has(patterns)) {
                # Save samples used to generate temporal patterns
                patterns_samples <- samples
                # Filter samples if required
                if (!is.null(start_date) && !is.null(end_date)) {
                    patterns_samples <- .samples_select_interval(
                        samples = patterns_samples,
                        start_date = start_date,
                        end_date = end_date
                    )
                }
                # Generate samples patterns (temporal median)
                patterns <- .pattern_temporal_median(patterns_samples)
            }
            # Check patterns
            .check_chr_contains(
                x = .samples_labels(samples),
                contains = .pattern_labels(patterns)
            )
            # Define detection function
            detect_change_fun <- function(values, ...) {
                options <- list(...)
                # Extract tile
                tile <- options[["tile"]]
                # Fill with zeros remaining NA pixels
                values[is.na(values)] <- NA
                # Define the type of the operation
                dtw_fun <- .dtw_ts
                # Check if is in data cube context
                if (!is.null(tile)) {
                    # Transform values as time-series
                    values <- .pred_as_ts(
                        data = values,
                        bands = .samples_bands(samples),
                        timeline = .tile_timeline(tile)
                    )
                    # Nest time-series
                    values <- tidyr::nest(
                        .data = values,
                        .by = "sample_id",
                        .key = "time_series"
                    )
                    # Extract time-series
                    values <- values[["time_series"]]
                    # Update dtw function to classify data cube
                    dtw_fun <- .dtw_cube
                }
                # Detect changes!
                dtw_fun(
                    values = values,
                    patterns = patterns,
                    window = window,
                    threshold = threshold
                )
            }
            # Set model class
            detect_change_fun <- .set_class(
                detect_change_fun,
                "dtw_model",
                "sits_model",
                class(detect_change_fun)
            )
            detect_change_fun
        }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
