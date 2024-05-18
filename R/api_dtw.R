# ---- Distances ----
#' @title Calculate the DTW distance between temporal patterns and time-series.
#' @name .dtw_distance
#' @description This function calculates the DTW distance between label patterns
#' and real data (e.g., sample data, data cube data). The distance is calculated
#' without a window. It's use is recommended for big datasets.
#' @keywords internal
#' @noRd
.dtw_distance <- function(data, patterns) {
    # Prepare input data
    data <- as.matrix(.ts_values(data))
    # Calculate the DTW distance between `data` and `patterns`
    purrr::map_dfc(patterns, function(pattern) {
        # Prepare pattern data
        pattern_ts <- as.matrix(.ts_values(pattern))
        # Calculate distance
        stats::setNames(
            data.frame(distance = dtw_distance(data, pattern_ts)),
            pattern[["label"]][[1]]
        )
    })
}
#' @title Calculate the DTW distance between temporal patterns and time-series.
#' @name .dtw_distance_windowed
#' @description This function calculates the DTW distance between label patterns
#' and real data (e.g., sample data, data cube data). The distance is calculated
#' using windows.
#' @keywords internal
#' @noRd
.dtw_distance_windowed <- function(data, patterns, windows) {
    # Calculate the DTW distance between `data` and `patterns`
    purrr::map_dfc(patterns, function(pattern) {
        # Get pattern data
        pattern_ts <- as.matrix(.ts_values(pattern))
        # Windowed search
        distances <- purrr::map_df(windows, function(window) {
            # Get time-series in the window
            data_in_window <-
                dplyr::filter(data,
                              .data[["Index"]] >= window[["start"]],
                              .data[["Index"]] <= window[["end"]])
            # Remove the time reference column
            data_in_window <- as.matrix(.ts_values(data_in_window))
            # Calculate distance
            data.frame(distance = dtw_distance(data_in_window, pattern_ts))
        })
        # Associate the pattern name with the distances
        stats::setNames(distances, pattern[["label"]][[1]])
    })
}
# ---- Operation mode ----
#' @title Search for events in time series using complete data (no windowing).
#' @name .dtw_complete_ts
#' @description This function searches for events in time series without
#' windowing.
#' @keywords internal
#' @noRd
.dtw_complete_ts <- function(values, patterns, threshold, ...) {
    # Do the change detection for each time-series
    purrr::map_vec(values, function(value_row) {
        # Search for the patterns
        patterns_distances <- .dtw_distance(
            data       = value_row,
            patterns   = patterns
        )
        # Remove distances out the user-defined threshold
        as.numeric(any(patterns_distances <= threshold))
    })
}
#' @title Search for events in time series using windowing.
#' @name .dtw_windowed_ts
#' @description This function searches for events in time series with windowing.
#' @keywords internal
#' @noRd
.dtw_windowed_ts <- function(values, patterns, window, threshold) {
    # Extract dates
    dates_min  <- .ts_min_date(values[[1]])
    dates_max  <- .ts_max_date(values[[1]])
    # Assume time-series are regularized, then use the period
    # as the step of the moving window. As a result, we have
    # one step per iteration.
    dates_step <- lubridate::as.period(
        lubridate::int_diff(.ts_index(values[[1]]))
    )
    dates_step <- dates_step[[1]]
    # Create comparison windows
    comparison_windows <- .period_windows(
        period = window,
        step = dates_step,
        start_date = dates_min,
        end_date = dates_max
    )
    # Do the change detection for each time-series
    purrr::map(values, function(value_row) {
        # Search for the patterns
        patterns_distances <- .dtw_distance_windowed(
            data       = value_row,
            patterns   = patterns,
            windows    = comparison_windows
        )
        # Remove distances out the user-defined threshold
        patterns_distances[patterns_distances > threshold] <- NA
        # Define where each label was detected. For this, first
        # get from each label the minimal distance
        detections_idx <- apply(patterns_distances, 2, which.min)
        detections_name <- names(detections_idx)
        # For each label, extract the metadata where they had
        # minimal distance
        purrr::map_df(seq_len(length(detections_idx)), function(idx) {
            # Extract detection name and inced
            detection_name <- detections_name[idx]
            detection_idx <- detections_idx[idx]
            # Extract detection distance (min one defined above)
            detection_distance <- patterns_distances[detection_idx,]
            detection_distance <- detection_distance[detection_name]
            detection_distance <- as.numeric(detection_distance)
            # Extract detection dates
            detection_dates <- comparison_windows[[detection_idx]]
            # Prepare result and return it!
            tibble::tibble(
                change = detection_name,
                distance = detection_distance,
                from = detection_dates[["start"]],
                to = detection_dates[["end"]]
            )
        })
    })
}
