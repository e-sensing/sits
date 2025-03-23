# ---- Distances ----
#' @title Calculate the DTW distance between temporal patterns and time-series.
#' @name .dtw_distance_windowed
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description This function calculates the DTW distance between label patterns
#' and real data (e.g., sample data, data cube data). The distance is calculated
#' using windows.
#' @keywords internal
#' @noRd
.dtw_distance_windowed <- function(data, patterns, windows) {
    # Calculate the DTW distance between `data` and `patterns`
    .map_dfc(patterns, function(pattern) {
        # Get pattern data
        pattern_ts <- as.matrix(.ts_values(pattern))
        # Windowed search
        distances <- purrr::map_df(windows, function(window) {
            # Get time-series in the window
            data_in_window <- as.matrix(.ts_values(data[window,]))
            # Calculate distance
            data.frame(distance = dtw_distance(data_in_window, pattern_ts))
        })
        # Associate the pattern name with the distances
        stats::setNames(distances, pattern[["label"]][[1]])
    })
}
# ---- Operation mode ----
#' @title Search for events in data cube.
#' @name .dtw_cube
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description This function searches for events in data cubes.
#' @keywords internal
#' @noRd
.dtw_cube <- function(values, patterns, window, threshold, ...) {
    # Extract dates
    dates <- .ts_index(values[[1]])
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
    # Transform comparison windows to indices to avoid filters
    comparison_windows <- purrr::map(comparison_windows, function(window) {
        which(
            dates >= window[["start"]] & dates <= window[["end"]]
        )
    })
    # Do the change detection for each time-series
    as.matrix(
        purrr::map_vec(values, function(value_row) {
            # Search for the patterns
            patterns_distances <- .dtw_distance_windowed(
                data       = value_row,
                patterns   = patterns,
                windows    = comparison_windows
            )
            # Define what intervals are detection
            detection <- patterns_distances[patterns_distances > threshold]
            detection <- detection[which.max(detection)]
            # Select the detection
            # (min is used to avoid errors with equal DTW distances)
            min(which(patterns_distances == detection))
        })
    )
}
#' @title Search for events in time-series.
#' @name .dtw_ts
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description This function searches for events in time-series
#' @keywords internal
#' @noRd
.dtw_ts <- function(values, patterns, window, threshold, ...) {
    # Extract dates
    dates <- .ts_index(values[[1]])
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
    # Transform comparison windows to indices to avoid filters
    comparison_windows_idx <- purrr::map(comparison_windows, function(window) {
        which(
            dates >= window[["start"]] & dates <= window[["end"]]
        )
    })
    # Do the change detection for each time-series
    purrr::map(values, function(value_row) {
        # Search for the patterns
        patterns_distances <- .dtw_distance_windowed(
            data       = value_row,
            patterns   = patterns,
            windows    = comparison_windows_idx
        )
        # Remove distances out the user-defined threshold
        patterns_distances[patterns_distances < threshold] <- NA
        # Define where each label was detected. For this, first
        # get from each label the minimal distance
        detections_idx <- apply(patterns_distances, 2, which.max)
        detections_name <- names(detections_idx)
        # For each label, extract the metadata where they had
        # minimal distance
        purrr::map_df(seq_along(detections_idx), function(idx) {
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
