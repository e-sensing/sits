
#' @title Extract temporal pattern of samples using temporal median.
#' @name .pattern_temporal_median
#' @keywords internal
#' @noRd
.pattern_temporal_median <- function(samples) {
    samples |>
        dplyr::group_by(.data[["label"]]) |>
        dplyr::group_map(function(data, name) {
            ts_median <- dplyr::bind_rows(data[["time_series"]]) |>
                dplyr::group_by(.data[["Index"]]) |>
                dplyr::summarize(dplyr::across(dplyr::everything(),
                                               stats::median, na.rm = TRUE)) |>
                dplyr::select(-.data[["Index"]])

            ts_median["label"] <- name
            ts_median
        })
}

#' @title Calculate the DTW distance between label patterns and sample data.
#' @name .pattern_distance_dtw
#' @description This function calculates the DTW distance between label patterns
#' and sample data in a given temporal window.
#' @keywords internal
#' @noRd
.pattern_distance_dtw <- function(data, patterns, windows) {
    # Calculate the DTW distance between `data` and `patterns`
    purrr::map_dfc(1:length(patterns), function(pattern_index) {
        # Get pattern metadata
        pattern <- patterns[pattern_index][[1]]
        pattern_label <- unique(pattern[["label"]])
        # Get pattern data
        pattern_ts <- dplyr::select(pattern, -.data[["label"]])
        pattern_ts <- as.matrix(pattern_ts)
        # Windowed search
        distances <- purrr::map_df(windows, function(window) {
            # Get time-series in the window
            data_in_window <-
                dplyr::filter(data,
                              .data[["Index"]] >= window[["start"]],
                              .data[["Index"]] <= window[["end"]])
            # Remove the time reference column
            data_in_window <- dplyr::select(data_in_window, -.data[["Index"]])
            # Transform values in matrix (as expected in the cpp code)
            data_in_window <- as.matrix(data_in_window)
            data_in_window <- data_in_window
            # Calculate distance
            distance_from_pattern <- dtw_distance(data_in_window, pattern_ts)
            # Prepare result and return it
            data.frame(distance = distance_from_pattern)
        })
        # Associate the pattern name with the distances
        stats::setNames(distances, pattern_label)
    })
}
