
#' @title Extract temporal pattern from samples data.
#' @name .pattern_temporal_median
#' @keywords internal
#' @noRd
#' @param    samples Samples data.
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
