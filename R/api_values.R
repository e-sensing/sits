#' @title Return the values of a set of time series
#' @name .values_ts
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description This function returns the values of a sits tibble
#' (according a specified format).
#' This function is useful to use packages such as ggplot2, dtwclust, or kohonen
#' that require values that are rowwise or colwise organized.
#'
#' @param  data       A sits tibble with time series for different bands.
#' @param  bands      Bands whose values are to be extracted.
#' @param  format     A string with either "cases_dates_bands"
#'                    or "bands_cases_dates" or "bands_dates_cases".
#'
#' @return A matrix with values.
.values_ts <- function(data, bands = NULL, format = "cases_dates_bands") {
    # set caller to show in errors
    .check_set_caller(".values_ts")
    .check_chr_within(
        format,
        within = c(
            "cases_dates_bands",
            "bands_cases_dates",
            "bands_dates_cases"
        ),
        discriminator = "one_of"
    )
    class(format) <- c(format, class(format))
    UseMethod(".values_ts", format)
}
#' @noRd
#' @export
.values_ts.cases_dates_bands <- function(data, bands = NULL, format) {
    if (.has_not(bands)) {
        bands <- .samples_bands(data)
    }
    # populates result
    values <- data[["time_series"]] |>
        purrr::map(function(ts) {
            data.matrix(dplyr::select(ts, dplyr::all_of(bands)))
        })
    values
}
#' @noRd
#' @export
.values_ts.bands_cases_dates <- function(data, bands = NULL, format) {
    if (.has_not(bands)) {
        bands <- .samples_bands(data)
    }
    # get the distances tables
    distances_tbl <- data |>
        dplyr::mutate(
            sample_id = seq_len(nrow(!!data))
        ) |>
        tidyr::unnest("time_series") |>
        dplyr::select("sample_id", dplyr::all_of(bands)) |>
        dplyr::group_by(.data[["sample_id"]]) |>
        dplyr::mutate(temp_index = seq_len(dplyr::n())) |>
        dplyr::ungroup()
    # melt the data
    if (length(bands) > 1L) {
        distances_tbl <- tidyr::pivot_wider(
            distances_tbl,
            names_from = "temp_index",
            values_from = !!bands,
            names_sep = ""
        )
    } else {
        distances_tbl <- tidyr::pivot_wider(
            distances_tbl,
            names_from = "temp_index",
            values_from = !!bands,
            names_prefix = bands,
            names_sep = ""
        )
    }
    # return values
    values <- purrr::map(bands, function(band) {
        unname(as.matrix(dplyr::select(
            distances_tbl, dplyr::starts_with(band)
        )))
    })
    names(values) <- bands
    values
}
#' @noRd
#' @export
.values_ts.bands_dates_cases <- function(data, bands = NULL, format) {
    if (.has_not(bands)) {
        bands <- .samples_bands(data)
    }
    values <- bands |> purrr::map(function(band) {
        data[["time_series"]] |>
            purrr::map(function(ts) {
                dplyr::select(ts, dplyr::all_of(band))
            }) |>
            data.frame() |>
            tibble::as_tibble() |>
            as.matrix()
    })
    names(values) <- bands
    values
}
