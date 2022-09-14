#' @title Use time series values as distances for training patterns
#' @name .sits_distances
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The attributes used to train the model
#' are the series themselves. It extracts the time series from a sits tibble
#' and "spreads" them in time to produce a tibble with distances.
#'
#' @param  data       Tibble with time series data and metadata.
#' @return            Data.table where columns have the reference label
#'                    and the time series values as distances.
#'
.sits_distances <- function(data) {

    # get bands order
    bands <- names(data$time_series[[1]][-1])

    # create a tibble with the time series transposed from columns to rows
    # and create original_row and reference columns as the first two
    # columns for training
    distances_tbl <- data %>%
        dplyr::mutate(
            original_row = seq_len(nrow(data)),
            reference = .data[["label"]]
        ) %>%
        tidyr::unnest("time_series") %>%
        dplyr::select("original_row", "reference", !!bands) %>%
        dplyr::group_by(.data[["original_row"]]) %>%
        dplyr::mutate(temp_index = seq_len(dplyr::n())) %>%
        dplyr::ungroup()

    if (length(bands) > 1) {
        distances_tbl <- tidyr::pivot_wider(distances_tbl,
            names_from = .data[["temp_index"]],
            values_from = !!bands,
            names_sep = ""
        )
    } else {
        distances_tbl <- tidyr::pivot_wider(distances_tbl,
            names_from = .data[["temp_index"]],
            values_from = !!bands,
            names_prefix = bands,
            names_sep = ""
        )
    }

    distances_tbl <- data.table::data.table(distances_tbl)
    # postcondition
    .check_na(distances_tbl)

    return(distances_tbl)
}

#' @title Sample a percentage of a time series distance matrix
#' @name .sits_distances_sample
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new table contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicates
#'              the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples
#'              without reposition. If frac > 1, no sampling is done.
#'
#' @param  distances       Distances associated to a time series.
#' @param  frac            Percentage of samples to pick.
#' @return                 Data.table with a fixed quantity of samples
#'                         of informed labels and all other.
.sits_distances_sample <- function(distances, frac) {
    # compute sampling
    reference <- NULL # to avoid setting global variable
    result <- distances[, .SD[sample(.N, round(frac * .N))], by = reference]

    return(result)
}
