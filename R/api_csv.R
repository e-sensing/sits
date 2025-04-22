#' @title Transform a CSV into a samples file
#' @name .csv_get_samples
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param csv_file        CSV that describes the data to be retrieved.
#' @return                A tibble with information the samples to be retrieved
#'
.csv_get_samples <- function(csv_file) {
    # read sample information from CSV file and put it in a tibble
    samples <- tibble::as_tibble(
        utils::read.csv(
            file = csv_file,
            stringsAsFactors = FALSE
        )
    ) |>
        # select valid columns
        dplyr::select(
            .conf("df_sample_columns")
        ) |>
        # transform to date
        dplyr::mutate(
            start_date = as.Date(.data[["start_date"]]),
            end_date = as.Date(.data[["end_date"]])
        )
    class(samples) <- c("sits", class(samples))
    samples
}

#' @title Transform a CSV with labelled points for accuracy assessment
#'        into a samples file
#' @name .csv_get_validation_samples
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param csv_file        CSV that describes the data to be retrieved.
#' @return                A tibble with information the samples to be retrieved
#'
.csv_get_validation_samples <- function(csv_file) {
    # read sample information from CSV file and put it in a tibble
    tibble::as_tibble(
        utils::read.csv(
            file = csv_file,
            stringsAsFactors = FALSE
        )
    ) |>
        .samples_convert_to_sits() |>
        # select valid columns
        dplyr::select(
            c("longitude", "latitude", "label")
        )
}
#' @title Transform a CSV with lat/long into samples
#' @name .csv_get_lat_lon
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param csv_file        CSV that describes the data to be retrieved.
#' @return                A tibble with information the samples to be retrieved
#'
.csv_get_lat_lon <- function(csv_file) {
    # read sample information from CSV file and put it in a tibble
    tibble::as_tibble(
        utils::read.csv(
            file = csv_file,
            stringsAsFactors = FALSE
        )
    ) |>
        # select valid columns
        dplyr::select(
            c("longitude", "latitude")
        )
}
#' @title Get samples metadata as CSV
#' @name .csv_metadata_from_samples
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param data        A sits tibble.
#' @return            A tibble with metadata
#'
.csv_metadata_from_samples <- function(data) {
    # select the parts of the tibble to be saved
    csv_columns <- .conf("df_sample_columns")
    csv <- dplyr::select(data, dplyr::all_of(csv_columns))
    # create a column with the id
    n_rows_csv <- nrow(csv)
    id <- tibble::tibble(id = seq_len(n_rows_csv))
    # join the two tibbles
    dplyr::bind_cols(id, csv)
}
