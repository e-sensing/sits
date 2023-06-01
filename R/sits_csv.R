#' @title Export a sits tibble metadata to the CSV format
#'
#' @name sits_to_csv
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts metadata from a sits tibble to a CSV file.
#'              The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a
#'              CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date",
#'              "end_date", "cube", "label").
#'
#' @param  data       Time series.
#' @param  file       Name of the exported CSV file.
#'
#' @return            No return value, called for side effects.
#'
#' @examples
#' csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
#' sits_to_csv(cerrado_2classes, file = csv_file)
#' @export
#'
sits_to_csv <- function(data, file) {

    # set caller to show in errors
    .check_set_caller("sits_metadata_to_csv")

    # check the samples are valid
    .check_samples(data)

    .check_that(
        x = suppressWarnings(file.create(file)),
        msg = "file is not writable"
    )

    csv_columns <- .conf("df_sample_columns")
    # select the parts of the tibble to be saved
    csv <- dplyr::select(data, dplyr::all_of(csv_columns))

    n_rows_csv <- nrow(csv)
    # create a column with the id
    id <- tibble::tibble(id = 1:n_rows_csv)

    # join the two tibbles
    csv <- dplyr::bind_cols(id, csv)

    # write the CSV file
    utils::write.csv(csv, file, row.names = FALSE, quote = FALSE)
}

#' @title Transform a CSV into a samples file
#' @name .csv_get_samples
#' @author Gilberto Camara
#' @keywords internal
#' @noRd
#' @param csv_file        CSV that describes the data to be retrieved.
#' @return                A tibble with information the samples to be retrieved
#'
.csv_get_samples <- function(csv_file) {

    # read sample information from CSV file and put it in a tibble
    samples <- tibble::as_tibble(utils::read.csv(csv_file))

    # pre-condition - check if CSV file is correct
    .check_csv(samples)

    # select valid columns
    samples <- dplyr::select(
        samples,
        .conf("df_sample_columns")
    )
    # transform to date
    samples <- dplyr::mutate(samples,
                             start_date = as.Date(.data[["start_date"]]),
                             end_date = as.Date(.data[["end_date"]])
    )

    class(samples) <- c("sits", class(samples))

    return(samples)
}
