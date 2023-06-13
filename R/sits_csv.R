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
