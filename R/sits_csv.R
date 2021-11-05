#' @title Export a sits tibble metadata to the CSV format
#'
#' @name sits_metadata_to_csv
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
#' @param  data       A sits time series.
#' @param  file       Name of the exported CSV file.
#'
#' @return The status of the operation.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series
#' csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
#' sits_metadata_to_csv(cerrado_2classes, file = csv_file)
#'
#' @export
#'
sits_metadata_to_csv <- function(data, file) {

    # set caller to show in errors
    .check_set_caller("sits_metadata_to_csv")

    .check_that(
        x = suppressWarnings(file.create(file)),
        msg = "file is not writable"
    )

    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    # select the parts of the tibble to be saved
    csv <- dplyr::select(data, csv_columns)

    .check_that(
        x = nrow(csv) > 0,
        msg = "invalid csv file"
    )
    n_rows_csv <- nrow(csv)
    # create a column with the id
    id <- tibble::tibble(id = 1:n_rows_csv)

    # join the two tibbles
    csv <- dplyr::bind_cols(id, csv)

    # write the CSV file
    utils::write.csv(csv, file, row.names = FALSE, quote = FALSE)

    return(invisible(file))
}
#' @title Check if a CSV tibble is valid
#' @name  .sits_csv_check
#' @keywords internal
#'
#' @param  csv       Tibble read from a CSV file
#' @return A logical value
#'
.sits_csv_check <- function(csv) {

    # set caller to show in errors
    .check_set_caller(".sits_csv_check")

    # check if required col names are available
    .check_chr_within(
        x = c("longitude", "latitude", "start_date", "end_date", "label"),
        within = colnames(csv),
        msg = "invalid csv file")

    return(invisible(TRUE))
}
