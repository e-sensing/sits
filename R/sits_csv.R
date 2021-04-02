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
    assertthat::assert_that(
        suppressWarnings(file.create(file)),
        msg = "sits_metadata_to_csv: file is not writable"
    )

    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    # select the parts of the tibble to be saved
    csv <- dplyr::select(data, csv_columns)

    assertthat::assert_that(
        nrow(csv) > 0,
        msg = "sits_metadata_to_csv: invalid csv file"
    )
    n_rows_csv <- nrow(csv)
    # create a column with the id
    id <- tibble::tibble(id = 1:n_rows_csv)

    # join the two tibbles
    csv <- dplyr::bind_cols(id, csv)

    # write the CSV file
    utils::write.csv(csv, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}

#' @title Export a sits tibble data to the CSV format
#'
#' @name sits_data_to_csv
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts time series data from a sits tibble to a CSV file.
#'              The CSV file will not contain the metadata,
#'              but only the actual time series, with a reference value.
#'              This function is useful to export the data to external apps.
#'
#' @param  data       A tibble with time series data and metadata.
#' @param  file       Name of the exported CSV file.
#'
#' @return            Status of the operation.
#'
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series
#' csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
#' sits_data_to_csv(cerrado_2classes, file = csv_file)
#'
#' @export
#'
sits_data_to_csv <- function(data, file) {

    # check if data is valid
    .sits_test_tibble(data)

    assertthat::assert_that(
        suppressWarnings(file.create(file)),
        msg = "sits_data_to_csv: file is not writable"
    )

    distances <- .sits_distances(data)

    # write the CSV file
    utils::write.csv(distances, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}

#' @title Check if a CSV tibble is valid
#' @name  .sits_csv_check
#' @keywords internal
#'
#' @param  csv       Tibble read from a CSV file
#' @return A logical value
#'
.sits_csv_check <- function(csv) {
    # check if required col names are available
    assertthat::assert_that(
        all(c("longitude", "latitude", "start_date", "end_date", "label")
        %in% colnames(csv)),
        msg = "invalid csv file"
    )
    return(invisible(TRUE))
}
