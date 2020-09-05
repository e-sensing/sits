#' @title Export a sits tibble metadata to the CSV format
#' @name sits_metadata_to_csv
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
#' @return The status of the operation.
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_metadata_to_csv (cerrado_2classes, file = "./cerrado_2classes.csv")
#' # cleanup (optional)
#' file.remove("./cerrado_2classes.csv")
#' }
#' @export
sits_metadata_to_csv <- function(data, file){
    # backward compatibility
    data <- .sits_tibble_rename(data)
    assertthat::assert_that(suppressWarnings(file.create(file)),
                         msg = "sits_metadata_to_csv - file is not writable")

    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    #select the parts of the tibble to be saved
    csv.tb <- dplyr::select(data, csv_columns)

    assertthat::assert_that(NROW(csv.tb) > 0,
                         msg = "sits_metadata_to_csv: invalid csv file")
    n_rows_csv <- NROW(csv.tb)
    # create a column with the id
    id.tb <- tibble::tibble(id = 1:n_rows_csv)

    # join the two tibbles
    csv.tb <- dplyr::bind_cols(id.tb, csv.tb)

    # write the CSV file
    utils::write.csv(csv.tb, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}

#' @title Export a sits tibble data to the CSV format
#' @name sits_data_to_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts time series data from a sits tibble to a CSV file.
#'              The CSV file will not contain the metadata,
#'              but only the actual time series, with a reference value.
#'              This function is useful to export the data to external apps.
#'
#' @param  data       A tibble with time series data and metadata.
#' @param  file       Name of the exported CSV file.
#' @return            Status of the operation.
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series
#' sits_data_to_csv(cerrado_2classes, file = "cerrado_2classes.csv")
#'
#' # cleanup (optional)
#' file.remove("./cerrado_2classes.csv")
#' }
#' @export
sits_data_to_csv <- function(data, file){
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # check if data is valid
    .sits_test_tibble(data)

    assertthat::assert_that(suppressWarnings(file.create(file)),
                         msg = "sits_data_to_csv - file is not writable")

    distances_DT <- .sits_distances(data)

    # write the CSV file
    utils::write.csv(distances_DT, file, row.names = FALSE, quote = FALSE)

    return(invisible(TRUE))
}

#' @title Check if a CSV tibble is valid
#' @name  .sits_csv_check
#'
#' @param  csv.tb    Tibble read from a CSV file
#' @return           TRUE/FALSE
#'
.sits_csv_check <- function(csv.tb){
    # check if required col names are available
    assertthat::assert_that(
        all(c("longitude", "latitude","start_date", "end_date", "label")
            %in% colnames(csv.tb)),
        msg = "invalid csv file")
    return(invisible(TRUE))
}
