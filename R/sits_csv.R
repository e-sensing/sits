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
#'              If the file is NULL, returns a data.frame as an object
#'
#' @param  data       Time series (tibble of class "sits").
#' @param  file       Full path of the exported CSV file
#'                    (valid file name with extension ".csv").
#' @return            Return data.frame with CSV columns (optional)
#'
#' @examples
#' csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
#' sits_to_csv(cerrado_2classes, file = csv_file)
#' @export
#'
sits_to_csv <- function(data, file = NULL) {
    # set caller to show in errors
    .check_set_caller("sits_to_csv")
    UseMethod("sits_to_csv", data)
}
#' @rdname sits_to_csv
#' @export
sits_to_csv.sits <- function(data, file = NULL) {
    # check the samples are valid
    .check_samples(data)
    data <- .samples_convert_to_sits(data)
    # check the file name is valid
    if (.has(file)) {
        .check_file(
            x = file,
            extensions = "csv",
            file_exists = FALSE
        )
    }
    # get metadata
    csv <- .csv_metadata_from_samples(data)
    # write the CSV file
    if (.has(file)) {
        utils::write.csv(csv, file, row.names = FALSE, quote = FALSE)
    }
    return(csv)
}
#' @rdname sits_to_csv
#' @export
sits_to_csv.tbl_df <- function(data, file) {
    if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_to_csv_default"))
    }
    sits_to_csv(data, file)
}
#' @rdname sits_to_csv
#' @export
sits_to_csv.default <- function(data, file) {
    data <- tibble::as_tibble(data)
    sits_to_csv(data, file)
}
#' @title Export a a full sits tibble to the CSV format
#'
#' @name sits_timeseries_to_csv
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts metadata and data from a sits tibble to a CSV file.
#'              The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a
#'              CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date",
#'              "end_date", "cube", "label"), plus the all the time series for
#'              each data
#' @param  data       Time series (tibble of class "sits").
#' @param  file       Full path of the exported CSV file
#'                    (valid file name with extension ".csv").
#' @return            Return data.frame with CSV columns (optional)
#'
#' @examples
#' csv_ts <- sits_timeseries_to_csv(cerrado_2classes)
#' csv_file <- paste0(tempdir(), "/cerrado_2classes_ts.csv")
#' sits_timeseries_to_csv(cerrado_2classes, file = csv_file)
#' @export
#'
sits_timeseries_to_csv <- function(data, file = NULL) {
    # check the samples are valid
    .check_samples(data)
    data <- .samples_convert_to_sits(data)
    csv_1 <- .csv_metadata_from_samples(data)
    csv_2 <- .predictors(data)[-2L:0L]
    csv_ts <- dplyr::bind_cols(csv_1, csv_2)

    # write the CSV file
    if (.has(file)) {
        utils::write.csv(csv_ts,
            file,
            row.names = FALSE,
            quote = FALSE
        )
    } else {
        return(csv_ts)
    }
}
