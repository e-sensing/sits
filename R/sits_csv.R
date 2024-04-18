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
    data <- .check_samples(data)
    # check the file name is valid
    if (.has(file))
        .check_file(
            x = file,
            extensions = "csv",
            file_exists = FALSE
        )
    # select the parts of the tibble to be saved
    csv_columns <- .conf("df_sample_columns")
    csv <- dplyr::select(data, dplyr::all_of(csv_columns))
    # create a column with the id
    n_rows_csv <- nrow(csv)
    id <- tibble::tibble(id = 1:n_rows_csv)
    # join the two tibbles
    csv <- dplyr::bind_cols(id, csv)
    # write the CSV file
    if (.has(file)) {
        utils::write.csv(csv, file, row.names = FALSE, quote = FALSE)
        return(invisible(csv))
    } else
        return(csv)
}
#' @rdname sits_to_csv
#' @export
sits_to_csv.tbl_df <- function(data, file) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_tibble_cols") %in% colnames(data)))
        class(data) <- c("sits", class(data))
    else
        stop(.conf("messages", "sits_to_csv_default"))
    data <- sits_to_csv(data, file)
    return(invisible(data))
}
#' @rdname sits_to_csv
#' @export
sits_to_csv.default <- function(data, file) {
    stop(.conf("messages", "sits_to_csv_default"))
}
