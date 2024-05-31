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
    samples <- tibble::as_tibble(
        utils::read.csv(
            file = csv_file,
            stringsAsFactors = FALSE
        )
    )
    # pre-condition - check if CSV file is correct
    .check_samples(samples)
    # select valid columns
    samples <- dplyr::select(
        samples,
        .conf("df_sample_columns")
    )
    # transform to date
    samples <- dplyr::mutate(
        samples,
        start_date = as.Date(.data[["start_date"]]),
        end_date = as.Date(.data[["end_date"]])
    )
    class(samples) <- c("sits", class(samples))
    return(samples)
}
