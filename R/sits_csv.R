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
