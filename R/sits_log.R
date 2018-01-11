#' @title Creates a directory and logfile for logging information and debug
#' @name sits_log
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Creates a logger file for debugging and information (uses log4r package)
#'
#' @return boolean      TRUE if creation succeeds
#' @examples
#' # Creates a sits logger using the default location
#' logger <- sits_log()
#' @export
sits_log <- function() {

    dir.create(dirname(sits.env$config$log_file), showWarnings = FALSE)
    # Does a logger object exist? If not create it
    if (purrr::is_null(sits.env$logger)) {
        sits.env$logger <- log4r::create.logger(logfile = sits.env$config$log_file, level = "ERROR")
        message(paste0("Created logger for SITS package in ", sits.env$config$log_file))
    }
    ensurer::ensure_that(sits.env$config$log_file, file.exists(.),
                         err_desc = "Log file does not exist")
    return(TRUE)
}

.sits_log_data <- function(data, file_name = "data_save.rda") {
    # pre-conditions
    ensurer::ensure_that(data, !purrr::is_null(.),
                         err_desc = "Cannot save NULL data")

    save(data.tb, file = paste0(dirname(sits.env$config$log_file),"/", file_name))

    # post-condition
    ensurer::ensure_that(file, file.exists(.),
                         err_desc = "Unable to save temporary data")
}

.sits_log_CSV <- function(csv.tb, file_name = "csv.rda"){
    # pre-conditions
    ensurer::ensure_that(csv.tb, !purrr::is_null(.),
                         err_desc = "Cannot save NULL CSV data")

    sits_toCSV(csv.tb, file = paste0(dirname(sits.env$config$log_file),"/", file_name))

    # post-condition
    ensurer::ensure_that(file, file.exists(.),
                         err_desc = "Unable to save temporary CSV data")
}
