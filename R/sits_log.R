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
    file.create(sits.env$config$log_file, showWarnings = FALSE)
    # Does a logger object exist? If not create it
    if (purrr::is_null(sits.env$logger)) {
        sits.env$logger <- log4r::create.logger(logfile = sits.env$config$log_file, level = "ERROR")
        message(paste0("Created logger for SITS package in ", sits.env$config$log_file))
    }
    ensurer::ensure_that(sits.env$config$log_file, file.exists(.),
                         err_desc = "Log file does not exist")
    return(TRUE)
}

.sits_log_error <- function(message) {
    log4r::error(sits.env$logger, message)
}

.sits_log_data <- function(data, file_name = "data_save.rda") {
    # pre-conditions
    ensurer::ensure_that(data, !purrr::is_null(.),
                         err_desc = "Cannot save NULL data")

    file_save = paste0(dirname(sits.env$config$log_file),"/", file_name)

    tryCatch({save(data, file = file_save)},
             error = function(e){
                msg <- paste0("WTSS - unable to save data in file ", file_save)
                .sits_log_error(msg)
                message("WTSS - unable to retrieve point - see log file for details" )
                return(NULL)})
}

.sits_log_CSV <- function(csv.tb, file_name = "csv.rda"){
    # pre-conditions
    ensurer::ensure_that(csv.tb, !purrr::is_null(.),
                         err_desc = "Cannot save NULL CSV data")

    sits_metadata_toCSV(csv.tb, file = paste0(dirname(sits.env$config$log_file),"/", file_name))
}
