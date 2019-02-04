#' @title Creates a directory and logfile for logging information and debug
#' @name sits_log
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Creates a logger file for debugging and information (uses log4r package).
#'
#' @return TRUE if creation succeeds.
#' @examples
#' # Creates a sits logger using the default location
#' logger <- sits_log()
#' @export
sits_log <- function() {
    sits.env$debug_file <- tempfile(pattern = "sits_debug", fileext = ".log")
    sits.env$logger_debug <- log4r::create.logger(logfile = sits.env$debug_file, level = "DEBUG")
    message(paste0("Created logger for sits package - DEBUG level at ", sits.env$debug_file))

    sits.env$error_file <- tempfile(pattern = "sits_error", fileext = ".log")
    sits.env$logger_error <- log4r::create.logger(logfile = sits.env$error_file, level = "ERROR")
    message(paste0("Created logger for sits package - ERROR level at ", sits.env$error_file))

    return(TRUE)
}

#' @title Logs an error in the log file
#' @name .sits_log_error
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Logs an error message in the log file.
#' @param message       Message to be logged.
#' @return TRUE if creation succeeds.
.sits_log_error <- function(message) {
    log4r::error(sits.env$logger_error, message)
}

#' @title Logs an error in the log file
#' @name .sits_log_debug
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Logs an debug message in the log file.
#' @param message       Message to be logged.
#' @return TRUE if creation succeeds.
.sits_log_debug <- function(message) {
    log4r::debug(sits.env$logger_debug, message)
}

#' @title Saves a data set for future use
#' @name .sits_log_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Save a data set in the log directory.
#' @param data          Data set to be saved.
#' @param file_name     Name of data to be saved.
.sits_log_data <- function(data, file_name = "data_save.rda") {
    # pre-conditions
    ensurer::ensure_that(data, !purrr::is_null(.),
                         err_desc = "Cannot save NULL data")

    file_save = paste0(dirname(sits.env$debug_file),"/", file_name)

    tryCatch({save(data, file = file_save)},
             error = function(e){
                msg <- paste0("WTSS - unable to save data in file ", file_save)
                .sits_log_error(msg)
                message("WTSS - unable to retrieve point - see log file for details" )
                return(NULL)})
}

#' @title Saves a CSV data set
#' @name .sits_log_csv
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Save a CSV data set in the log directory.
#' @param csv.tb        Tibble containing CSV data.
#' @param file_name     Name of data to be saved.
.sits_log_csv <- function(csv.tb, file_name = "errors.csv"){
    # pre-conditions
    ensurer::ensure_that(csv.tb, !purrr::is_null(.),
                         err_desc = "Cannot save NULL CSV data")

    sits_metadata_to_csv(csv.tb, file = paste0(dirname(sits.env$debug_file),"/", file_name))
}

#' @title Shows the memory used in GB
#' @name .sits_mem_used
#' @description Calls the gc() and rounds the result in GB.
#' @return Memory used in GB.
#' @export
.sits_mem_used <- function() {
    dt <- gc()
    return(sum(dt[,2]/1000))
}

#' @title Prints the error log
#' @name sits_show_errors
#' @description Prints the errors log.
#'
#' @export
sits_show_errors <- function() {
    file.show(sits.env$error_file)
}

#' @title Prints the debug log
#' @name sits_show_debug
#' @description Prints the debug log.
#'
#' @export
sits_show_debug <- function() {
    file.show(sits.env$debug_file)
}

#' @title Loads the CSV error file saved in the log directory
#' @name sits_csv_error_file
#' @description Loads the CSV error file saved in the log directory.
#' @param    file_name Name of file to be retrieved.
#' @export
sits_csv_error_file <- function(file_name = "errors.csv") {
    csv.tb <- utils::read.csv(paste0(dirname(sits.env$debug_file),"/", file_name))
    return(csv.tb)
}
