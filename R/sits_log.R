#' @title Creates a directory and logfile for logging information and debug
#' @name sits_log
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Creates a logger file for debugging and information (uses log4r package)
#'
#' @return logger      A pointer to a logger
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
    return(invisible(sits.env$logger))
}
