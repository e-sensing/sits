#' @title Creates a directory and logfile for logging information and debug
#' @name sits_log
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Creates a logger file for debugging and information (uses log4r package)
#'
#' @param  overwrite   If a file exist should it be overwritten?
#' @return logger      A pointer to a logger
#' @examples
#' # Creates a sits logger using the default location
#' logger <- sits_log()
#' @export
sits_log <- function(overwrite = TRUE) {

    # Does a logger object exist? If not create it
    if (!exists(as.character(substitute(logger)))) {

        # try to find a valid log file
        WD <- getwd()
        directory <- (paste0(WD, "/log"))
        # creates a directory if non-existent
        dir.create(directory, showWarnings = FALSE, recursive = TRUE)
        # find out the file path
        file <- "sits_logfile.txt"
        file_path <- paste0(directory,"/", file)
        # does the file exist?
        if (file.exists(file_path))
            # should we overwrite it?
            if (overwrite)
                file.create(file_path, showWarnings = TRUE)
        # if the log file does not exist, create it
        else{
            file.create(file_path, showWarnings = TRUE)
        }
        # Set the logger's file output: currently only allows flat files
        logger <<- log4r::create.logger(logfile = file_path, level = "ERROR")

        message(paste0("Created logger for SITS package in ", file_path))
        return(logger)
    }
    return(logger)
}
