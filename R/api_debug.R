#' @title Log functions
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' logs to a CSV file the following values:
#' * date_time: event date and time
#' * pid: process identifier
#' * event: event name
#' * elapsed_time: duration (in seconds) from the last log call
#' * mem_used: session used memory (in MB)
#' * max_mem_used: maximum memory used (in MB) from first log call
#' * tag: any character string to be registered
#'
#' Each event will be logged in one row in the log file.
#' The log file name will be the same as the base name of the current
#' session's temporary directory.
#'
#' @param flag     A logical value to set the debug flag
#' @param event    The name of the event to be logged
#' @param key      A key describing the value.
#' @param value    Any value to be logged. The value will be converted
#'                 to string and escaped.
#' @return  A logical value with current debug flag
.debug_log <- function(event = "", key = "", value = "") {
    # If debug flag is FALSE, then exit
    if (!.debug()) {
        return(invisible(NULL))
    }
    # Get output_dir
    output_dir <- sits_env[["output_dir"]]
    if (is.null(output_dir)) {
        return(invisible(NULL))
    }
    # Record time to compute elapsed time
    time <- Sys.time()
    on.exit(sits_env[["log_time"]] <- Sys.time(), add = TRUE)
    # Function to escape CSV values
    esc <- function(value) {
        value <- gsub("\"", "\"\"", paste0(value))
        if (grepl("[\",\n\r]", value)) {
            return(paste0('"', value, '"'))
        }
        value
    }
    # Output log file
    log_file <- .file_log_name(output_dir)
    # Elapsed time
    elapsed_time <- NULL
    if (.has(sits_env[["log_time"]])) {
        elapsed_time <- format(difftime(
            time1 = time,
            time2 = sits_env[["log_time"]],
            units = "secs"
        )[[1]], digits = 4)
    }
    # Add log header once
    if (is.null(elapsed_time)) {
        # First call to gc
        mem <- gc(reset = TRUE)
        # columns
        cat(paste0(paste(
            "date_time", "pid", "event", "elapsed_time",
            "mem_used", "max_mem_used", "key", "value",
            sep = ", "
        ), "\n"), file = log_file, append = TRUE)
    } else {
        # Memory information
        mem <- gc()
    }
    # Log entry
    cat(paste0(paste(
        esc(time), Sys.getpid(), esc(event[[1]]), elapsed_time,
        sum(mem[, 2]), sum(mem[, 6]), esc(key[[1]]), esc(list(value)),
        sep = ", "
    ), "\n"), file = log_file, append = TRUE)
    return(invisible(NULL))
}

#' @title Log functions
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' When called without parameters retrieves the current debug flag value.
#' The flag parameter sets the current flag and output_dir
#' and writes the debug info to a directory.
#'
#' @param flag  A logical value to set the debug flag.
#' @param output_dir  Directory to write the debug info.
#' @return  The flag associated to the debug.
.debug <- function(flag = NULL, output_dir = NULL) {
    .check_set_caller(".debug")
    # If no parameter is passed get current debug flag
    if (is.null(flag)) {
        flag <- sits_env[["debug_flag"]]

        # Defaults to FALSE
        if (is.null(flag)) {
            flag <- FALSE
            sits_env[["debug_flag"]] <- flag
        }

        return(flag)
    }
    .check_lgl_parameter(flag, allow_null = TRUE)
    # Set debug flag
    sits_env[["debug_flag"]] <- flag
    # Set output_dir
    sits_env[["output_dir"]] <- output_dir
    return(invisible(flag))
}
