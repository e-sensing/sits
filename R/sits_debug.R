#' Sits log functions
#'
#' Set of functions for log generation.
#'
#' @param flag         A logical value to set the debug flag
#' @param event        The name of the event to be logged
#' @param key          A key describing the value.
#' @param value        Any value to be logged. The value will be converted
#'                     to string and escaped.
#' @param output_dir   directory to save logs.
#'
#' @return             A logical value with current debug flag
#'
#' @name log_functions
#' @keywords internal
#' @noRd
NULL

#' @describeIn log_functions Generate a log entry into a CSV file.
#' @details
#' Log entry is composed of the following values:
#' \itemize{
#' \item date_time: event date and time
#' \item pid: process identifier
#' \item event: event name
#' \item elapsed_time: duration (in seconds) from the last log call
#' \item mem_used: session used memory (in MB)
#' \item max_mem_used: maximum memory used (in MB) from first log call
#' \item tag: any character string to be registered
#' }
#' Each event will be logged in one row in the log file.
#' @noRd
.sits_debug_log <- function(event = "", key = "", value = "") {
    # If debug flag is FALSE, then exit
    if (!.sits_debug()) {
        return(invisible(NULL))
    }
    # Get output_dir
    output_dir <- sits_env$output_dir
    if (is.null(output_dir)) {
        return(invisible(NULL))
    }
    # Record time to compute elapsed time
    time <- Sys.time()
    on.exit(sits_env$log_time <- Sys.time(), add = TRUE)
    # Function to escape CSV values
    esc <- function(value) {
        value <- gsub("\"", "\"\"", paste0(value))
        if (grepl("[\",\n\r]", value)) {
            return(paste0('"', value, '"'))
        }
        value
    }
    # Output log file
    log_file <- paste0(file.path(output_dir, basename(tempdir())), ".log")
    # Elapsed time
    elapsed_time <- NULL
    if (.has(sits_env$log_time)) {
        elapsed_time <- format(difftime(
            time1 = time,
            time2 = sits_env$log_time,
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
            sep = ", "), "\n"
        ), file = log_file, append = TRUE)
    } else {
        # Memory information
        mem <- gc()
    }
    # Log entry
    cat(paste0(paste(
        esc(time), Sys.getpid(), esc(event[[1]]), elapsed_time,
        sum(mem[, 2]), sum(mem[, 6]), esc(key[[1]]), esc(list(value)),
        sep = ", "), "\n"
    ), file = log_file, append = TRUE)

    return(invisible(NULL))
}

#' @describeIn log_functions When called without parameters retrieves the
#'   current debug flag value. The sits write log files when the debug
#'   flag is \code{TRUE}. Returns a \code{logical} informing current
#'   debug flag.
#' @noRd
.sits_debug <- function(flag = NULL, output_dir = NULL) {
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
    .check_lgl(
        x = flag, allow_null = TRUE,
        msg = "flag must be a logical value"
    )
    # Set debug flag
    sits_env[["debug_flag"]] <- flag
    # Set output_dir
    sits_env[["output_dir"]] <- output_dir
    return(invisible(flag))
}
