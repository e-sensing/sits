
#' @title sits log functions
#' @name .sits_debug_log
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description logs to a CSV file the following values:
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
#' The log file name will be the same as the base name of the current
#' session's temporary directory.
#'
#'
#' @param flag         A logical value to set the debug flag
#' @param event        The name of the event to be logged
#' @param key          A key describing the value.
#' @param value        Any value to be logged. The value will be converted
#'                     to string and escaped.
#' @param output_dir   directory to save logs.
#'
#' @return             A logical value with current debug flag
.sits_debug_log <- function(event = "", key = "", value = "") {

    # if debug flag is FALSE, then exit
    if (!.sits_debug()) {
        return(invisible(NULL))
    }
    # Get output_dir
    output_dir <- sits_env$output_dir
    if (is.null(output_dir)) {
        return(invisible(NULL))
    }

    # record time to compute elapsed time
    time <- Sys.time()
    on.exit(
        {
            # save the last system time on exit
            sits_env$log_time <- Sys.time()
        },
        add = TRUE
    )

    # function to escape CSV values
    esc <- function(value) {
        value <- gsub("\"", "\"\"", paste0(value))
        if (grepl("[\",\n\r]", value)) {
            return(paste0('"', value, '"'))
        }
        value
    }

    # output log file
    log_file <- paste0(file.path(output_dir, basename(tempdir())), ".log")

    # elapsed time
    elapsed_time <- NULL

    if (!purrr::is_null(sits_env$log_time)) {
        elapsed_time <- format(
            difftime(
                time1 = time,
                time2 = sits_env$log_time,
                units = "secs"
            )[[1]],
            digits = 4
        )
    }

    # add log header once
    if (purrr::is_null(elapsed_time)) {

        # first call to gc
        mem <- gc(reset = TRUE)

        # columns
        cat(paste0(
            paste("date_time", "pid", "event", "elapsed_time",
                "mem_used", "max_mem_used", "key", "value",
                sep = ", "
            ),
            "\n"
        ), file = log_file, append = TRUE)
    } else {
        # memory information
        mem <- gc()
    }

    # log entry
    cat(paste0(
        paste(esc(time), Sys.getpid(), esc(event[[1]]), elapsed_time,
            sum(mem[, 2]), sum(mem[, 6]), esc(key[[1]]), esc(list(value)),
            sep = ", "
        ),
        "\n"
    ), file = log_file, append = TRUE)

    return(invisible(NULL))
}

#' @title sits debug log functions
#' @name .sits_debug
#' @keywords internal
#' @noRd
#' @description  When called without parameters retrieves the
#' current debug flag value. Flag sets the current flag
#' and output_dir writes the debug info to a directory.
#'
#' @param flag         A logical value to set the debug flag
#' @param output_dir   Directory to write the debug info.
#' @return  flag associated to the debug
.sits_debug <- function(flag = NULL, output_dir = NULL) {

    # set caller to show in errors
    .check_set_caller(".sits_debug")

    # if no parameter is passed get current debug flag
    if (purrr::is_null(flag)) {
        flag <- sits_env$debug_flag

        # defaults to FALSE
        if (purrr::is_null(flag)) {
            flag <- FALSE
            sits_env$debug_flag <- flag
        }

        return(flag)
    }

    .check_lgl(
        x = flag, allow_null = TRUE,
        msg = "flag must be a logical value"
    )

    # set debug flag
    sits_env$debug_flag <- flag

    # set output_dir
    sits_env$output_dir <- output_dir

    return(invisible(flag))
}
