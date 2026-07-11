#' @title Debug log functions
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' Logs debug events to a CSV file.
#'
#' The log contains the following columns:
#' * date_time: event date and time
#' * timestamp: event timestamp, as seconds since the Unix epoch
#' * pid: process identifier
#' * event: event name
#' * elapsed_since_last: elapsed time, in seconds, since the last log call in
#'   the same process
#' * mem_used: session used memory, in MB
#' * max_mem_used: maximum memory used, in MB, since the first log call
#' * key: key describing the logged value
#' * value: value associated with the key
#'
#' Each event is logged as one row. The log file name is based on the current
#' session's temporary directory.
#'
#' `elapsed_since_last` is not the duration of the event. It is the time since
#' the previous call to `.debug_log()` in the same process. Durations of code
#' blocks should be computed by pairing `start_*` and `end_*` events in the
#' post-processing step.
#'
#' @param event   Event name.
#' @param key     Key describing the logged value.
#' @param value   Value to be logged. It is converted to string and escaped.
#' @param memory  Logical value indicating whether memory usage should be
#'                collected with `gc()`.
#' @return Invisibly returns `NULL`.
.debug_log <- function(event = "",
                       key = "",
                       value = "",
                       memory = TRUE) {
    # Exit if debug is disabled
    if (!.debug()) {
        return(invisible(NULL))
    }

    # Get output directory
    output_dir <- sits_env[["output_dir"]]
    if (is.null(output_dir)) {
        return(invisible(NULL))
    }

    # Record event time
    time <- Sys.time()
    timestamp <- as.numeric(time)

    # Update last log time on exit
    on.exit(sits_env[["log_time"]] <- time, add = TRUE)

    # Escape CSV values
    esc <- function(value) {
        # Convert value to one scalar string for one CSV field.
        # Character values are already strings and must not be deparsed,
        # otherwise literal quotes are written into the CSV field.
        if (is.character(value)) {
            value <- paste(value, collapse = " ")
        } else {
            value <- paste(deparse(value, width.cutoff = 500L), collapse = " ")
        }

        # Avoid very large CSV fields
        if (nchar(value, type = "chars") > 500L) {
            value <- paste0(substr(value, 1L, 500L), "...<truncated>")
        }

        # Escape double quotes inside the field
        value <- gsub("\"", "\"\"", value)

        # Quote the field when required by CSV
        if (grepl("[\",\n\r]", value)) {
            value <- paste0('"', value, '"')
        }

        value
    }

    # Output log file
    log_file <- .file_log_name(output_dir)

    # Elapsed time since the previous log call in the same process
    first_log <- !.has(sits_env[["log_time"]])
    elapsed_since_last <- NA_real_

    if (!first_log) {
        elapsed_since_last <- as.numeric(difftime(
            time1 = time,
            time2 = sits_env[["log_time"]],
            units = "secs"
        ))
    }

    # Memory information
    if (isTRUE(memory)) {
        if (first_log) {
            mem <- gc(reset = TRUE)
        } else {
            mem <- gc()
        }

        mem_used <- sum(mem[, 2L])
        max_mem_used <- sum(mem[, 6L])
    } else {
        mem_used <- NA_real_
        max_mem_used <- NA_real_
    }

    # Add log header once
    if (!file.exists(log_file)) {
        cat(paste0(paste(
            "date_time", "timestamp", "pid", "event",
            "elapsed_since_last", "mem_used", "max_mem_used",
            "key", "value",
            sep = ", "
        ), "\n"), file = log_file, append = TRUE)
    }

    # Log entry
    cat(paste0(paste(
        esc(format(time, "%Y-%m-%d %H:%M:%OS6")),
        format(timestamp, digits = 15L),
        Sys.getpid(),
        esc(event[[1L]]),
        format(elapsed_since_last, digits = 4L),
        format(mem_used, digits = 4L),
        format(max_mem_used, digits = 4L),
        esc(key[[1L]]),
        esc(value),
        sep = ", "
    ), "\n"), file = log_file, append = TRUE)

    return(invisible(NULL))
}


#' @title Set or get debug mode
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' Gets or sets the debug flag used by `.debug_log()`.
#'
#' When called without arguments, returns the current debug flag. When `flag`
#' is provided, sets the debug flag and the output directory used for writing
#' debug logs.
#'
#' @param flag  Logical value used to set debug mode.
#' @param output_dir  Directory where debug logs are written.
#' @return The current debug flag.
.debug <- function(flag = NULL, output_dir = NULL) {
    .check_set_caller(".debug")

    # Return current debug flag
    if (is.null(flag)) {
        flag <- sits_env[["debug_flag"]]

        # Default to FALSE
        if (is.null(flag)) {
            flag <- FALSE
            sits_env[["debug_flag"]] <- flag
        }

        return(flag)
    }

    .check_lgl_parameter(flag, allow_null = TRUE)

    # Set debug flag
    sits_env[["debug_flag"]] <- flag

    # Set output directory
    sits_env[["output_dir"]] <- output_dir

    return(invisible(flag))
}
