#' @title Log variable content to a file
#' @name .sits_log
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Log to a text file the content of a variable passed to values.
#' Log file name is the same base name of temporary directory for current R
#' session.
#'
#' @param output_dir    Output directory to write log file
#' @param topic         A description to append to time stamp
#' @param ...           Any valid value to be logged
.sits_log <- function(output_dir = tempdir(), topic = "", ...) {

    # if no debug environment variable is set, then exit
    if (Sys.getenv("__SITS_DEBUG__") != TRUE)
        return(invisible(NULL))

    # compute elapsed time from last .sits_log call
    elapsed_time <- .sits_log_elapsed_time()

    # save the last system time on exit
    on.exit({
        sits_env$last_time_sits_log_call <- Sys.time()
    }, add = TRUE)

    # dots
    values <- list(...)

    # output file
    log_file = paste0(output_dir, "/", basename(tempdir()), ".log")

    # head
    time_stamp <- format(x = Sys.time(), format = "[%Y-%m-%dT%H:%M:%S]")
    topic <- paste(time_stamp, paste0(topic, collapse = " "))

    # write head
    cat(paste0(topic, "\n"), file = log_file, append = TRUE)

    # write elapsed time
    if (!purrr::is_null(elapsed_time)) {
        cat("elapsed_time=\n", file = log_file, append = TRUE)
        cat(elapsed_time, file = log_file, append = TRUE)
    }

    # sink output
    sink(file = log_file,
         append = TRUE,
         type = c("output", "message"),
         split = FALSE)

    # values to be appended
    tryCatch({
        for (i in seq_along(values)) {
            cat(paste0(names(values)[[i]], "=\n"), append = TRUE)
            print(values[[i]])
        }
    }, finally = sink()) # revert sink

    # add a blank line
    cat("\n", file = log_file, append = TRUE)

    return(invisible(NULL))
}


#' @title Compute the duration between the last .sits_log call and now
#' @name .sits_log_elapsed_time
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Compute the difference between now and the last call to
#' .sits_log function.
#'
#' @return A character string informing the ellapsed time
.sits_log_elapsed_time <- function() {

    if (purrr::is_null(sits_env$last_time_sits_log_call))
        return(NULL)

    time_elapsed <- difftime(Sys.time(), sits_env$last_time_sits_log_call)

    return(paste(time_elapsed[[1]], attr(time_elapsed, "units")))
}
