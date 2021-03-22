#' @title Estimate the processing time of a task
#' @name .sits_processing_task_time
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function normalizes image values.
#'
#' @param  task           Description of the task
#' @param  start_time     Initial processing time.

#' @return message about processing time for a task
.sits_processing_task_time <- function(task, start_time) {
    # compute current time
    current_time <- lubridate::now()

    # compute elapsed time and estimates remaining time
    elapsed_time <- lubridate::time_length(current_time - start_time,
        unit = "minute"
    )
    message(paste0(
        "Elapsed time for task ", task, " : ",
        round(elapsed_time, digits = 2), " minutes"
    ))
    return(invisible(TRUE))
}
