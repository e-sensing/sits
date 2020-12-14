#' @title Estimate the processing time
#' @name .sits_est_class_time
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function normalizes image values.
#'
#' @param  task           task to be performed
#' @param  start_time     initial processing time.
#' @param  n_intervals    number of time intervals
#' @param  bs             raster block parameters.
#' @param  block          current block.
#' @param  time           current interval.
#' @return Scaled matrix.
.sits_est_class_time <- function(start_time,
                                 n_intervals,
                                 bs,
                                 block,
                                 time) {
    # compute current time
    current_time <- lubridate::now()

    # compute elapsed time and estimates remaining time
    elaps_time <- lubridate::time_length(current_time - start_time,
        unit = "minute"
    )
    elaps_intervals <- (block - 1) * n_intervals + time
    total_intervals <- bs$n * n_intervals
    if (elaps_intervals < total_intervals) {
        message(sprintf(
            "Elapsed time %s minute(s).
         Estimated total process time %s minute(s)...",
            round(as.numeric(elaps_time), 1),
            round(
                as.numeric((total_intervals / elaps_intervals) * elaps_time),
                1
            )
        ))
    } else {
        message(paste0(
            "Classification finished at ", current_time,
            ". Total elapsed time: ",
            round(as.numeric(elaps_time), 1),
            "minute(s)."
        ))
    }
    return(invisible(TRUE))
}

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
