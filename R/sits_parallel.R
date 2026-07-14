#' @title Configure or query sits parallel processing
#'
#' @name sits_parallel
#'
#' @description
#' \code{sits_parallel} starts, restarts, stops, or gets a persistent
#' \code{PSOCK} cluster used by \code{sits} functions that support
#' parallel processing.
#'
#' To stop the cluster and free resources, call
#' \code{sits_parallel(workers = 0)} (recommended). Calling with
#' \code{workers = 1} has the same effect (parallel disabled).
#'
#' When called with no arguments, \code{sits_parallel()} returns the
#' current cluster object. If no cluster is active, it returns
#' \code{NULL}.
#'
#' @param workers    Number of workers to use (integer).
#'   \itemize{
#'     \item \code{workers >= 2}: start or restart a \code{PSOCK}
#'       cluster with \code{workers} workers.
#'     \item \code{workers <= 1}: stop any active cluster.
#'   }
#' @param log        Logical. If \code{TRUE}, enables worker
#'   log/debug mode.
#' @param output_dir Output directory where log files are written when
#'   \code{log = TRUE}.
#'
#' @return
#' If called with no arguments, returns the current \code{parallel}
#' cluster object or \code{NULL} if no cluster is active.
#'
#' If called with \code{workers}, returns \code{invisible(NULL)}.
#'
#' @note
#' This function is intended for long pipelines and production
#' environments where repeatedly creating and stopping clusters
#' inside each \code{sits} call is expensive. After
#' \code{sits_parallel(workers = N)} is called, \code{sits} functions
#' can reuse the same cluster across multiple calls.
#'
#' When \code{workers >= 2}, worker processes inherit the current
#' library paths (\code{.libPaths()}) and selected environment
#' variables required for data access (for example, variables
#' starting with \code{AWS_}).
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Start a persistent cluster
#'     sits_parallel(workers = 32)
#'
#'     # Query the current cluster
#'     cl <- sits_parallel()
#'     cl
#'
#'     # Stop the cluster (recommended)
#'     sits_parallel(workers = 0)
#'
#'     # Alternative stop (also disables parallel)
#'     sits_parallel(workers = 1)
#' }
#' @export
sits_parallel <- function(workers,
                          log = FALSE,
                          output_dir = NULL) {
    # set caller for error messages
    .check_set_caller("sits_parallel")

    # getter: return current cluster (or NULL)
    if (missing(workers)) {
        return(sits_env[["cluster"]])
    }

    # Pre-conditions
    .check_int_parameter(workers, min = 0L, max = 2048L)
    .check_lgl_parameter(log)

    # output_dir is only required when log is TRUE
    if (log) {
        .check_output_dir(output_dir)
    }

    # Stop cluster (workers <= 1)
    if (workers <= 1L) {
        if (.parallel_is_open()) {
            if (workers == 0L) {
                message(.conf("messages", ".parallel_stop_0"))
            } else {
                message(.conf("messages", ".parallel_stop_1"))
            }
        }
        .parallel_stop(TRUE)
        return(invisible(NULL))
    }

    # Start or restart cluster (workers >= 2)
    if (.parallel_is_open()) {
        message(sprintf(
            .conf("messages", ".parallel_restart"),
            workers
        ))
        .parallel_stop(TRUE)
    } else {
        message(sprintf(
            .conf("messages", ".parallel_start"),
            workers
        ))
    }

    .parallel_start(
        workers = workers,
        log = log,
        output_dir = output_dir
    )

    # Finalize cluster at the end of session
    reg.finalizer(sits_env, function(x) {
        if (!is.null(x[["cluster"]])) {
            parallel::stopCluster(x[["cluster"]])
            x[["cluster"]] <- NULL
        }
    }, TRUE)

    invisible(NULL)
}
