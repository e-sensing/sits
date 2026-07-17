#' @title Is the streaming GPU pipeline enabled?
#' @name .stream_enabled
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description
#' The streaming GPU pipeline overlaps parallel chunk reads with sequential
#' GPU inference and parallel writes using pull-based stages from the
#' \code{siphon} package. It is opt-in via the environment variable
#' \code{SITS_GPU_PIPELINE=stream} and requires the suggested package
#' \code{siphon}. The default is the staged (grouped) pipeline.
#' @return TRUE/FALSE
.stream_enabled <- function() {
    identical(Sys.getenv("SITS_GPU_PIPELINE", unset = "staged"), "stream") &&
        requireNamespace("siphon", quietly = TRUE)
}
#' @title Read-stage buffer size for the streaming pipeline
#' @name .stream_buffer_size
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description
#' Number of completed-but-not-yet-inferred chunks buffered in the main
#' session. Together with the read stage \code{max_workers} this bounds
#' memory: the main session holds at most \code{buffer_size} buffered reads,
#' plus one chunk in inference, plus one retained by the write slot; workers
#' hold \code{multicores} in-flight reads. The staged pipeline holds
#' \code{multicores} chunks in the main session at once, so a buffer of 2
#' keeps the main-session peak at or below the staged pipeline for
#' \code{multicores >= 4}.
#' @return Integer buffer size
.stream_buffer_size <- function() {
    2L
}
#' @title Start a siphon backend for the streaming GPU pipeline
#' @name .stream_backend_start
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description
#' Creates one shared worker pool used by both the read and the write
#' stages of the streaming pipeline. The pool has \code{multicores + 1}
#' workers: \code{multicores} read slots plus one near-idle writer slot
#' (writes are two orders of magnitude cheaper than reads). Stages sharing
#' the pool must keep the sum of their \code{max_workers} within the pool
#' size.
#'
#' Worker bootstrap mirrors \code{.parallel_start()}: library paths,
#' \code{AWS_*} environment variables and the debug flag are replayed on
#' every worker (and automatically on any replacement node created by
#' siphon after a worker failure). The sits namespace is loaded here so
#' that worker warm-up stays off the pipeline's critical path. The model
#' closure is deliberately NOT exported: in the streaming pipeline the
#' model lives only in the main process.
#' @param multicores Number of read workers (the writer slot is added).
#' @param log        Logical: enable .debug() logging on workers.
#' @param output_dir Directory for debug logs.
#' @return A siphon parallel backend.
.stream_backend_start <- function(multicores, log = FALSE, output_dir = NULL) {
    # set debug flag in the main process (mirrors .parallel_start)
    .debug(flag = log, output_dir = output_dir)
    bk <- siphon::parallel_backend(multicores + 1L)
    # make sure library paths are the same as the current environment
    lib_paths <- .libPaths()
    siphon::parallel_setup_workers(bk, .libPaths({{ lib_paths }}))
    # load sits (and its dependencies) at creation, not lazily on first job
    siphon::parallel_setup_workers(bk, suppressMessages(loadNamespace("sits")))
    # it is necessary to export the keys from aws to access the
    # request payer cubes
    env_vars <- as.list(Sys.getenv())
    env_vars <- env_vars[grepl(pattern = "^AWS_", names(env_vars))]
    if (.has(env_vars)) {
        siphon::parallel_setup_workers(bk, do.call(Sys.setenv, {{ env_vars }}))
    }
    # export debug flag
    siphon::parallel_setup_workers(
        bk, .debug(flag = {{ log }}, output_dir = {{ output_dir }})
    )
    bk
}
#' @title Stop a siphon backend
#' @name .stream_backend_stop
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param bk A siphon parallel backend (or NULL).
#' @return No value, called for side effect.
.stream_backend_stop <- function(bk) {
    if (.has(bk)) {
        siphon::parallel_stop(bk, force = TRUE)
    }
    invisible(NULL)
}
