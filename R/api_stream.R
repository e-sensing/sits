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
#' Creates the shared worker pool used by both the read and the write
#' stages of the streaming pipeline.
#'
#' When the persistent cluster created by \code{sits_parallel()} is open,
#' the backend attaches to it (\code{siphon::parallel_backend(cluster =)}):
#' siphon never creates, replaces, or stops its nodes, and sits keeps
#' ownership. \code{.parallel_start()} already replayed library paths,
#' \code{AWS_*} variables and the debug flag on those workers; here only
#' the sits namespace load and the (possibly different) debug flag are
#' replayed. A fresh adapter is created per call — attaching is ~free and
#' keeps siphon's recorded setup from growing across calls.
#'
#' Otherwise a private pool with \code{multicores + 1} workers is created:
#' \code{multicores} read slots plus one near-idle writer slot (writes are
#' two orders of magnitude cheaper than reads). Worker bootstrap mirrors
#' \code{.parallel_start()}: library paths, \code{AWS_*} environment
#' variables and the debug flag are replayed on every worker (and
#' automatically on any replacement node created by siphon after a worker
#' failure). All worker-side expressions must reference sits internals via
#' \code{sits:::} because siphon evaluates them in the workers' global
#' environment.
#'
#' The sits namespace is loaded at creation so worker warm-up stays off
#' the pipeline's critical path. The model closure is deliberately NOT
#' exported: in the streaming pipeline the model lives only in the main
#' process.
#' @param multicores Number of read workers (the writer slot is added).
#' @param log        Logical: enable .debug() logging on workers.
#' @param output_dir Directory for debug logs.
#' @return A siphon parallel backend.
.stream_backend_start <- function(multicores, log = FALSE, output_dir = NULL) {
    # set debug flag in the main process (mirrors .parallel_start)
    .debug(flag = log, output_dir = output_dir)
    if (.parallel_is_open()) {
        bk <- siphon::parallel_backend(cluster = sits_env[["cluster"]])
    } else {
        bk <- siphon::parallel_backend(multicores + 1L)
        # make sure library paths are the same as the current environment
        lib_paths <- .libPaths()
        siphon::parallel_setup_workers(bk, .libPaths({{ lib_paths }}))
        # it is necessary to export the keys from aws to access the
        # request payer cubes
        env_vars <- as.list(Sys.getenv())
        env_vars <- env_vars[grepl(pattern = "^AWS_", names(env_vars))]
        if (.has(env_vars)) {
            siphon::parallel_setup_workers(
                bk, do.call(Sys.setenv, {{ env_vars }})
            )
        }
    }
    # load sits (and its dependencies) at creation, not lazily on first job
    siphon::parallel_setup_workers(bk, suppressMessages(loadNamespace("sits")))
    # export debug flag; siphon evaluates setup expressions in the worker
    # globalenv, where sits internals are not visible by name, so .debug is
    # shipped by value ({{ }} injection): its closure environment
    # serializes as a namespace reference and resolves in the worker's
    # installed sits
    debug_fn <- .debug
    siphon::parallel_setup_workers(
        bk, {{ debug_fn }}(flag = {{ log }}, output_dir = {{ output_dir }})
    )
    bk
}
#' @title Stop a siphon backend
#' @name .stream_backend_stop
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description
#' Stops an owned backend created by \code{.stream_backend_start()}. An
#' attached backend is never stopped (sits owns the cluster); instead,
#' nodes left busy by an aborted run are repaired. siphon quiesces its
#' backends on abort (draining in-flight results within
#' \code{options(siphon.quiesce_timeout =)}), so a node still busy here
#' either died mid-job or outlived that budget: its socket may hold (or
#' later receive) an orphaned result that would corrupt the next
#' \code{.parallel_map()} on the cluster. Replacing the node makes the
#' cluster safe again; killed read/write jobs are harmless because the
#' stage functions are idempotent (\code{.raster_is_valid()} check-first).
#' @param bk A siphon parallel backend (or NULL).
#' @return No value, called for side effect.
.stream_backend_stop <- function(bk) {
    if (.has_not(bk)) {
        return(invisible(NULL))
    }
    if (isTRUE(bk[["owned"]])) {
        siphon::parallel_stop(bk, force = TRUE)
        return(invisible(NULL))
    }
    for (worker_id in which(siphon::parallel_busy(bk))) {
        .parallel_reset_node(worker_id)
    }
    invisible(NULL)
}
#' @title Number of read-stage workers for a stream backend
#' @name .stream_read_workers
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description
#' The read and write stages share one worker pool and the sum of their
#' \code{max_workers} must not exceed the pool size (siphon dispatch
#' invariant). The owned pool is created with \code{multicores + 1} slots,
#' so reads get \code{multicores}. An attached sits cluster has a size
#' fixed by \code{sits_parallel(workers =)}, so reads are clamped to leave
#' the write slot free.
#' @param bk         A siphon parallel backend.
#' @param multicores Number of read workers requested.
#' @return Integer number of read slots.
.stream_read_workers <- function(bk, multicores) {
    if (isTRUE(bk[["owned"]])) {
        return(multicores)
    }
    max(1L, min(multicores, siphon::parallel_workers(bk) - 1L))
}
