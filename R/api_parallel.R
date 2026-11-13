#' @title Stop all workers of the sits cluster
#' @name .parallel_stop
#' @keywords internal
#' @noRd
#' @param started Was the parallel cluster created by the current routine?
#' @param cleanup_vars Character informing global vars to remove in workers.
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @return No value, called for side effect.
#'
.parallel_stop <- function(started, cleanup_vars = character(0L)) {
    if (started) {
        if (.parallel_is_open()) {
            tryCatch(
                {
                    parallel::stopCluster(sits_env[["cluster"]])
                },
                finally = {
                    sits_env[["cluster"]] <- NULL
                }
            )
        }
    } else if (.parallel_is_open() && .has(cleanup_vars)) {
        # cleanup applies to cluster workers only; sequential processing
        # exports nothing (see .parallel_start)
        eval(
            bquote(
                parallel::clusterEvalQ(
                    cl = sits_env[["cluster"]],
                    expr = rm(list = .(cleanup_vars), envir = globalenv())
                )
            )
        )
    }
}


#' @title Check if sits clusters are open or not
#' @name .parallel_is_open
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @return No value, called for side effect.
#'
.parallel_is_open <- function() {
    n <- length(sits_env[["cluster"]])
    if (n == 0L) {
        return(FALSE)
    }
    socklist <- lapply(seq_len(n), function(i) {
        sits_env[["cluster"]][[i]][["con"]]
    })
    tryCatch(
        any(socketSelect(socklist, write = TRUE)),
        error = function(e) FALSE
    )
}

#' @title Export the sources defined in the session to cluster workers
#' @name .parallel_export_sources
#' @keywords internal
#' @noRd
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @description Workers build their own configuration when sits is loaded, and
#' are not aware of sources defined while sits is running (e.g., data cubes
#' shared on HuggingFace). This function includes those sources in each worker
#' of an open cluster.
#' @return No value, called for side effect.
.parallel_export_sources <- function() {
    # get the sources defined during the session
    sources <- .conf_sources_session()
    # verify if there are workers and sources
    if (!.parallel_is_open() || .has_not(sources)) {
        return(invisible(NULL))
    }
    # if so, export sources to cluster workers
    parallel::clusterCall(
        cl = sits_env[["cluster"]],
        fun = .conf_set_options,
        sources = sources
    )
    # return!
    invisible(NULL)
}
#' @title Start a new sits cluster for parallel processing
#' @name .parallel_start
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param workers    number of cluster to instantiate
#' @param export_vars character vector with variables to export to workers
#' @param log        a logical indicating if log files must be written
#' @param output_dir output_dir where to save logs.
#' @return Logical indicating if a new cluster was created or not.
#'   \code{FALSE} means no change in sits cluster. \code{TRUE} indicates
#'   that a new cluster was created.
#'
.parallel_start <- function(workers,
                            export_vars = character(0L),
                            log = FALSE,
                            output_dir = NULL) {
    .debug(flag = log, output_dir = output_dir)
    if (.parallel_is_open() || workers <= 1L) {
        # export variables to cluster workers only; with workers <= 1 and
        # no cluster, processing is sequential in the main process and
        # there is nothing to export
        .parallel_export_sources()
        if (.parallel_is_open() && .has(export_vars)) {
            parallel::clusterExport(
                cl = sits_env[["cluster"]],
                varlist = export_vars,
                envir = parent.frame()
            )
        }
        return(FALSE)
    }
    sits_env[["cluster"]] <- parallel::makePSOCKcluster(workers)

    # make sure library paths is the same as actual environment
    lib_paths <- .libPaths()
    # it is necessary to export some keys
    env_vars <- Sys.getenv()
    var_prefix <- "^(SITS_|AWS_|GDAL_|TERRASCOPE_)"
    env_vars <- as.list(env_vars[grepl(var_prefix, names(env_vars))])
    env_vars <- c(env_vars, list(TORCH_INSTALL = "0"))

    parallel::clusterExport(
        cl = sits_env[["cluster"]],
        varlist = c("lib_paths", "env_vars"),
        envir = environment()
    )
    # Set .libPaths
    parallel::clusterEvalQ(
        cl = sits_env[["cluster"]],
        expr = .libPaths(lib_paths)
    )
    # Set environment variables
    parallel::clusterEvalQ(
        cl = sits_env[["cluster"]],
        expr = do.call(Sys.setenv, env_vars)
    )
    # Do not allow torch run with multiple threads
    if (.torch_is_functional()) {
        parallel::clusterEvalQ(
            cl = sits_env[["cluster"]],
            expr = torch::torch_set_num_threads(1L)
        )
    }
    # export debug flag; .debug is passed by value (its namespace resolves
    # on the worker at deserialization), avoiding a sits::: self-reference
    parallel::clusterCall(
        cl = sits_env[["cluster"]],
        fun = .debug,
        flag = log,
        output_dir = output_dir
    )
    # workers build their own configuration when sits is loaded: sources
    # defined during the session must also be included in them
    .parallel_export_sources()
    # export export_list
    if (.has(export_vars)) {
        parallel::clusterExport(
            cl = sits_env[["cluster"]],
            varlist = export_vars,
            envir = parent.frame()
        )
    }
    TRUE
}
#' @title Recreates a cluster worker
#' @name .parallel_reset_node
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param worker_id   id of the cluster work to be recreated
#' @return No value, called for side effect.
#'
.parallel_reset_node <- function(worker_id) {
    # stop node
    tryCatch({
        if (isOpen(sits_env[["cluster"]][[worker_id]][["con"]])) {
            close(sits_env[["cluster"]][[worker_id]][["con"]])
        }
    })

    # create a new node
    sits_env[["cluster"]][[worker_id]] <- parallel::makePSOCKcluster(1L)[[1L]]
}

#' @name .parallel_recv_one_data
#' @keywords internal
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' These internal functions are a reimplementation of a fault tolerant
#' version of snow package functions \code{recv_one_data()},
#' \code{recv_one_result()}, and \code{cluster_apply()}
#' from Luke Tierney, A. J. Rossini, Na Li, H. Sevcikova.
#' snow package is licensed as GPL-2 | GPL-3.
#' This re-implementation allows `sits` cope with massive volume of data
#' processing over networks without compromise overall results.
#'
#' @param x     a given list to be passed to a function
#' @param fn    a function to be applied to each list element
#' @param ...   additional arguments to fn function
#' @return      List with IDs of workers and their values
.parallel_recv_one_data <- function() {
    # fault tolerant version of parallel:::recvOneData
    cl <- sits_env[["cluster"]]

    # get connections
    socklist <- lapply(cl, function(x) x[["con"]])

    # wait for data in socket
    repeat {
        ready <- socketSelect(socklist)
        if (any(ready)) break
    }
    # which cluster is going to be read
    worker_id <- which.max(ready)

    # fault tolerance change
    value <- tryCatch(
        {
            unserialize(socklist[[worker_id]])
        },
        error = function(e) {
            # catch only errors in connection
            if (grepl("error reading from connection", e[["message"]])) {
                msg <- .conf("messages", ".parallel_recv_one_data")
                message(msg)
                # reset node
                .parallel_reset_node(worker_id)

                return(list(
                    node = worker_id,
                    value = list(
                        value = structure(list(), class = "retry"),
                        tag = NULL
                    )
                ))
            }
            # otherwise rise error
            stop(e)
        }
    )

    list(node = worker_id, value = value)
}

#' @name .parallel_recv_one_result
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @return      List with values and nodes
.parallel_recv_one_result <- function() {
    # fault tolerant version of parallel:::recvOneData
    v <- .parallel_recv_one_data()

    list(
        value = v[["value"]][["value"]],
        node = v[["node"]],
        tag = v[["value"]][["tag"]]
    )
}
#' @name .parallel_check_remote_errors
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @return      No value, called for side effect
.parallel_check_remote_errors <- function(val) {
    is_warn <- vapply(val, inherits, logical(1), "warning")
    warns <- unique(val[is_warn])
    if (.has(warns)) {
        for (msg in warns) {
            warning(msg, call. = FALSE)
        }
    }

    is_err <- vapply(val, inherits, logical(1), "try-error")
    if (!any(is_err)) {
        return(val[!is_warn])
    }

    msgs <- unique(vapply(val[is_err], as.character, character(1)))

    # Emit warnings for remaining errors
    if (length(msgs) > 1) {
        for (msg in msgs[-1]) {
            warning(msg, call. = FALSE)
        }
    }

    # Stop for the first error
    stop(sprintf(
        .conf("messages", ".parallel_remote_errors"), length(msgs), msgs[[1]]
    ))
}
#' @title Send a job to one worker of the sits cluster
#' @name .parallel_submit
#' @keywords internal
#' @noRd
#' @description Sends \code{fn(args)} to one worker and does not wait for
#' the result; \code{.parallel_poll()} receives it, and \code{tag}
#' identifies the job. The message has the format that a worker of the
#' \code{parallel} package reads.
#' @param node  Index of the worker in \code{cl}.
#' @param fn    Function to be run by the worker.
#' @param args  List of arguments of \code{fn}.
#' @param tag   Value returned with the result, to identify the job.
#' @param cl    Cluster of the worker.
#' @return      No value, called for side effect.
.parallel_submit <- function(node, fn, args, tag,
                             cl = sits_env[["cluster"]]) {
    worker <- cl[[node]]
    message <- list(
        type = "EXEC",
        data = list(fun = fn, args = args, return = TRUE, tag = tag),
        tag = NULL
    )
    # a cluster made with useXDR = FALSE has nodes of class SOCK0node
    serialize(
        message, worker[["con"]], xdr = !inherits(worker, "SOCK0node")
    )
    invisible(NULL)
}
#' @title Take the result of one job of the sits cluster
#' @name .parallel_poll
#' @keywords internal
#' @noRd
#' @description Waits for the result of a job sent by
#' \code{.parallel_submit()}. With a \code{timeout}, it waits at most that
#' many seconds and returns \code{NULL} when no result has arrived.
#' @param timeout  Seconds to wait, or \code{NULL} to wait for a result.
#' @return  List with the value, the node and the tag of the job, or NULL.
.parallel_poll <- function(timeout = NULL) {
    if (.has(timeout)) {
        socklist <- lapply(sits_env[["cluster"]], function(x) x[["con"]])
        if (!any(socketSelect(socklist, timeout = timeout))) {
            return(NULL)
        }
    }
    .parallel_recv_one_result()
}
#' @rdname .parallel_cluster_apply
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @return      No value, called for side effect.
.parallel_cluster_apply <- function(x, fn, ..., pb = NULL) {
    # fault tolerant version of parallel::clusterApplyLB
    cl <- sits_env[["cluster"]]
    multicores <- max(1L, min(sits_env[["forced_multicores"]], length(cl)))
    if (multicores != length(cl)) cl <- cl[multicores]
    # number of jobs
    n <- length(x)
    # number of workers
    p <- length(cl)
    if (n > 0L && p) {
        # function to dispatch a job to a node
        submit <- function(node, job) {
            .parallel_submit(
                node = node, fn = fn, args = c(list(x[[job]]), list(...)),
                tag = job, cl = cl
            )
        }
        # start initial jobs
        for (i in seq_len(min(n, p))) submit(i, i)
        # prepare result list
        val <- vector("list", n)
        # retrieve results and start jobs
        for (i in seq_len(n)) {
            # fault tolerant version of parallel:::recvOneResult
            d <- .parallel_recv_one_result()
            # next job
            j <- i + min(n, p)
            if (j <= n) {
                submit(d[["node"]], j)
            }
            # organize result
            if (.has(d[["tag"]])) {
                val[d[["tag"]]] <- list(d[["value"]])
                # update progress bar
                if (!is.null(pb)) {
                    utils::setTxtProgressBar(
                        pb = pb,
                        value = utils::getTxtProgressBar(pb) + 1L
                    )
                }
            }
        }
        # process errors
        .parallel_check_remote_errors(val)
    }
}

#' @title Maps a function to a given list in parallel or sequential processing
#' @name .parallel_map
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param x              List to be passed to a function.
#' @param fn             Function to be applied to each list element.
#' @param progress       Show progress bar?
#' @param n_retries      Number of retries before fail.
#' @param sleep          Number in seconds to wait before trying again.
#'
#' @return               List with the function results in the same order
#'                       as the input list
#'
.parallel_map <- function(x, fn, ..., progress = FALSE,
                          n_retries = 3L, sleep = 0L) {
    # check documentation mode
    progress <- .message_progress(progress)
    # create progress bar
    pb <- NULL
    if (progress) {
        pb <- utils::txtProgressBar(min = 0L, max = length(x), style = 3L)
    }
    # sequential processing
    if (!.parallel_is_open()) {
        result <- lapply(seq_along(x), function(i) {
            value <- fn(x[[i]], ...)
            # update progress bar
            if (progress) {
                utils::setTxtProgressBar(
                    pb = pb,
                    value = utils::getTxtProgressBar(pb) + 1L
                )
            }
            value
        })
        # close progress bar
        if (progress) {
            close(pb)
        }
        return(result)
    }
    # parallel processing
    values <- .parallel_cluster_apply(x, fn, ..., pb = pb)

    # check for faults
    retry <- vapply(values, inherits, logical(1L), "retry")

    # is there any node to be recovered?
    if (any(retry)) {
        # try three times
        for (i in seq_len(n_retries)) {
            # seconds to wait before try again
            Sys.sleep(sleep)
            # retry for faulted values
            values[retry] <- .parallel_cluster_apply(
                x[retry], fn, ...,
                pb = pb
            )
            # check for faults again
            retry <- vapply(values, inherits, logical(1L), "retry")
            if (!any(retry)) break
        }
        if (any(retry)) {
            stop(.conf("messages", ".parallel_map"),
                call. = FALSE
            )
        }
    }
    # close progress bar
    if (progress) {
        close(pb)
    }
    return(values)
}
#' @title Run a queue of jobs with dependencies in the sits cluster
#' @name .parallel_queue
#' @keywords internal
#' @noRd
#' @description Sends each job to a free worker as soon as the jobs it
#' waits for are done. Among the ready jobs, those marked \code{first} are
#' sent before the others; within each group the lowest index goes first.
#' Without a cluster this process runs the jobs one at a time, the lowest
#' ready index first, since there is no other worker to free.
#' @param jobs      List of jobs; \code{fn(jobs[[k]], ...)} runs in a worker.
#' @param fn        Function to be applied to each job.
#' @param ...       Additional arguments to \code{fn}.
#' @param after     List as long as \code{jobs}; \code{after[[k]]} holds the
#'                  indices of the jobs that must be done before job \code{k}
#'                  is sent. NULL means no dependencies.
#' @param first     Logical vector as long as \code{jobs}, or NULL.
#' @param on_send   Function of \code{k}, run in this process when job
#'                  \code{k} is sent, or NULL.
#' @param on_done   Function of \code{k} and the value of job \code{k}, run
#'                  in this process when its result arrives, or NULL.
#' @param progress  Show progress bar?
#' @return List with the values of the jobs, in the order of \code{jobs}.
.parallel_queue <- function(jobs, fn, ..., after = NULL, first = NULL,
                            on_send = NULL, on_done = NULL,
                            progress = FALSE) {
    n <- length(jobs)
    if (is.null(after)) {
        after <- rep(list(integer(0L)), n)
    }
    if (is.null(first)) {
        first <- rep(FALSE, n)
    }
    .parallel_queue_check(after = after, first = first, n = n)
    after <- purrr::map(after, function(a) unique(as.integer(a)))
    # the jobs that wait for each job, to count the dependencies left
    dependents <- split(
        rep(seq_len(n), lengths(after)),
        factor(unlist(after), levels = seq_len(n))
    )
    waiting <- lengths(after)
    sent <- rep(FALSE, n)
    values <- vector("list", n)
    dots <- list(...)
    progress <- .message_progress(progress)
    pb <- NULL
    if (progress && n > 0L) {
        pb <- utils::txtProgressBar(min = 0L, max = n, style = 3L)
        on.exit(close(pb), add = TRUE)
    }
    cluster <- .parallel_is_open()
    # without a cluster one slot, filled by running the job here
    free <- if (cluster) seq_along(sits_env[["cluster"]]) else 1L
    priority <- if (cluster) first else NULL
    running <- 0L
    ran <- NULL
    failed <- list()
    repeat {
        # after a failure no job is sent; the running ones are drained
        while (length(free) > 0L && length(failed) == 0L) {
            k <- .parallel_queue_next(sent, waiting, priority)
            if (is.na(k)) {
                break
            }
            sent[[k]] <- TRUE
            if (.has(on_send)) {
                on_send(k)
            }
            if (cluster) {
                .parallel_submit(
                    node = free[[1L]], fn = fn,
                    args = c(list(jobs[[k]]), dots), tag = k
                )
            } else {
                ran <- list(value = fn(jobs[[k]], ...), node = 1L, tag = k)
            }
            free <- free[-1L]
            running <- running + 1L
        }
        if (running == 0L) {
            break
        }
        d <- if (cluster) .parallel_poll() else ran
        running <- running - 1L
        free <- c(free, d[["node"]])
        value <- d[["value"]]
        # a dead worker leaves no tag, so its job cannot be sent again
        if (inherits(value, "retry") || inherits(value, "try-error")) {
            failed <- c(failed, list(value))
            next
        }
        k <- d[["tag"]]
        values[k] <- list(value)
        released <- dependents[[k]]
        waiting[released] <- waiting[released] - 1L
        if (.has(on_done)) {
            on_done(k, value)
        }
        if (.has(pb)) {
            utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb) + 1L)
        }
    }
    if (any(purrr::map_lgl(failed, inherits, "retry"))) {
        stop(.conf("messages", ".parallel_map"), call. = FALSE)
    }
    if (length(failed) > 0L) {
        .parallel_check_remote_errors(failed)
    }
    values
}
#' @title Next job to send in a queue of the sits cluster
#' @name .parallel_queue_next
#' @keywords internal
#' @noRd
#' @param sent      Logical vector, TRUE for the jobs already sent.
#' @param waiting   Number of dependencies left of each job.
#' @param first     Logical vector of the jobs that go first, or NULL.
#' @return Index of the job, or NA when no job is ready.
.parallel_queue_next <- function(sent, waiting, first) {
    ready <- which(!sent & waiting == 0L)
    if (.has(first)) {
        ready <- c(ready[first[ready]], ready[!first[ready]])
    }
    if (length(ready) == 0L) {
        return(NA_integer_)
    }
    ready[[1L]]
}
#' @title Check the dependencies of a queue of the sits cluster
#' @name .parallel_queue_check
#' @keywords internal
#' @noRd
#' @param after   List with the indices of the jobs each job waits for.
#' @param first   Logical vector of the jobs that go first.
#' @param n       Number of jobs.
#' @return No value, called for side effect.
.parallel_queue_check <- function(after, first, n) {
    .check_set_caller(".parallel_queue_check")
    .check_that(
        is.logical(first) && length(first) == n && !anyNA(first),
        msg = .conf("messages", ".parallel_queue_check_first")
    )
    from <- unlist(after)
    .check_that(
        is.list(after) && length(after) == n && all(from %in% seq_len(n)),
        msg = .conf("messages", ".parallel_queue_check_range")
    )
    # release the jobs that wait for nothing until none is released; the
    # jobs left wait for each other
    to <- rep(seq_len(n), lengths(after))
    waiting <- tabulate(to, nbins = n)
    left <- rep(TRUE, n)
    repeat {
        free <- left & waiting == 0L
        if (!any(free)) {
            break
        }
        left[free] <- FALSE
        waiting <- waiting - tabulate(to[free[from]], nbins = n)
    }
    .check_that(
        !any(left),
        msg = .conf("messages", ".parallel_queue_check_cycle")
    )
}

.parallel_force_multicores <- function(multicores = NULL) {
    sits_env[["forced_multicores"]] <- multicores
}
