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
                    sits_env[["cluster_setup"]] <- NULL
                    sits_env[["cluster_exports"]] <- NULL
                }
            )
        }
    } else if (.parallel_is_open() && .has(cleanup_vars)) {
        # cleanup applies to cluster workers only; sequential processing
        # exports nothing (see .parallel_start)
        exports <- sits_env[["cluster_exports"]]
        if (is.environment(exports)) {
            rm(list = intersect(cleanup_vars, ls(exports)), envir = exports)
        }
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
.parallel_export_sources <- function(cl = sits_env[["cluster"]]) {
    # get the sources defined during the session
    sources <- .conf_sources_session()
    # verify if there are workers and sources
    if (length(cl) == 0L || .has_not(sources)) {
        return(invisible(NULL))
    }
    # if so, export sources to cluster workers
    parallel::clusterCall(
        cl = cl,
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
        if (.parallel_is_open()) {
            .parallel_export_sources()
            .parallel_export(export_vars, envir = parent.frame())
        }
        return(FALSE)
    }
    sits_env[["cluster"]] <- parallel::makePSOCKcluster(workers)
    # kept to set up a node that replaces a dead one (.parallel_reset_node);
    # saved now, so that the new node equals the others
    sits_env[["cluster_setup"]] <- list(
        log = log,
        output_dir = output_dir,
        lib_paths = .libPaths(),
        env_vars = .parallel_env_vars()
    )
    sits_env[["cluster_exports"]] <- new.env(parent = emptyenv())
    .parallel_setup_nodes(sits_env[["cluster"]])
    .parallel_export(export_vars, envir = parent.frame())
    TRUE
}
#' @title Set up the nodes of a cluster as the sits cluster
#' @name .parallel_setup_nodes
#' @keywords internal
#' @noRd
#' @param cl  Cluster whose nodes are set up: the new sits cluster, or a
#'   cluster of one node that replaces a dead one
#' @return No value, called for side effect.
#'
.parallel_setup_nodes <- function(cl) {
    setup <- sits_env[["cluster_setup"]]
    lib_paths <- setup[["lib_paths"]]
    env_vars <- setup[["env_vars"]]
    parallel::clusterExport(
        cl = cl,
        varlist = c("lib_paths", "env_vars"),
        envir = environment()
    )
    # Set .libPaths
    parallel::clusterEvalQ(cl = cl, expr = .libPaths(lib_paths))
    # Set environment variables
    parallel::clusterEvalQ(cl = cl, expr = do.call(Sys.setenv, env_vars))
    # Do not allow torch run with multiple threads
    if (.torch_is_functional()) {
        parallel::clusterEvalQ(cl = cl, expr = torch::torch_set_num_threads(1L))
    }
    # export debug flag; .debug is passed by value (its namespace resolves
    # on the worker at deserialization), avoiding a sits::: self-reference
    parallel::clusterCall(
        cl = cl,
        fun = .debug,
        flag = setup[["log"]],
        output_dir = setup[["output_dir"]]
    )
    # workers build their own configuration when sits is loaded: sources
    # defined during the session must also be included in them
    .parallel_export_sources(cl)
    # variables exported so far (a new cluster has none yet)
    exports <- sits_env[["cluster_exports"]]
    if (is.environment(exports) && .has(ls(exports))) {
        parallel::clusterExport(cl = cl, varlist = ls(exports), envir = exports)
    }
    invisible(NULL)
}
#' @title Environment variables of the sits cluster
#' @name .parallel_env_vars
#' @keywords internal
#' @noRd
#' @return Named list of the variables that workers need, such as access
#'   keys and GDAL settings.
#'
.parallel_env_vars <- function() {
    env_vars <- Sys.getenv()
    var_prefix <- "^(SITS_|AWS_|GDAL_|TERRASCOPE_)"
    env_vars <- as.list(env_vars[grepl(var_prefix, names(env_vars))])
    # workers must not download torch
    c(env_vars, list(TORCH_INSTALL = "0"))
}
#' @title Export variables to the sits cluster
#' @name .parallel_export
#' @keywords internal
#' @noRd
#' @param export_vars  Names of the variables to export
#' @param envir        Environment where the variables are found
#' @return No value, called for side effect.
#' @description The values are also kept in sits_env, because the caller's
#'   frame no longer exists when a dead node is replaced.
#'
.parallel_export <- function(export_vars, envir) {
    if (.has_not(export_vars)) {
        return(invisible(NULL))
    }
    exports <- sits_env[["cluster_exports"]]
    if (!is.environment(exports)) {
        exports <- new.env(parent = emptyenv())
        sits_env[["cluster_exports"]] <- exports
    }
    list2env(mget(export_vars, envir = envir, inherits = TRUE), envir = exports)
    parallel::clusterExport(
        cl = sits_env[["cluster"]],
        varlist = export_vars,
        envir = exports
    )
    invisible(NULL)
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

    # create a new node with the setup of the other nodes
    node <- parallel::makePSOCKcluster(1L)
    .parallel_setup_nodes(node)
    sits_env[["cluster"]][[worker_id]] <- node[[1L]]
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
                # the shape of an unserialized message, wrapped once below;
                # the caller gives it the job of the dead node
                return(list(
                    value = structure(list(), class = "retry"),
                    tag = NULL
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
    # number of jobs
    n <- length(x)
    # number of workers: the first multicores nodes
    p <- min(multicores, length(cl))
    if (n > 0L && p) {
        # job running in each node, to resend the job of a dead node
        running <- integer(length(cl))
        # function to dispatch a job to a node
        submit <- function(node, job) {
            # read the node from sits_env: a dead node is replaced there
            .parallel_submit(
                node = node, fn = fn, args = c(list(x[[job]]), list(...)),
                tag = job, cl = sits_env[["cluster"]]
            )
            running[[node]] <<- job
        }
        # start initial jobs
        for (i in seq_len(min(n, p))) submit(i, i)
        # prepare result list
        val <- vector("list", n)
        next_job <- min(n, p) + 1L
        received <- 0L
        # retrieve results and start jobs
        while (received < n) {
            # fault tolerant version of parallel:::recvOneResult
            d <- .parallel_recv_one_result()
            node <- d[["node"]]
            tag <- d[["tag"]]
            # a dead node: its job is marked to be sent again by .parallel_map
            retry <- inherits(d[["value"]], "retry")
            if (retry) {
                tag <- running[[node]]
            }
            running[[node]] <- 0L
            # a node that died while idle has no result to count
            if (retry && tag == 0L) {
                next
            }
            received <- received + 1L
            # next job
            if (next_job <= n) {
                submit(node, next_job)
                next_job <- next_job + 1L
            }
            # organize result
            val[tag] <- list(d[["value"]])
            # update progress bar
            if (!is.null(pb) && !retry) {
                utils::setTxtProgressBar(
                    pb = pb,
                    value = utils::getTxtProgressBar(pb) + 1L
                )
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
    # Job i draws from the i-th substream, whatever worker runs it
    rng_exit <- .rng_enter(NULL)
    on.exit(rng_exit(), add = TRUE)
    x <- .rng_jobs(x)
    fn <- .rng_job_fn(fn)
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
#' @title CPUs available to the process
#' @name .parallel_cores
#' @keywords internal
#' @noRd
#' @description Returns the number of CPUs that the process can use.
#'   In a container, R sees every core of the machine, even when only a
#'   few CPUs are allowed. The quota is read in the cgroup files
#'   of the process and of its parents: \code{cpu.max} in cgroup v2, and
#'   \code{cpu.cfs_quota_us} divided by \code{cpu.cfs_period_us} in v1.
#'   The lowest value is rounded down and kept between 1 and the cores.
#'   Without a quota, all cores are returned. Paths come from the config
#'   key \code{cgroup_root}, not from the mount table, so the quota can be
#'   missed, or taken from another cgroup, in three cases: the host tree is
#'   mounted inside a cgroup namespace; the host is hybrid (v2 under
#'   \code{unified}); a v1 host has no \code{cpu} directory.
#' @return An integer.
.parallel_cores <- function() {
    cgroup_root <- .conf("cgroup_root")
    cores <- suppressWarnings(as.integer(parallel::detectCores()))
    if (length(cores) != 1L || is.na(cores) || cores < 1L) {
        cores <- 1L
    }
    paths <- .parallel_cgroup_paths(.conf("cgroup_proc_file"))
    v1_dir <- file.path(cgroup_root, .conf("cgroup_v1_cpu_dir"))
    limits <- c(
        purrr::map_dbl(.parallel_cgroup_levels(paths[["v2"]]), function(lv) {
            .parallel_cgroup_v2_limit(file.path(cgroup_root, lv))
        }),
        purrr::map_dbl(.parallel_cgroup_levels(paths[["v1"]]), function(lv) {
            .parallel_cgroup_v1_limit(file.path(v1_dir, lv))
        })
    )
    limits <- limits[!is.na(limits)]
    if (length(limits) == 0L) {
        return(cores)
    }
    as.integer(max(1L, min(floor(min(limits)), cores)))
}
#' @title Cgroup paths of the process
#' @name .parallel_cgroup_paths
#' @keywords internal
#' @noRd
#' @param proc_cgroup File that lists the cgroups of the process.
#' @return A list with the cgroup v2 path and the v1 cpu path; the root
#'   when a path is unknown, which keeps reading the mount root.
.parallel_cgroup_paths <- function(proc_cgroup) {
    lines <- .parallel_cgroup_read(proc_cgroup)
    # kernel format of each line: hierarchy-id:controller-list:path
    parts <- regmatches(lines, regexec("^([0-9]+):([^:]*):(.*)$", lines))
    parts <- parts[lengths(parts) == 4L]
    is_v2 <- purrr::map_lgl(parts, function(p) p[[2L]] == "0" && p[[3L]] == "")
    is_v1 <- purrr::map_lgl(parts, function(p) {
        .conf("cgroup_v1_cpu_controller") %in%
            strsplit(p[[3L]], ",", fixed = TRUE)[[1L]]
    })
    path_of <- function(found) {
        if (any(found)) parts[[which(found)[[1L]]]][[4L]] else "/"
    }
    list(v2 = path_of(is_v2), v1 = path_of(is_v1 & !is_v2))
}
#' @title Levels of a cgroup path, from the path up to the root
#' @name .parallel_cgroup_levels
#' @keywords internal
#' @noRd
#' @param path A cgroup path such as "/a/b".
#' @return Character vector of relative paths, the root as "".
.parallel_cgroup_levels <- function(path) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    parts <- parts[nzchar(parts)]
    levels <- purrr::map_chr(rev(seq_along(parts)), function(i) {
        paste(parts[seq_len(i)], collapse = "/")
    })
    c(levels, "")
}
#' @title CPU limit of one cgroup v2 level
#' @name .parallel_cgroup_v2_limit
#' @keywords internal
#' @noRd
#' @param dir Directory of the cgroup level.
#' @return CPUs as a number, or NA when the level has no limit.
.parallel_cgroup_v2_limit <- function(dir) {
    line <- .parallel_cgroup_read(file.path(dir, .conf("cgroup_v2_cpu_max")))
    values <- suppressWarnings(
        as.numeric(strsplit(line[1L], " ", fixed = TRUE)[[1L]])
    )
    # "max <period>" gives NA for the quota, which means no limit
    if (length(values) != 2L || anyNA(values) || any(values <= 0)) {
        return(NA_real_)
    }
    values[[1L]] / values[[2L]]
}
#' @title CPU limit of one cgroup v1 level
#' @name .parallel_cgroup_v1_limit
#' @keywords internal
#' @noRd
#' @param dir Directory of the cgroup level in the cpu controller.
#' @return CPUs as a number, or NA when the level has no limit.
.parallel_cgroup_v1_limit <- function(dir) {
    read_num <- function(name) {
        line <- .parallel_cgroup_read(file.path(dir, .conf(name)))
        suppressWarnings(as.numeric(line[1L]))
    }
    quota <- read_num("cgroup_v1_cpu_quota")
    period <- read_num("cgroup_v1_cpu_period")
    # a quota of -1 means no limit
    if (anyNA(c(quota, period)) || quota <= 0 || period <= 0) {
        return(NA_real_)
    }
    quota / period
}
#' @title Read a cgroup file
#' @name .parallel_cgroup_read
#' @keywords internal
#' @noRd
#' @param file Path of the file.
#' @return Its lines; NA when the file is missing, empty or unreadable.
.parallel_cgroup_read <- function(file) {
    lines <- tryCatch(
        suppressWarnings(readLines(file, warn = FALSE)),
        error = function(e) character(0L)
    )
    if (length(lines) == 0L) NA_character_ else lines
}
