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
    env_vars <- env_vars[grepl(var_prefix, names(env_vars))]
    env_vars <- c(env_vars, TORCH_INSTALL = "0")

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
    if (.torch_is_installed()) {
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
            # get hidden object from parallel
            .send_call <- get("sendCall",
                envir = asNamespace("parallel"),
                inherits = FALSE
            )
            .send_call(
                con = cl[[node]],
                fun = fn,
                args = c(list(x[[job]]), list(...)),
                return = TRUE,
                tag = job
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

.parallel_force_multicores <- function(multicores = NULL) {
    sits_env[["forced_multicores"]] <- multicores
}
