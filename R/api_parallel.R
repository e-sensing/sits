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
