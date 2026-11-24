#' @title Estimate the minimum memory need to process a job
#' @name .jobs_block_memsize
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param block_size  Size of the each block to be processed
#' @param npaths      Number of inputs (n_bands * n_times)
#' @param nbytes      Number of bytes per image
#' @param proc_bloat  Estimated processing bloat
#' @return           Estimated job size in GB
.jobs_block_memsize <- function(block_size, npaths, nbytes, proc_bloat) {
    # Memory needed per job
    block_size * npaths * nbytes * proc_bloat * 1e-09
}
#' @title Update block parameter
#' @name .jobs_optimal_block
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param job_block_memsize  Total memory required for to process one block
#' @param block              Initial estimate of block size
#' @param image_size         Size of image to be processed
#' @param memsize            Memory available (in GB)
#' @param multicores         Number of cores available for processing
#' @return                  Optimal estimate of block size
.jobs_optimal_block <- function(job_block_memsize, block, image_size, memsize,
                                multicores) {
    # Memory per core
    mpc <- memsize / multicores
    # Blocks per core
    bpc <- max(1L, floor(mpc / job_block_memsize))
    # Image blocks in the horizontal direction
    hb <- ceiling(image_size[["ncols"]] / block[["ncols"]])
    if (bpc < hb * 2L) {
        # 1st optimization - line level
        # Number of segments to process whole line
        h_nsegs <- ceiling(hb / bpc)
        # Number of horizontal blocks
        return(c(
            ncols = ceiling(hb / h_nsegs) * block[["ncols"]],
            nrows = block[["nrows"]]
        ))
    }
    # 2nd optimization - area level
    # How many blocks per core in the vertical direction?
    lpc <- floor(bpc / hb)
    # Image vertical blocks
    vb <- ceiling(image_size[["nrows"]] / block[["nrows"]])
    # Number of vertical segments
    v_nsegs <- ceiling(vb / lpc)
    # Number of vertical blocks
    block <- c(
        ncols = min(
            hb * block[["ncols"]],
            image_size[["ncols"]]
        ),
        nrows = min(
            ceiling(vb / v_nsegs) * block[["nrows"]],
            image_size[["nrows"]]
        )
    )
    # Terra requires at least two pixels to recognize an extent as valid
    # polygon and not a line or point
    .block_regulate_size(block)
}
#' @title Block of the chunks of a function that draws per chunk
#' @name .jobs_draw_block
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @description The draw is made per chunk, so the chunk does not depend on
#' `memsize` or `multicores`. Whole file blocks are read once; a fixed side
#' keeps the chunks few, since the candidates grow with their number.
#' @param block       File block size
#' @param image_size  Size of the image to be processed
#' @return            Block size of the chunks
.jobs_draw_block <- function(block, image_size) {
    side <- .conf("draw_chunk_side")
    ncols <- max(1L, round(side / block[["ncols"]])) * block[["ncols"]]
    nrows <- max(1L, round(side / block[["nrows"]])) * block[["nrows"]]
    block <- c(
        ncols = as.integer(min(ncols, image_size[["ncols"]])),
        nrows = as.integer(min(nrows, image_size[["nrows"]]))
    )
    .block_regulate_size(block)
}
#' @title Estimate the number of multicores to be used
#' @name  .job_max_multicore
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param job_block_memsize  Total memory required to process one block
#' @param memsize            Memory available (in GB)
#' @param multicores         Number of cores available for processing
#' @return            Number of cores required for processing
.jobs_max_multicores <- function(job_block_memsize, memsize, multicores) {
    # set caller to show in errors
    .check_set_caller(".jobs_max_multicores")
    # Check if memsize is above minimum needed to process one block
    .check_that(job_block_memsize < memsize)
    # Max parallel blocks supported by memsize
    max_blocks <- floor(memsize / job_block_memsize)
    # Max multicores
    min(multicores, max_blocks)
}
#' @title One memory budget of a job
#' @name .jobs_budget
#' @noRd
#' @description Part of the canonical contract written in the roxygen of
#'   `.texture_backend()`.
#' @param name           Argument the user sets: "memsize", "gpu_memsize"
#' @param memsize        Memory of the budget in GB
#' @param block_memsize  GB of one block at the base block
#' @param worker_memsize GB held by each worker besides its block
#' @param call_memsize   GB the call holds once, besides its workers
#' @return               List with name, memsize, block, worker and call
.jobs_budget <- function(name, memsize, block_memsize, worker_memsize = 0,
                         call_memsize = 0) {
    list(
        name = name, memsize = memsize, block = block_memsize,
        worker = worker_memsize, call = call_memsize
    )
}
#' @title Fixed memory of a call
#' @name .jobs_call_memsize
#' @noRd
#' @description Call term of a RAM budget, in the contract written in the
#'   roxygen of `.texture_backend()`.
#' @param  options     Name of the options of the algorithm in config
#' @param  backend     Name of the backend block inside the options
#' @param  multicores  Workers asked; one worker runs one merge at a time
#' @return GB the call adds to the main process and to its merges at once;
#'   a missing key counts as 0
.jobs_call_memsize <- function(options, backend, multicores) {
    merges <- 0
    if (.conf_exists(options, "merges")) {
        merges <- min(.conf(options, "merges"), multicores)
    }
    .conf_backend(options, backend, "ram_main_memsize", 0) +
        merges * .conf_backend(options, backend, "ram_merge_memsize", 0)
}
#' @title Workers and block that fit every budget
#' @name .jobs_size
#' @noRd
#' @description Criteria (b) to (d) of the contract written in the roxygen
#'   of `.texture_backend()`.
#' @param budgets     List of `.jobs_budget()`; the call term of each is
#'   taken from its memory before the workers are counted
#' @param block       Initial estimate of block size
#' @param image_size  Size of image to be processed
#' @param multicores  Workers asked
#' @return            List with block, multicores and limit, the names of
#'   the budgets that cut the workers pasted with " and ", or NULL
.jobs_size <- function(budgets, block, image_size, multicores) {
    .check_that(length(budgets) > 0L)
    # a block that does not fit names its budget, before any worker count
    purrr::walk(budgets, function(budget) {
        msg <- if (budget[["call"]] > 0) {
            sprintf(
                .conf("messages", ".jobs_size_call"),
                budget[["call"]], budget[["block"]] + budget[["worker"]],
                budget[["memsize"]], budget[["name"]], budget[["name"]]
            )
        } else {
            sprintf(
                .conf("messages", ".jobs_size_budget"),
                budget[["name"]], budget[["name"]]
            )
        }
        .check_that(
            budget[["block"]] + budget[["worker"]] + budget[["call"]] <
                budget[["memsize"]],
            msg = msg
        )
    })
    allowed <- purrr::map_dbl(budgets, function(budget) {
        floor(
            (budget[["memsize"]] - budget[["call"]]) /
                (budget[["block"]] + budget[["worker"]])
        )
    })
    workers <- min(multicores, allowed)
    # each block is sized for the final workers; fewer pixels wins
    blocks <- purrr::map(budgets, function(budget) {
        .jobs_optimal_block(
            job_block_memsize = budget[["block"]],
            block = block,
            image_size = image_size,
            memsize = budget[["memsize"]] - budget[["call"]] -
                workers * budget[["worker"]],
            multicores = workers
        )
    })
    pixels <- purrr::map_dbl(blocks, function(b) {
        as.numeric(b[["nrows"]]) * b[["ncols"]]
    })
    cut <- purrr::map_chr(budgets, "name")[allowed < multicores]
    list(
        block = blocks[[which.min(pixels)]],
        multicores = workers,
        limit = if (length(cut) > 0L) paste(cut, collapse = " and ")
    )
}
#' @title Limit the request of workers to the open cluster
#' @name .jobs_request_workers
#' @noRd
#' @description Criterion (a) of the contract written in the roxygen of
#'   `.texture_backend()`: the given workers, else the open cluster, else
#'   2. An open cluster is never enlarged: a request above its size falls
#'   to the size and a warning says so.
#' @param multicores  Workers asked, or NULL
#' @return            Workers asked, at most the size of the open cluster
.jobs_request_workers <- function(multicores) {
    # a cluster whose sockets died is not open: no request, no cut
    is_open <- .parallel_is_open()
    nodes <- if (is_open) length(sits_env[["cluster"]]) else 0L
    if (is.null(multicores)) {
        multicores <- if (is_open) nodes else 2L
    }
    if (nodes > 0L && multicores > nodes) {
        if (.message_warnings()) {
            warning(
                sprintf(
                    .conf("messages", ".jobs_request_workers"),
                    nodes, as.integer(multicores)
                ),
                call. = FALSE
            )
        }
        return(nodes)
    }
    multicores
}
#' @title Warn when memory allows fewer workers than asked
#' @name .jobs_warn_workers
#' @noRd
#' @param  asked    Workers asked
#' @param  workers  Workers that memory allows
#' @param  limit    Budgets that cut the workers, from `.jobs_size()`, or
#'                  NULL
#' @return No value, called for side effect
.jobs_warn_workers <- function(asked, workers, limit) {
    if (.has(limit) && .message_warnings()) {
        warning(
            sprintf(
                .conf("messages", ".jobs_warn_workers"),
                limit, as.integer(workers), as.integer(asked), limit
            ),
            call. = FALSE
        )
    }
}
#' @title Return the number of multicores used
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @return         Number of multicores
.jobs_multicores <- function() {
    length(sits_env[["cluster"]])
}
#' @title Return  list of jobs
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param jobs      Jobs to be processed
#' @return         List of jobs
.jobs_split <- function(jobs) {
    list(jobs)
}
#' @title Run a sequential function for all jobs
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run sequentially
#' @return         List with function results
.jobs_map_sequential <- function(jobs, fn, ...) {
    slider::slide(jobs, fn, ...)
}
#' @title Run a sequential function for all jobs and return data.frame
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run sequentially
#' @return         Data.frame with function results
.jobs_map_sequential_dfr <- function(jobs, fn, ...) {
    slider::slide_dfr(jobs, fn, ...)
}
#' @title Run a parallel function for all jobs
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run in parallel
#' @param ...       Additional parameters for function
#' @param sync_fn   Function to be synchronize jobs
#' @param progress  Show progress bar?
#' @return         List with function results
.jobs_map_parallel <- function(jobs, fn, ..., sync_fn = NULL,
                               progress = FALSE) {
    # Do split by rounds only if sync_fn is not NULL
    rounds <- .jobs_split(jobs)
    unlist(purrr::map(rounds, function(round) {
        if (!is.null(sync_fn)) {
            sync_fn(round)
        }
        round <- slider::slide(round, identity)
        .parallel_map(round, fn, ..., progress = progress)
    }), recursive = FALSE)
}
#' @title Run a parallel function for all jobs and return vector
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run in parallel
#' @param ...       Additional parameters for function
#' @param progress  Show progress bar?
#' @return         Character vector with function results
.jobs_map_parallel_chr <- function(jobs, fn, ..., progress = progress) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    vapply(values_lst, c, NA_character_)
}
#' @title Run a parallel function for all jobs and return data.frame
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run in parallel
#' @param ...       Additional parameters for function
#' @param progress  Show progress bar?
#' @return         Data.frame with function results
.jobs_map_parallel_dfr <- function(jobs, fn, ..., progress = progress) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    dplyr::bind_rows(values_lst)
}
#' @title Run groups of jobs in a queue of the sits cluster
#' @name .jobs_map_queue
#' @noRd
#' @description The last job of a group waits for the other jobs of its group
#' and is sent before the ready jobs of other groups, through
#' \code{.parallel_queue()}. So a worker closes a group while the others run
#' the jobs of the next groups.
#' @param jobs      Jobs to be processed, with a column \code{group}; the rows
#'                  of a group are contiguous and its last row closes it
#' @param fn        Function to be run for each job, as in
#'                  \code{.jobs_map_parallel()}
#' @param ...       Additional parameters for function
#' @param on_start  Function of the job, run in this process when the first
#'                  job of a group is sent, or NULL
#' @param on_last   Function of the job, run in this process when the last
#'                  job of a group is sent, or NULL
#' @param on_end    Function of the job and its value, run in this process
#'                  when the last job of a group is done, or NULL
#' @param progress  Show progress bar?
#' @return          List with the values of the last job of each group, in
#'                  the order of the groups
.jobs_map_queue <- function(jobs, fn, ..., on_start = NULL, on_last = NULL,
                            on_end = NULL, progress = FALSE) {
    .check_set_caller(".jobs_map_queue")
    # a cube without tiles gives jobs without columns
    if (nrow(jobs) == 0L) {
        return(list())
    }
    runs <- rle(jobs[["group"]])
    # each run of a split group would close before the later rows of it
    .check_that(
        !anyDuplicated(runs[["values"]]),
        msg = .conf("messages", ".jobs_map_queue")
    )
    ends <- cumsum(runs[["lengths"]])
    starts <- ends - runs[["lengths"]] + 1L
    n <- nrow(jobs)
    is_first <- seq_len(n) %in% starts
    is_last <- seq_len(n) %in% ends
    after <- rep(list(integer(0L)), n)
    after[ends] <- purrr::map2(starts, ends, function(from, to) {
        seq_len(to - from) + from - 1L
    })
    rows <- slider::slide(jobs, identity)
    on_send <- function(k) {
        if (is_first[[k]] && .has(on_start)) {
            on_start(rows[[k]])
        }
        if (is_last[[k]] && .has(on_last)) {
            on_last(rows[[k]])
        }
    }
    on_done <- function(k, value) {
        if (is_last[[k]] && .has(on_end)) {
            on_end(rows[[k]], value)
        }
    }
    values <- .parallel_queue(
        jobs = rows, fn = fn, ..., after = after, first = is_last,
        on_send = on_send, on_done = on_done, progress = progress
    )
    values[is_last]
}
