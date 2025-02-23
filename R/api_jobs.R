#' @title Estimate the minimum memory need to process a job
#' @noRd
#' @param block_size  Size of the each block to be processed
#' @param npaths      Number of inputs (n_bands * n_times)
#' @param nbytes      Number of bytes per image
#' @param proc_bloat  Estimated processing bloat
#' @returns           Estimated job size in GB
.jobs_block_memsize <- function(block_size, npaths, nbytes, proc_bloat) {
    # Memory needed per job
    block_size * npaths * nbytes * proc_bloat * 1e-09
}
#' @title Update block parameter
#' @noRd
#' @param job_block_memsize  Total memory required for to process one block
#' @param block              Initial estimate of block size
#' @param image_size         Size of image to be processed
#' @param memsize            Memory available (in GB)
#' @param multicores         Number of cores available for processing
#' @returns                  Optimal estimate of block size
.jobs_optimal_block <- function(job_block_memsize, block, image_size, memsize,
                                multicores) {
    # Memory per core
    mpc <- memsize / multicores
    # Blocks per core
    bpc <- max(1, floor(mpc / job_block_memsize))
    # Image blocks in the horizontal direction
    hb <- ceiling(image_size[["ncols"]] / block[["ncols"]])
    if (bpc < hb * 2) {
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
    block <- .block_regulate_size(block)
    return(block)
}
#' @title Estimate the number of multicores to be used
#' @noRd
#' @param job_block_memsize  Total memory required to process one block
#' @param memsize            Memory available (in GB)
#' @param multicores         Number of cores available for processing
#' @returns            Number of cores required for processing
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
#' @title Return the number of multicores used
#' @noRd
#' @returns         Number of multicores
.jobs_multicores <- function() {
    length(sits_env[["cluster"]])
}
#' @title Return  list of jobs
#' @noRd
#' @param jobs      Jobs to be processed
#' @returns         List of jobs
.jobs_split <- function(jobs) {
    list(jobs)
}
#' @title Run a sequential function for all jobs
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run sequentially
#' @returns         List with function results
.jobs_map_sequential <- function(jobs, fn, ...) {
    slider::slide(jobs, fn, ...)
}
#' @title Run a sequential function for all jobs and return vector
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run sequentially
#' @returns         Character vector with function results
.jobs_map_sequential_chr <- function(jobs, fn, ...) {
    slider::slide_chr(jobs, fn, ...)
}
#' @title Run a sequential function for all jobs and return data.frame
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run sequentially
#' @returns         Data.frame with function results
.jobs_map_sequential_dfr <- function(jobs, fn, ...) {
    slider::slide_dfr(jobs, fn, ...)
}
#' @title Run a parallel function for all jobs
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run in parallel
#' @param ...       Additional parameters for function
#' @param sync_fn   Function to be synchronize jobs
#' @param progress  Show progress bar?
#' @returns         List with function results
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
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run in parallel
#' @param ...       Additional parameters for function
#' @param progress  Show progress bar?
#' @returns         Character vector with function results
.jobs_map_parallel_chr <- function(jobs, fn, ..., progress = FALSE) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    vapply(values_lst, c, NA_character_)
}
#' @title Run a parallel function for all jobs and return data.frame
#' @noRd
#' @param jobs      Jobs to be processed
#' @param fn        Function to be run in parallel
#' @param ...       Additional parameters for function
#' @param progress  Show progress bar?
#' @returns         Data.frame with function results
.jobs_map_parallel_dfr <- function(jobs, fn, ..., progress = FALSE) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    dplyr::bind_rows(values_lst)
}
