.jobs_memsize <- function(job_size, npaths, nbytes, proc_bloat) {
    # Memory needed per job
    job_size * npaths * nbytes * proc_bloat * 1e-09
}

.jobs_max_multicores <- function(job_memsize, memsize, multicores) {
    # Check if memsize is above minimum needed to process one block
    .check_that(
        x = job_memsize < memsize,
        local_msg = paste("minimum memsize needed is", job_memsize, "GB"),
        msg = "provided 'memsize' is insufficient for processing"
    )
    # Max parallel blocks supported by memsize
    max_blocks <- floor(memsize / job_memsize)
    # Max multicores
    min(multicores, max_blocks)
}

.jobs_optimal_block <- function(job_memsize, block, image_size, memsize,
                                multicores) {
    # Memory per core
    mpc <- memsize / multicores
    # Blocks per core
    bpc <- max(1, floor(mpc / job_memsize))
    # Image horizontal blocks
    hb <- ceiling(image_size[["ncols"]] / block[["ncols"]])
    if (bpc < hb * 2) {
        # 1st optimization - line level
        # Number of segments to process whole line
        h_nsegs <- ceiling(hb / bpc)
        # Number of horizontal blocks
        return(c(ncols = ceiling(hb / h_nsegs) * block[["ncols"]],
                 nrows = block[["nrows"]]))
    }
    # 2nd optimization - area level
    # Lines per core
    lpc <- floor(bpc / hb)
    # Image vertical blocks
    vb <- ceiling(image_size[["nrows"]] / block[["nrows"]])
    # Number of vertical segments
    v_nsegs <- ceiling(vb / lpc)
    # Number of vertical blocks
    return(c(ncols = min(hb * block[["ncols"]], image_size[["ncols"]]),
             nrows = min(ceiling(vb / v_nsegs) * block[["nrows"]],
                         image_size[["nrows"]]))
    )
}

.jobs_multicores <- function() {
    length(sits_env[["cluster"]])
}

.jobs_split <- function(jobs) {
    # TODO: split jobs by multicores (nrow(jobs) / muticores = #rounds)
    list(jobs)
}

.jobs_map_sequential <- function(jobs, fn, ...) {
    slider::slide(jobs, fn, ...)
}

.jobs_map_sequential_chr <- function(jobs, fn, ...) {
    slider::slide_chr(jobs, fn, ...)
}

.jobs_map_sequential_dfr <- function(jobs, fn, ...) {
    slider::slide_dfr(jobs, fn, ...)
}

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

.jobs_map_parallel_chr <- function(jobs, fn, ..., progress = FALSE) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    vapply(values_lst, c, NA_character_)
}

.jobs_map_parallel_dfr <- function(jobs, fn, ..., progress = FALSE) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    dplyr::bind_rows(values_lst)
}
