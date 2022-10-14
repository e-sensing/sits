
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

.jobs_multicores <- function() {
    length(sits_env[["cluster"]])
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

.jobs_map_parallel <- function(jobs, fn, ..., progress = FALSE) {
    jobs <- slider::slide(jobs, identity)
    .sits_parallel_map(jobs, fn, ..., progress = progress)
}

.jobs_map_parallel_chr <- function(jobs, fn, ..., progress = FALSE) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    vapply(values_lst, c, NA_character_)
}

.jobs_map_parallel_dfr <- function(jobs, fn, ..., progress = FALSE) {
    values_lst <- .jobs_map_parallel(jobs, fn, ..., progress = progress)
    dplyr::bind_rows(values_lst)
}
