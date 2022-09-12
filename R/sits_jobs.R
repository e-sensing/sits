
.jobs_memsize <- function(job_size, npaths, nbytes, proc_bloat, overlap) {
    # Memory needed per job
    (job_size[["nrows"]] + 2 * overlap) *
        (job_size[["ncols"]] + 2 * overlap) * npaths *
        nbytes * proc_bloat * 1e-09
}

.jobs_max_multicores <- function(job_memsize, memsize, multicores) {
    # Max parallel blocks supported by memsize
    max_blocks <- floor(memsize / job_memsize)
    # Max multicores
    min(multicores, max_blocks)
}

.jobs_create <- function(job_size, block_overlap, ncols, nrows, xmin, xmax,
                         ymin, ymax, crs, roi = NULL) {
    # Prepare raster tile template
    template_obj <- .raster_new_rast(
        nrows = nrows, ncols = ncols, xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax, nlayers = 1, crs = crs
    )
    # Generate (col, row) for all blocks
    cr <- purrr::cross_df(list(
        col = seq(1, ncols - block_overlap, job_size[["ncols"]]),
        row = seq(1, nrows - block_overlap, job_size[["nrows"]])
    ))
    # Create all jobs
    jobs <- purrr::pmap(cr, function(col, row, ncols, nrows, max_ncols,
                                     max_nrows, overlap, template_obj) {
        # Start job creation
        job <- tibble::tibble(
            col = as.integer(max(1, col - overlap)),
            row = as.integer(max(1, row - overlap))
        )
        job[["ncols"]] = as.integer(
            min(job[["col"]] + ncols + overlap - 1, max_ncols) -
                job[["col"]] + 1
        )
        job[["nrows"]] = as.integer(
            min(job[["row"]] + nrows + overlap - 1, max_nrows) -
                job[["row"]] + 1
        )
        job[["overlap"]] = as.integer(overlap)
        # Crop block from template
        r_obj <- .raster_crop_metadata(
            r_obj = template_obj,
            block = c(col = col, row = row, ncols = job[["ncols"]],
                      nrows = job[["nrows"]])
        )
        # Add bbox information
        job[["xmin"]] = .raster_xmin(r_obj = r_obj)
        job[["xmax"]] = .raster_xmax(r_obj = r_obj)
        job[["ymin"]] = .raster_ymin(r_obj = r_obj)
        job[["ymax"]] = .raster_ymax(r_obj = r_obj)
        job[["crs"]] = .raster_crs(r_obj = r_obj)
        job
    }, ncols = job_size[["ncols"]], nrows = job_size[["nrows"]],
    max_ncols = ncols, max_nrows = nrows, overlap = block_overlap,
    template = template_obj)
    # If no roi was provided return all jobs
    if (is.null(roi)) {
        return(jobs)
    }
    # Filter intersecting jobs
    jobs <- .jobs_intersects(jobs = jobs, roi = roi)
    return(jobs)
}

.jobs_intersects <- function(jobs, roi) {
    # Preconditions
    .check_that(
        x = length(jobs),
        min = 1,
        local_msg = "number of jobs should be >= 1",
        msg = "invalid 'jobs' parameter"
    )
    .check_that(
        x = inherits(roi, "sf"),
        local_msg = "value should be an sf object",
        msg = "invalid 'roi' parameter"
    )

    jobs[.intersects(.bbox_as_sf(dplyr::bind_rows(jobs)), roi)]
}

.jobs_parallel_chr <- function(jobs, fn, ...) {
    values_lst <- .sits_parallel_map(jobs, fn, ..., progress = TRUE)
    vapply(values_lst, c, NA_character_)
}
