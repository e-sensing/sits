.jobs_max_multicores <- function(job_ncols,
                                 job_nrows,
                                 npaths,
                                 nbytes,
                                 proc_bloat,
                                 overlap,
                                 memsize,
                                 multicores) {
    # Memory needed per block
    job_memsize <- (job_nrows + 2 * overlap) * (job_ncols + 2 * overlap) *
        npaths * nbytes * proc_bloat * 1e-09
    # Max parallel blocks supported by memsize
    max_blocks <- floor(memsize / job_memsize)
    # Check if memsize is above minimum needed to process one block
    .check_that(
        x = max_blocks > 0,
        local_msg = paste("minimum memsize needed is",
                          job_memsize, "GB"),
        msg = "provided 'memsize' is insufficient for processing"
    )
    # Max multicores
    return(min(multicores, max_blocks))
}
.jobs_create_job <- function(col,
                             row,
                             ncols,
                             nrows,
                             max_ncols,
                             max_nrows,
                             overlap,
                             template_obj) {
    # Start job creation
    job <- tibble::tibble(
        col = as.integer(max(1, col - overlap)),
        row = as.integer(max(1, row - overlap))
    )
    job[["ncols"]] = as.integer(
        min(job[["col"]] + ncols + overlap - 1, max_ncols) - job[["col"]] + 1
    )
    job[["nrows"]] = as.integer(
        min(job[["row"]] + nrows + overlap - 1, max_nrows) - job[["row"]] + 1
    )
    job[["overlap"]] = as.integer(overlap)
    # Crop block from template
    r_obj <- .raster_crop_metadata(
        r_obj = template_obj,
        block = c(first_col = col,
                  first_row = row,
                  ncols = job[["ncols"]],
                  nrows = job[["nrows"]])
    )
    # Add bbox information
    job[["xmin"]] = .raster_xmin(r_obj = r_obj)
    job[["xmax"]] = .raster_xmax(r_obj = r_obj)
    job[["ymin"]] = .raster_ymin(r_obj = r_obj)
    job[["ymax"]] = .raster_ymax(r_obj = r_obj)
    job[["crs"]] = .raster_crs(r_obj = r_obj)
    return(job)
}
.jobs_create <- function(block_ncols,
                         block_nrows,
                         block_overlap = 0,
                         ncols,
                         nrows,
                         xmin,
                         xmax,
                         ymin,
                         ymax,
                         crs,
                         roi = NULL) {
    # Prepare raster tile template
    template_obj <- .raster_new_rast(
        nrows = nrows,
        ncols = ncols,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        nlayers = 1,
        crs = crs
    )
    # Generate (col, row) for all blocks
    cols_rows <- purrr::cross_df(list(
        col = seq(1, ncols - block_overlap, block_ncols),
        row = seq(1, nrows - block_overlap, block_nrows)
    ))
    # Create all jobs
    jobs <- purrr::pmap(cols_rows, .jobs_create_job,
        ncols = block_ncols,
        nrows = block_nrows,
        max_ncols = ncols,
        max_nrows = nrows,
        overlap = block_overlap,
        template = template_obj
    )
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
    crs <- jobs[[1]][["crs"]]
    # Reproject roi to template's CRS
    roi <- sf::st_transform(
        x = roi,
        crs = crs
    )
    # Compute intersecting jobs
    jobs <- purrr::keep(jobs, function(job) {
        any(c(sf::st_intersects(
            x = .jobs_as_sf(job = job),
            y = roi,
            sparse = FALSE)
        ))
    })
    return(jobs)
}
# .jobs_process <- function(job, files, fn) {
#     .raster_read_stack(
#         files = files,
#         block = .jobs_as_block(job)
#     )
# }
# .jobs_process <- function(job, ..., fn) {
#     chunk_lst <- purrr::map(
#         list(...), .raster_read_stack,
#         block = .jobs_as_block(job)
#     )
#     res <- fn(chunk_lst)
#
# }
.jobs_as_block <- function(job) {
    dplyr::transmute(
        job,
        first_col = .data[["col"]],
        first_row = .data[["row"]],
        nrows = .data[["nrows"]],
        ncols = .data[["ncols"]]
    )
}
.jobs_as_sf <- function(job) {
    .bbox_as_sf(
        xmin = job[["xmin"]],
        xmax = job[["xmax"]],
        ymin = job[["ymin"]],
        ymax = job[["ymax"]],
        crs = job[["crs"]]
    )
}
.job_save_values <- function(job, values, data_type, out_file) {
    # create a new raster
    r_obj <- .raster_new_rast(
        nrows = job[["nrows"]],
        ncols = job[["ncols"]],
        xmin = job[["xmin"]],
        xmax = job[["xmax"]],
        ymin = job[["ymin"]],
        ymax = job[["ymax"]],
        nlayers = ncol(values),
        crs = job[["crs"]]
    )
    # copy values
    r_obj <- .raster_set_values(
        r_obj = r_obj,
        values = values
    )
    # write the probabilities to a raster file
    .raster_write_rast(
        r_obj = r_obj,
        file = out_file,
        format = "GTiff",
        data_type = data_type,
        overwrite = TRUE
    )
}
