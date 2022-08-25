.jobs_check_multicores <- function(jobs, npaths, memsize, multicores, overlap) {
    # number of bytes per pixel
    nbytes <- 8
    # estimated processing bloat
    proc_bloat <- as.numeric(.config_processing_bloat())

    template <- .jobs_template(jobs)
    block_ncols <- template[["block_ncols"]]
    block_nrows <- template[["block_nrows"]]

    # regra universal -> 1 bloco por processo
    block_mem <- (block_nrows + 2 * overlap) * (block_ncols + 2 * overlap) *
        npaths * nbytes * proc_bloat * 1e-09

    # quantidade max de blocos simultaneos
    max_parallel_blocks <- floor(memsize / block_mem)

    .check_num(
        x = max_parallel_blocks,
        min = 1,
        msg = "provided 'memsize' is insufficient for processing"
    )

    return(min(multicores, max_parallel_blocks))
}

.jobs_template <- function(jobs) {
    attr(jobs, "tempĺate")
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
                         crs) {

    jobs <- purrr::pmap_dfr(
        purrr::cross_df(list(
            col = seq(1, ncols - block_overlap, block_ncols),
            row = seq(1, nrows - block_overlap, block_nrows)
        )),
        function(col, row, block_ncols, block_nrows, ncols, nrows, overlap) {
            jobs <- tibble::tibble(
                col = as.integer(max(1, col - overlap)),
                row = as.integer(max(1, row - overlap)),
                ncols = as.integer(min(.data[["col"]] + block_ncols + overlap - 1, ncols) - .data[["col"]] + 1),
                nrows = as.integer(min(.data[["row"]] + block_nrows + overlap - 1, nrows) - .data[["row"]] + 1),
                overlap = as.integer(overlap)
            )

        },
        block_ncols = block_ncols,
        block_nrows = block_nrows,
        ncols = ncols,
        nrows = nrows,
        overlap = block_overlap
    )

    attr(jobs, "template") <- list(block_ncols = block_ncols,
                                   block_nrows = block_nrows,
                                   nrows = nrows,
                                   ncols = ncols,
                                   xmin = xmin,
                                   xmax = xmax,
                                   ymin = ymin,
                                   ymax = ymax,
                                   crs = crs)

    return(jobs)
}


.jobs_filter_roi <- function(jobs, roi = NULL) {
    if (is.null(roi)) {
        return(jobs)
    }

    template <- .jobs_template(jobs)

    # tile template
    template <- .raster_new_rast(
        nrows = template[["nrows"]],
        ncols = template[["ncols"]],
        xmin = template[["xmin"]],
        xmax = template[["xmax"]],
        ymin = template[["ymin"]],
        ymax = template[["ymax"]],
        nlayers = 1,
        crs = template[["crs"]]
    )

    roi <- sf::st_transform(
        x = .sits_parse_roi_cube(roi),
        crs = .raster_crs(template)
    )

    is_intersected <- purrr::pmap_lgl(jobs, function(col,
                                                     row,
                                                     ncols,
                                                     nrows, ...) {

        r_obj <- .raster_crop_metadata(
            r_obj = template,
            block = c(first_col = col,
                      first_row = row,
                      ncols = ncols,
                      nrows = nrows)
        )
        block_ext <- .raster_ext_as_sf(r_obj)

        any(c(sf::st_intersects(x = block_ext, y = roi, sparse = FALSE)))
    })

    return(jobs[is_intersected, ])
}
