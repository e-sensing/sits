
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
            block = c(first_col = col, first_row = row, ncols = job[["ncols"]],
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

.jobs_read_tile_eo <- function(job, tile, ml_model, impute_fn, filter_fn) {
    # Convert job to block
    block <- .jobs_as_block(job)
    # Get file_info
    fi <- .fi(tile)
    # Get cloud values
    cloud_mask <- .fi_read_block(fi = fi, band = .band_cloud(), block = block)
    # Prepare cloud_mask
    if (!is.null(cloud_mask)) {
        cloud_mask <- .values_cloud_mask(
            values = cloud_mask,
            interp_values = .tile_cloud_interp_values(tile),
            is_bit_mask = .tile_cloud_bit_mask(tile)
        )
    }
    # Get model bands
    bands <- .ml_model_bands(ml_model)
    # Get bands values
    values <- purrr::map(bands, function(band) {
        # Get band values
        values <- .fi_read_block(fi = fi, band = band, block = block)
        # Remove cloud masked pixels
        if (!is.null(cloud_mask)) values[cloud_mask] <- NA
        # Correct missing, minimum, and maximum values; and scale values
        # Get band defaults for derived cube from config
        conf_band <- .conf_eo_band(
            source = .tile_source(tile), collection = .tile_collection(tile),
            band = band
        )
        # This can be ignored as it is already process by gdal
        # miss_value = .band_miss_value(conf_band)
        # if (!is.null(miss_value)) {
        #     values[values == miss_value] <- NA
        # }
        min_value <- .band_min_value(conf_band)
        if (!is.null(min_value)) {
            values[values < min_value] <- NA
        }
        max_value <- .band_max_value(conf_band)
        if (!is.null(max_value)) {
            values[values > max_value] <- NA
        }
        # Remove NA pixels
        if (!is.null(impute_fn)) {
            values <- impute_fn(values)
        }
        offset <- .band_offset(conf_band)
        if (!is.null(offset)) {
            values <- values - offset
        }
        scale <- .band_scale(conf_band)
        if (!is.null(scale)) {
            values <- values * scale
        }
        # Filter the time series
        if (!(is.null(filter_fn))) {
            values <- filter_fn(values)
        }
        # Normalize values
        stats <- .ml_model_stats(ml_model)
        if (!is.null(stats$q02) && !is.null(stats$q98)) {
            values <- normalize_data(values, stats$q02, stats$q98)
        }
        values
    })
    values <- do.call(cbind, values)
    colnames(values) <- .ml_model_attr_names(ml_model)
    values
}

.jobs_write_values <- function(job, values, data_type, file_pattern,
                               output_dir) {
    # create a new raster
    r_obj <- .raster_new_rast(
        nrows = job[["nrows"]], ncols = job[["ncols"]],
        xmin = job[["xmin"]], xmax = job[["xmax"]],
        ymin = job[["ymin"]], ymax = job[["ymax"]],
        nlayers = ncol(values), crs = job[["crs"]]
    )
    # copy values
    r_obj <- .raster_set_values(
        r_obj = r_obj,
        values = values
    )
    # Get output file name
    file <- .file_block_name(
        pattern = file_pattern, block = .jobs_as_block(job),
        output_dir = output_dir
    )
    # write the probabilities to a raster file
    .raster_write_rast(
        r_obj = r_obj,
        file = file,
        data_type = data_type,
        overwrite = TRUE
    )
    file
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
    .bbox_as_sf(job)
}

.jobs_parallel_chr <- function(jobs, fn, ...) {
    values_lst <- .sits_parallel_map(jobs, fn, ..., progress = TRUE)
    vapply(values_lst, c, NA_character_)
}
