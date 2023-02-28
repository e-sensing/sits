#' @title File info API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' Set of functions for handling `file_info`.
#'
NULL

#' @title Get `file_info` from a given tile.
#' @noRd
#' @param tile  A tile.
#' @returns A `file_info` tibble.
.fi <- function(tile) {
    tile[["file_info"]][[1]]
}

#' @title Set `file_info` into a given tile.
#' @noRd
#' @param tile  A tile.
#' @param value  A `file_info` to be set.
#' @returns An updated tile tibble.
`.fi<-` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["file_info"]] <- list(value)
    tile
}

.fi_type <- function(fi) {
    if ("date" %in% names(fi)) {
        "eo_cube"
    } else if (all(c("start_date", "end_date") %in% names(fi))) {
        "derived_cube"
    } else {
        stop("invalid file info")
    }
}

.fi_switch <- function(fi, ...) {
    switch(.fi_type(fi),
           ...,
           stop("invalid file_info type")
    )
}

.fi_eo <- function(fid, band, date, ncols, nrows, xres, yres, xmin, xmax,
                   ymin, ymax, path) {
    # Create a new eo file_info
    tibble::tibble(
        fid = fid, band = .band_eo(band), date = date, ncols = ncols,
        nrows = nrows, xres = xres, yres = yres, xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax, path = path
    )
}

.fi_eo_from_files <- function(files, fid, bands, date) {
    .check_that(length(files) == length(bands))
    files <- path.expand(files)
    r_obj <- .raster_open_rast(files)
    .fi_eo(
        fid = fid[[1]],
        band = bands,
        date = date[[1]],
        ncols = .raster_ncols(r_obj),
        nrows = .raster_nrows(r_obj),
        xres = .raster_xres(r_obj),
        yres = .raster_yres(r_obj),
        xmin = .raster_xmin(r_obj),
        xmax = .raster_xmax(r_obj),
        ymin = .raster_ymin(r_obj),
        ymax = .raster_ymax(r_obj),
        path = files
    )
}

.fi_derived <- function(band, start_date, end_date, ncols, nrows, xres, yres,
                        xmin, xmax, ymin, ymax, crs, path) {
    # Create a new derived file_info
    tibble::tibble(
        band = .band_derived(band), start_date = start_date,
        end_date = end_date, ncols = ncols, nrows = nrows,
        xres = xres, yres = yres, xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax, crs = crs, path = path
    )
}

.fi_derived_from_file <- function(file, band, start_date, end_date) {
    file <- path.expand(file)
    r_obj <- .raster_open_rast(file)
    .fi_derived(
        band = band, start_date = start_date, end_date = end_date,
        ncols = .raster_ncols(r_obj), nrows = .raster_nrows(r_obj),
        xres = .raster_xres(r_obj), yres = .raster_yres(r_obj),
        xmin = .raster_xmin(r_obj), xmax = .raster_xmax(r_obj),
        ymin = .raster_ymin(r_obj), ymax = .raster_ymax(r_obj),
        crs = .raster_crs(r_obj), path = file
    )
}

.fi_fid <- function(fi) {
    .as_chr(fi[["fid"]])
}

.fi_cloud_cover <- function(fi) {
    .as_dbl(fi[["cloud_cover"]])
}

.fi_filter_fid <- function(fi, fid) {
    .fi_switch(
        fi = fi,
        eo_cube = {
            fid_in_fi <- fid %in% .fi_fid(fi)
            if (!all(fid_in_fi)) {
                miss_fid <- paste0("'", fid[!fid_in_fi], "'", collapse = ",")
                stop("fid(s) ", miss_fid, " not found")
            }
            fi[.fi_fid(fi) %in% .as_chr(fid), ]
        }
    )
}

.fi_bands <- function(fi) {
    .as_chr(fi[["band"]])
}

.fi_filter_bands <- function(fi, bands) {
    bands_in_fi <- bands %in% .fi_bands(fi)
    if (!all(bands_in_fi)) {
        miss_bands <- paste0("'", bands[!bands_in_fi], "'", collapse = ",")
        stop("band(s) ", miss_bands, " not found")
    }
    fi[.fi_bands(fi) %in% bands, ]
}

.fi_min_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = min(.as_date(fi[["date"]])),
        derived_cube = min(.as_date(fi[["start_date"]]))
    )
}

.fi_max_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = max(.as_date(fi[["date"]])),
        derived_cube = max(.as_date(fi[["end_date"]]))
    )
}

.fi_timeline <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = .as_date(fi[["date"]]),
        derived_cube = .as_date(c(fi[["start_date"]], fi[["end_date"]]))
    )
}

.fi_paths <- function(fi) {
    .as_chr(fi[["path"]])
}

.fi_path <- function(fi) {
    .as_chr(fi[["path"]][[1]])
}

.fi_as_sf <- function(fi) {
    .bbox_as_sf(.bbox(fi))
}

.fi_during <- function(fi, start_date, end_date) {
    .between(.fi_timeline(fi), start_date[[1]], end_date[[1]])
}

.fi_filter_interval <- function(fi, start_date, end_date) {
    if (!.has(start_date)) {
        start_date <- .fi_min_date(fi)
    }
    if (!.has(end_date)) {
        end_date <- .fi_max_date(fi)
    }
    dates_in_fi <- .fi_during(
        fi = fi, start_date = start_date, end_date = end_date
    )
    if (!any(dates_in_fi)) {
        stop("no dates found between interval ", start_date[[1]], end_date[[1]])
    }
    fi[dates_in_fi, ]
}

.fi_filter_dates <- function(fi, dates) {
    dates <- .as_date(dates)
    dates_in_fi <- dates %in% .fi_timeline(fi)
    if (!all(dates_in_fi)) {
        miss_dates <- paste0("'", dates[!dates_in_fi], "'", collapse = ",")
        stop("date(s) ", miss_dates, " not found")
    }
    fi[.fi_timeline(fi) %in% dates, ]
}

.fi_intersects <- function(fi, roi) {
    .intersects(.fi_as_sf(fi), .roi_as_sf(roi))
}

.fi_filter_spatial <- function(fi, roi) {
    features_in_fi <- .fi_intersects(fi = fi, roi = roi)
    if (!any(features_in_fi)) {
        stop("no feature intersects informed roi")
    }
    fi[features_in_fi, ]
}

.fi_read_block <- function(fi, band, block) {
    band <- band[[1]]
    # Stops if no band is found
    fi <- .fi_filter_bands(fi = fi, bands = band)
    files <- .fi_paths(fi)

    #
    # Log here
    #
    .sits_debug_log(
        event = "start_block_data_read",
        key = "band",
        value = band
    )


    # Read values from all files in file_info
    values <- .raster_read_rast(files = files, block = block)


    #
    # Log here
    #
    .sits_debug_log(
        event = "end_block_data_read",
        key = "band",
        value = band
    )

    # Return values
    values
}
