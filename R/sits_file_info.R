#' @title Functions to work with file info tibble
#' @name file_info_functions
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param cube    Input data cube.
#' @param bands   Bands to be filtered
#' @param dates   Dates to be filtered.
#' @param fid     Feature id (fid) to be filtered.
#'
#' @return        Vector with requested information obtained in the file_info.
NULL

#' @rdname file_info_functions
#'
#' @return The file info for a cube with a single tile
#'         filtered by bands if required.
#'
.fi <- function(cube,
                bands = NULL,
                fid = NULL,
                start_date = NULL,
                end_date = NULL) {

    # pre-condition - one tile at a time
    .check_num(nrow(cube),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile at a time for file_info"
    )

    # get the file info associated with the tile
    file_info <- cube[["file_info"]][[1]]

    # check bands
    if (!is.null(bands)) {
        .check_cube_bands(cube, bands = bands)
        file_info <- file_info[file_info[["band"]] %in% bands, ]
    }

    # filter fid
    if (!is.null(fid)) {
        fids <- .fi_fids(file_info)
        .check_chr_within(paste0(fid),
            within = paste0(fids),
            msg = "invalid fid value"
        )
        file_info <- file_info[file_info[["fid"]] == fid, ]
    }

    if (!is.null(start_date)) {
        cube_start_date <- sort(.fi_timeline(file_info))[[1]]

        .check_that(start_date >= cube_start_date, msg = "invalid start date")

        file_info <- file_info[file_info[["date"]] >= start_date, ]
    }

    if (!is.null(end_date)) {
        cube_end_date <- sort(.fi_timeline(file_info),
                              decreasing = TRUE)[[1]]
        .check_that(end_date < cube_end_date, msg = "invalid end date")
        file_info <- file_info[file_info[["date"]] < end_date, ]
    }

    return(file_info)
}


#' @rdname file_info_functions
#'
#' @return the file ids for a single tile
.fi_fids <- function(fi) {
    fids <- unique(fi[["fid"]])

    .check_num(length(fids),
        min = 1, is_integer = TRUE,
        msg = "wrong fid in file_info"
    )
    return(fids)
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

.fi_fid_filter <- function(fi, fid) {
    .fi_switch(
        fi = fi,
        eo_cube = fi[.fi_bands(fi) %in% .as_chr(fid), ]
    )
}

.fi_bands <- function(fi) {
    .as_chr(fi[["band"]])
}

.fi_filter_bands <- function(fi, bands) {
    bands_in_fi <- bands %in% .fi_bands(fi)
    if (!all(bands_in_fi)) {
        missing_bands <- paste0("'", bands[!bands_in_fi], "'", collapse = ",")
        stop("band(s) ", missing_bands, " not found")
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

.fi_dates <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = .as_date(fi[["date"]])
    )
}

.fi_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = .as_date(fi[["date"]][[1]])
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
    .bbox_as_sf(fi)
}

.fi_during <- function(fi, start_date, end_date) {
    .between(.fi_timeline(fi), start_date, end_date)
}

.fi_filter_interval <- function(fi, start_date, end_date) {
    if (!.has(start_date)) {
        start_date <- .fi_min_date(fi)
    }
    if (!.has(end_date)) {
        end_date <- .fi_max_date(fi)
    }
    fi[.fi_during(fi, start_date, end_date), ]
}

.fi_intersects <- function(fi, roi) {
    .intersects(.fi_as_sf(fi), .roi_as_sf(roi))
}

.fi_filter_spatial <- function(fi, roi) {
    fi[.fi_intersects(fi, roi), ]
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
