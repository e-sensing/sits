#' @keywords internal
#' @noRd
#' @export
.raster_check_package.terra <- function() {
    # package namespace
    pkg_name <- "terra"

    # check if terra package is available
    .check_require_packages(pkg_name)

    class(pkg_name) <- pkg_name

    return(invisible(pkg_name))
}

#' @keywords internal
#' @noRd
#' @export
.raster_get_values.terra <- function(r_obj, ...) {
    # read values and close connection
    terra::readStart(x = r_obj)
    res <- terra::readValues(x = r_obj, mat = TRUE, ...)
    terra::readStop(x = r_obj)

    return(res)
}

#' @keywords internal
#' @noRd
#' @export
.raster_set_values.terra <- function(r_obj, values, ...) {
    terra::values(x = r_obj) <- as.matrix(values)

    return(r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_extract.terra <- function(r_obj, xy, ...) {
    terra::extract(x = r_obj, y = xy, ...)
}

#' @keywords internal
#' @noRd
#' @export
.raster_file_blocksize.terra <- function(r_obj) {
    block_size <- c(terra::fileBlocksize(r_obj[[1]]))
    names(block_size) <- c("nrows", "ncols")

    return(block_size)
}

#' @keywords internal
#' @noRd
#' @export
.raster_rast.terra <- function(r_obj, nlayers = 1, ...) {
    suppressWarnings(
        terra::rast(x = r_obj, nlyrs = nlayers, ...)
    )
}

#' @keywords internal
#' @noRd
#' @export
.raster_open_rast.terra <- function(file, ...) {
    suppressWarnings(
        terra::rast(x = .file_normalize(file), ...)
    )
}

#' @keywords internal
#' @noRd
#' @export
.raster_write_rast.terra <- function(r_obj,
                                     file,
                                     data_type,
                                     overwrite, ...,
                                     missing_value = NA) {
    # set caller to show in errors
    .check_set_caller(".raster_write_rast.terra")

    suppressWarnings(
        terra::writeRaster(
            x = r_obj,
            filename = path.expand(file),
            wopt = list(
                filetype = "GTiff",
                datatype = data_type,
                gdal = .conf("gdal_creation_options")
            ),
            NAflag = missing_value,
            overwrite = overwrite, ...
        )
    )

    # was the file written correctly?
    .check_file(
        x = file,
        msg = "unable to write raster object"
    )

    return(invisible(NULL))
}

#' @keywords internal
#' @noRd
#' @export
.raster_new_rast.terra <- function(nrows,
                                   ncols,
                                   xmin,
                                   xmax,
                                   ymin,
                                   ymax,
                                   nlayers,
                                   crs, ...,
                                   xres = NULL,
                                   yres = NULL) {
    # prepare resolution
    resolution <- c(xres, yres)

    # prepare crs
    if (is.numeric(crs)) crs <- paste0("EPSG:", crs)

    if (is.null(resolution)) {
        # create a raster object
        r_obj <- suppressWarnings(
            terra::rast(
                nrows = nrows,
                ncols = ncols,
                nlyrs = nlayers,
                xmin  = xmin,
                xmax  = xmax,
                ymin  = ymin,
                ymax  = ymax,
                crs   = crs
            )
        )
    } else {
        # create a raster object
        r_obj <- suppressWarnings(
            terra::rast(
                nlyrs = nlayers,
                xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax,
                crs = crs,
                resolution = resolution
            )
        )
    }

    return(r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_read_rast.terra <- function(files, ..., block = NULL) {
    # create raster objects
    r_obj <- .raster_open_rast.terra(file = path.expand(files), ...)

    # start read
    if (purrr::is_null(block)) {
        # read values
        terra::readStart(r_obj)
        values <- terra::readValues(
            x   = r_obj,
            mat = TRUE
        )
        # close file descriptor
        terra::readStop(r_obj)
    } else {
        # read values
        terra::readStart(r_obj)
        values <- terra::readValues(
            x = r_obj,
            row = block[["row"]],
            nrows = block[["nrows"]],
            col = block[["col"]],
            ncols = block[["ncols"]],
            mat = TRUE
        )
        # close file descriptor
        terra::readStop(r_obj)
    }

    return(values)
}

#' @keywords internal
#' @noRd
#' @export
.raster_crop.terra <- function(r_obj,
                               file,
                               data_type,
                               overwrite,
                               block,
                               missing_value = NA) {
    # Update missing_value
    missing_value <- if (is.null(missing_value)) NA else missing_value
    # obtain coordinates from columns and rows
    # get extent
    xmin <- terra::xFromCol(
        object = r_obj,
        col = block[["col"]]
    )
    xmax <- terra::xFromCol(
        object = r_obj,
        col = block[["col"]] + block[["ncols"]] - 1
    )
    ymax <- terra::yFromRow(
        object = r_obj,
        row = block[["row"]]
    )
    ymin <- terra::yFromRow(
        object = r_obj,
        row = block[["row"]] + block[["nrows"]] - 1
    )

    # xmin, xmax, ymin, ymax
    extent <- terra::ext(x = c(xmin, xmax, ymin, ymax))

    # crop raster
    suppressWarnings(
        terra::crop(
            x = r_obj,
            y = extent,
            snap = "out",
            filename = path.expand(file),
            wopt = list(
                filetype = "GTiff",
                datatype = data_type,
                gdal = .conf("gdal_creation_options")
            ),
            NAflag = missing_value,
            overwrite = overwrite
        )
    )
}

#' @keywords internal
#' @noRd
#' @export
.raster_crop_metadata.terra <- function(r_obj, ...,
                                        block = NULL,
                                        bbox = NULL) {
    # obtain coordinates from columns and rows
    if (!is.null(block)) {
        # get extent
        xmin <- terra::xFromCol(
            object = r_obj,
            col = block[["col"]]
        )
        xmax <- terra::xFromCol(
            object = r_obj,
            col = block[["col"]] + block[["ncols"]] - 1
        )
        ymax <- terra::yFromRow(
            object = r_obj,
            row = block[["row"]]
        )
        ymin <- terra::yFromRow(
            object = r_obj,
            row = block[["row"]] + block[["nrows"]] - 1
        )
    } else if (!is.null(bbox)) {
        xmin <- bbox[["xmin"]]
        xmax <- bbox[["xmax"]]
        ymin <- bbox[["ymin"]]
        ymax <- bbox[["ymax"]]
    }

    # xmin, xmax, ymin, ymax
    extent <- terra::ext(x = c(xmin, xmax, ymin, ymax))

    # crop raster
    suppressWarnings(
        terra::crop(x = r_obj, y = extent, snap = "out")
    )
}

#' @keywords internal
#' @noRd
#' @export
.raster_nrows.terra <- function(r_obj, ...) {
    terra::nrow(x = r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_ncols.terra <- function(r_obj, ...) {
    terra::ncol(x = r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_nlayers.terra <- function(r_obj, ...) {
    terra::nlyr(x = r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_xmax.terra <- function(r_obj, ...) {
    terra::xmax(x = r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_xmin.terra <- function(r_obj, ...) {
    terra::xmin(x = r_obj)
}

#' @keywords internal
#' @export
#' @noRd
.raster_ymax.terra <- function(r_obj, ...) {
    terra::ymax(x = r_obj)
}

#' @keywords internal
#' @export
#' @noRd
.raster_ymin.terra <- function(r_obj, ...) {
    terra::ymin(x = r_obj)
}

#' @keywords internal
#' @export
#' @noRd
.raster_xres.terra <- function(r_obj, ...) {
    terra::xres(x = r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_yres.terra <- function(r_obj, ...) {
    terra::yres(x = r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_crs.terra <- function(r_obj, ...) {
    crs <- suppressWarnings(
        terra::crs(x = r_obj, describe = TRUE)
    )

    if (!is.na(crs[["code"]])) {
        return(paste(crs[["authority"]], crs[["code"]], sep = ":"))
    }

    suppressWarnings(
        as.character(terra::crs(x = r_obj))
    )
}

#' @keywords internal
#' @noRd
#' @export
#'
.raster_freq.terra <- function(r_obj, ...) {
    terra::freq(x = r_obj, bylayer = TRUE)
}

#' @keywords internal
#' @noRd
#' @export
.raster_col.terra <- function(r_obj, x) {
    terra::colFromX(r_obj, x)
}


#' @keywords internal
#' @noRd
#' @export
.raster_row.terra <- function(r_obj, y) {
    terra::rowFromY(r_obj, y)
}
