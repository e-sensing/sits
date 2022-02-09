
#' @keywords internal
#' @export
.raster_check_package.terra <- function() {

    # package namespace
    pkg_name <- "terra"

    # check if raster package is available
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
        stop(paste(".sits_config_raster_package: package", pkg_name,
                   "not available. Please install the package for CRAN."), call. = FALSE)
    }

    class(pkg_name) <- pkg_name

    return(invisible(pkg_name))
}


#' @keywords internal
#' @export
.raster_data_type.terra <- function(data_type, ...) {

    return(data_type)
}

#' @keywords internal
#' @export
.raster_get_values.terra <- function(r_obj, ...) {

    # read values and close connection
    terra::readStart(x = r_obj)
    res <- terra::readValues(x = r_obj, mat = TRUE, ...)
    terra::readStop(x = r_obj)

    return(res)
}

#' @keywords internal
#' @export
.raster_set_values.terra <- function(r_obj, values, ...) {

    terra::values(x = r_obj) <- as.matrix(values)

    return(invisible(r_obj))
}

#' @keywords internal
#' @export
.raster_extract.terra <- function(r_obj, xy, ...) {

    terra::extract(x = r_obj, y = xy, ...)
}

#' @keywords internal
#' @export
.raster_rast.terra <- function(r_obj, nlayers = 1, ...) {

    suppressWarnings(
        terra::rast(x = r_obj, nlyrs = nlayers, ...)
    )
}

#' @keywords internal
#' @export
.raster_open_rast.terra <- function(file, ...) {

    suppressWarnings(
        terra::rast(x = file, ...)
    )
}

#' @keywords internal
#' @export
.raster_write_rast.terra <- function(r_obj,
                                     file,
                                     format,
                                     data_type,
                                     gdal_options,
                                     overwrite, ...) {

    # set caller to show in errors
    .check_set_caller(".raster_write_rast.terra")

    suppressWarnings(
        terra::writeRaster(
            x         = r_obj,
            filename  = file,
            wopt      = list(filetype = format,
                             datatype = data_type,
                             gdal     = gdal_options),
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
#' @export
.raster_new_rast.terra <- function(nrows,
                                   ncols,
                                   xmin,
                                   xmax,
                                   ymin,
                                   ymax,
                                   nlayers,
                                   crs, ...) {

    # create a raster object
    suppressWarnings(
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
}

#' @keywords internal
#' @export
.raster_open_stack.terra <- function(files, ...) {

    suppressWarnings(
        terra::rast(files, ...)
    )
}

#' @keywords internal
#' @export
.raster_read_stack.terra <- function(files,
                                     block = NULL, ...) {

    # create raster objects
    r_obj <- .raster_open_stack.terra(files = files, ...)

    # start read
    if (purrr::is_null(block)) {

        # read values
        terra::readStart(r_obj)
        values <- terra::readValues(x   = r_obj,
                                    mat = TRUE)
        # close file descriptor
        terra::readStop(r_obj)
    } else {

        # read values
        terra::readStart(r_obj)
        values <- terra::readValues(x      = r_obj,
                                    row    = block[["first_row"]],
                                    nrows  = block[["nrows"]],
                                    col    = block[["first_col"]],
                                    ncols  = block[["ncols"]],
                                    mat    = TRUE)
        # close file descriptor
        terra::readStop(r_obj)
    }

    return(values)
}

#' @keywords internal
#' @export
.raster_crop.terra <- function(r_obj, ...,
                               block = NULL,
                               bbox = NULL) {

    # obtain coordinates from columns and rows
    if (!is.null(block)) {
        # xmin
        x1 <- terra::xFromCol(object = r_obj,
                              col    = c(block[["first_col"]]))
        # xmax
        x2 <- terra::xFromCol(object = r_obj,
                              col    = block[["first_col"]] + block[["ncols"]] - 1)
        # ymax
        y1 <- terra::yFromRow(object = r_obj,
                              row    = c(block[["first_row"]]))
        # ymin
        y2 <- terra::yFromRow(object = r_obj,
                              row    = block[["first_row"]] + block[["nrows"]] - 1)
    } else if (!is.null(bbox)) {

        x1 <- bbox[["xmin"]]
        x2 <- bbox[["xmax"]]
        y2 <- bbox[["ymin"]]
        y1 <- bbox[["ymax"]]
    }

    # xmin, xmax, ymin, ymax
    extent <- terra::ext(
        x = c(x1, x2, y2, y1)
    )

    # crop raster
    suppressWarnings(
        terra::crop(x = r_obj, y = extent, snap = "out")
    )
}

#' @keywords internal
#' @export
.raster_nrows.terra <- function(r_obj, ...) {

    terra::nrow(x = r_obj)
}

#' @keywords internal
#' @export
.raster_ncols.terra <- function(r_obj, ...) {

    terra::ncol(x = r_obj)
}

#' @keywords internal
#' @export
.raster_nlayers.terra <- function(r_obj, ...) {

    terra::nlyr(x = r_obj)
}

#' @keywords internal
#' @export
.raster_xmax.terra <- function(r_obj, ...) {

    terra::xmax(x = r_obj)
}

#' @keywords internal
#' @export
.raster_xmin.terra <- function(r_obj, ...) {

    terra::xmin(x = r_obj)
}

#' @keywords internal
#' @export
.raster_ymax.terra <- function(r_obj, ...) {

    terra::ymax(x = r_obj)
}

#' @keywords internal
#' @export
.raster_ymin.terra <- function(r_obj, ...) {

    terra::ymin(x = r_obj)
}

#' @keywords internal
#' @export
.raster_xres.terra <- function(r_obj, ...) {

    terra::xres(x = r_obj)
}

#' @keywords internal
#' @export
.raster_yres.terra <- function(r_obj, ...) {

    terra::yres(x = r_obj)
}

#' @keywords internal
#' @export
.raster_crs.terra <- function(r_obj, ...) {

    crs <- suppressWarnings(
        terra::crs(x = r_obj, describe = TRUE))

    if (!is.na(crs[["code"]]))
        return(c(crs = paste(crs[["authority"]], crs[["code"]], sep = ":")))

    suppressWarnings(
        c(crs = as.character(terra::crs(x = r_obj))))
}

#' @keywords internal
#' @export
.raster_freq.terra <- function(r_obj, ...) {

    terra::freq(x = r_obj, bylayer = TRUE)
}

#' @keywords internal
#' @export
.raster_col.terra <- function(r_obj, x) {

    terra::colFromX(r_obj, x)
}


#' @keywords internal
#' @export
.raster_row.terra <- function(r_obj, y) {

    terra::rowFromY(r_obj, y)
}
