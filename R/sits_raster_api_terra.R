
#' @keywords internal
#' @export
.sits_raster_api_check_package.terra_package <- function() {

    # package namespace
    pkg_name <- "terra"

    # check if raster package is available
    if (!requireNamespace(pkg_name)) {

        stop(paste(".sits_config_raster_package: package", pkg_name,
                   "not available. Install the package or change the",
                   "config file."), call. = FALSE)
    }

    class(pkg_name) <- paste0(pkg_name, "_package")

    return(invisible(pkg_name))
}

#' @keywords internal
#' @export
.sits_raster_api_get_values.terra_package <- function(r_obj, ...) {

    terra::values(x = r_obj, mat = TRUE, ...)
}

#' @keywords internal
#' @export
.sits_raster_api_set_values.terra_package <- function(r_obj, values, ...) {

    terra::values(x = r_obj) <- as.matrix(values)

    return(invisible(r_obj))
}

#' @keywords internal
#' @export
.sits_raster_api_extract.terra_package <- function(r_obj, xy, ...) {

    terra::extract(x = r_obj, y = xy, fun = NULL, cells = FALSE, ...)
}

#' @keywords internal
#' @export
.sits_raster_api_rast.terra_package <- function(r_obj, nlayers = 1, ...) {

    suppressWarnings(
        terra::rast(x = r_obj, nlyrs = nlayers, ...)
    )
}

#' @keywords internal
#' @export
.sits_raster_api_open_rast.terra_package <- function(file, ...) {

    # check for file length == 1
    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_open_rast: more than one file were informed"
    )

    suppressWarnings(
        terra::rast(x = file, ...)
    )
}

#' @keywords internal
#' @export
.sits_raster_api_read_rast.terra_package <- function(file,
                                                     extent = NULL, ...) {

    # check for files length == 1
    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_read_rast: more than one file were informed"
    )

    return(.sits_raster_api_read_stack.terra_package(files = file,
                                                     extent = extent))

}

#' @keywords internal
#' @export
.sits_raster_api_write_rast.terra_package <- function(r_obj,
                                                      file,
                                                      format,
                                                      data_type,
                                                      options,
                                                      overwrite, ...) {

    suppressWarnings(
        terra::writeRaster(
            x         = r_obj,
            filename  = file,
            wopt      = list(filetype = format,
                             datatype = data_type,
                             gdal     = options),
            overwrite = overwrite, ...
        )
    )

    # was the file written correctly?
    assertthat::assert_that(
        file.exists(file),
        msg = ".sits_raster_api_write_rast: unable to write raster object"
    )

    return(invisible(NULL))
}


#' @title Create a new raster file
#' @name .sits_raster_api_new_rast
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param nrows          Number of rows in the raster
#' @param ncols          Number of columns in the raster
#' @param xmin           X minimum of raster origin
#' @param xmax           X maximum of raster origin
#' @param ymin           Y minimum of raster origin
#' @param ymax           Y maximum of raster origin
#' @param nlayers        Number of layers of the raster
#' @param crs            Coordinate Reference System of the raster
#'
#' @return               Data cube values.
#' @export
.sits_raster_api_new_rast.terra_package <- function(nrows,
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
.sits_raster_api_open_stack.terra_package <- function(files, ...) {

    # check for files length > 0
    assertthat::assert_that(
        length(files) > 0,
        msg = ".sits_raster_api_open_stack: no file informed"
    )

    suppressWarnings(
        terra::rast(files, ...)
    )
}

#' @title Read a part of a raster file and return a matrix
#' @name .sits_raster_api_read_extent
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  files          Files associated to the raster object
#' @param  extent         Image extent to be read.
#' @return                Data.table of values
#' @export
.sits_raster_api_read_stack.terra_package <- function(files,
                                                      extent = NULL, ...) {

    # create raster objects
    r_obj <- .sits_raster_api_open_stack.terra_package(files = files, ...)

    # start read
    if (purrr::is_null(extent)) {
        return(terra::values(x = r_obj, mat = TRUE))
    }

    # precondition
    assertthat::assert_that(
        extent[["row"]] > 0 && extent[["col"]] > 0,
        msg = ".sits_raster_api_read_stack: invalid extent"
    )

    # precondition
    assertthat::assert_that(
        extent[["nrows"]] > 0 && extent[["ncols"]] > 0,
        msg = ".sits_raster_api_read_stack: invalid extent"
    )

    # read values
    terra::readStart(r_obj)
    values <- terra::readValues(x      = r_obj,
                                row    = extent[["row"]],
                                nrows  = extent[["nrows"]],
                                col    = extent[["col"]],
                                ncols  = extent[["ncols"]],
                                mat    = TRUE)
    terra::readStop(r_obj)

    return(values)
}

#' @keywords internal
#' @export
.sits_raster_api_crop.terra_package <- function(r_obj, extent, ...) {

    assertthat::assert_that(
        c("row", "nrows", "col", "ncols") %in% names(extent),
        msg = ".sits_raster_api_crop: invalid extent parameter"
    )

    # obtain coordinates from columns and rows
    x1 <- terra::xFromCol(object = r_obj,
                          col    = c(extent[["col"]]))
    x2 <- terra::xFromCol(object = r_obj,
                          col    = extent[["col"]] + extent[["ncols"]])
    y1 <- terra::yFromRow(object = r_obj,
                          row    = c(extent[["row"]]))
    y2 <- terra::yFromRow(object = r_obj,
                          row    = extent[["row"]] + extent[["nrows"]])

    # xmin, xmax, ymin, ymax
    ext <- terra::ext(
        x = c(min(x1, x2), max(x1, x2), min(y1, y2), max(y1, y2))
    )

    # crop raster
    suppressWarnings(
        terra::crop(x = r_obj, y = ext)
    )
}

#' @keywords internal
#' @export
.sits_raster_api_nrows.terra_package <- function(r_obj, ...) {

    terra::nrow(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_ncols.terra_package <- function(r_obj, ...) {

    terra::ncol(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_nlayers.terra_package <- function(r_obj, ...) {

    terra::nlyr(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_xmax.terra_package <- function(r_obj, ...) {

    terra::xmax(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_xmin.terra_package <- function(r_obj, ...) {

    terra::xmin(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_ymax.terra_package <- function(r_obj, ...) {

    terra::ymax(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_ymin.terra_package <- function(r_obj, ...) {

    terra::ymin(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_xres.terra_package <- function(r_obj, ...) {

    terra::xres(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_yres.terra_package <- function(r_obj, ...) {

    terra::yres(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_crs.terra_package <- function(r_obj, ...) {

    suppressWarnings(
        as.character(terra::crs(x = r_obj))
    )
}

#' @keywords internal
#' @export
.sits_raster_api_freq.terra_package <- function(r_obj, ...) {

    terra::freq(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_focal.terra_package <- function(r_obj,
                                                 window_size,
                                                 fun, ...) {

    # check fun parameter
    if (is.character(fun)) {

        fun <- fun[[1]]
        assertthat::assert_that(
            fun %in% c("modal", "sum", "mean"),
            msg = ".sits_raster_api_focal: invalid function"
        )

        if (fun == "modal")
            fun <- terra::modal
    }

    suppressWarnings(
        terra::focal(
            x   = r_obj,
            w   = window_size,
            fun = fun, ...
        )
    )
}
