
#' @keywords internal
#' @export
.raster_check_package.raster <- function() {

    # package namespace
    pkg_name <- "raster"

    # check if raster package is available
    if (!requireNamespace(pkg_name, quietly = TRUE)) {

        stop(paste(".sits_config_raster_package: package", pkg_name,
                   "not available. Install the package or change the",
                   "config file."), call. = FALSE)
    }

    class(pkg_name) <- pkg_name

    return(invisible(pkg_name))
}

#' @keywords internal
#' @export
.raster_data_type.raster <- function(data_type, ...) {

    return(data_type)
}

#' @keywords internal
#' @export
.raster_get_values.raster <- function(r_obj, ...) {

    unname(as.matrix(raster::values(x = r_obj, ...)))
}

#' @keywords internal
#' @export
.raster_set_values.raster <- function(r_obj, values, ...) {

    raster::values(x = r_obj) <- as.matrix(values)

    return(invisible(r_obj))
}

#' @keywords internal
#' @export
.raster_extract.raster <- function(r_obj, xy, ...) {

    as.matrix(raster::extract(x = r_obj,
                              y = xy,
                              fun = NULL,
                              cellnumber = FALSE, ...))
}

#' @keywords internal
#' @export
.raster_rast.raster <- function(r_obj, nlayers = 1, ...) {

    suppressWarnings(
        raster::brick(x = r_obj, values = FALSE, nl = nlayers, ...)
    )
}

#' @keywords internal
#' @export
.raster_open_rast.raster <- function(file, ...) {

    suppressWarnings(
        raster::brick(x = file, ...)
    )
}

#' @keywords internal
#' @export
.raster_read_rast.raster <- function(file,
                                     block = NULL, ...) {

    return(.raster_read_stack.raster(files = file,
                                     block = block))

}

#' @keywords internal
#' @export
.raster_write_rast.raster <- function(r_obj,
                                      file,
                                      format,
                                      data_type,
                                      gdal_options,
                                      overwrite, ...) {

    # set caller to show in errors
    .check_set_caller(".raster_write_rast.raster")

    suppressWarnings(
        raster::writeRaster(
            x         = r_obj,
            filename  = file,
            format    = format,
            datatype  = data_type,
            options   = gdal_options,
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
.raster_new_rast.raster <- function(nrows,
                                    ncols,
                                    xmin,
                                    xmax,
                                    ymin,
                                    ymax,
                                    nlayers,
                                    crs, ...) {

    # create a raster object
    suppressWarnings(
        raster::brick(
            nrows = nrows,
            ncols = ncols,
            xmn  = xmin,
            xmx  = xmax,
            ymn  = ymin,
            ymx  = ymax,
            nl = nlayers,
            crs   = crs
        )
    )
}

#' @keywords internal
#' @export
.raster_open_stack.raster <- function(files, ...) {

    suppressWarnings(
        raster::stack(files, quick = TRUE, ...)
    )
}

#' @export
.raster_read_stack.raster <- function(files,
                                      block = NULL, ...) {

    # create raster objects
    r_obj <- .raster_open_stack.raster(files = files, ...)

    # start read
    if (!purrr::is_null(block)) {
        # crop raster
        r_obj <- .raster_crop.raster(r_obj = r_obj, block = block)
    }

    # read values
    return(as.matrix(raster::values(x = r_obj, ...)))
}

#' @keywords internal
#' @export
.raster_crop.raster <- function(r_obj, block, ...) {

    # compute extent to be cropped
    extent <- raster::extent(
        x = r_obj,
        c1 = block[["first_col"]],
        c2 = block[["first_col"]] + block[["ncols"]] - 1,
        r1 = block[["first_row"]],
        r2 = block[["first_row"]] + block[["nrows"]] - 1
    )

    # crop raster
    suppressWarnings(
        raster::crop(x = r_obj, y = extent)
    )
}

#' @keywords internal
#' @export
.raster_nrows.raster <- function(r_obj, ...) {

    raster::nrow(x = r_obj)
}

#' @keywords internal
#' @export
.raster_ncols.raster <- function(r_obj, ...) {

    raster::ncol(x = r_obj)
}

#' @keywords internal
#' @export
.raster_nlayers.raster <- function(r_obj, ...) {

    raster::nlayers(x = r_obj)
}

#' @keywords internal
#' @export
.raster_xmax.raster <- function(r_obj, ...) {

    raster::xmax(x = r_obj)
}

#' @keywords internal
#' @export
.raster_xmin.raster <- function(r_obj, ...) {

    raster::xmin(x = r_obj)
}

#' @keywords internal
#' @export
.raster_ymax.raster <- function(r_obj, ...) {

    raster::ymax(x = r_obj)
}

#' @keywords internal
#' @export
.raster_ymin.raster <- function(r_obj, ...) {

    raster::ymin(x = r_obj)
}

#' @keywords internal
#' @export
.raster_xres.raster <- function(r_obj, ...) {

    raster::xres(x = r_obj)
}

#' @keywords internal
#' @export
.raster_yres.raster <- function(r_obj, ...) {

    raster::yres(x = r_obj)
}

#' @keywords internal
#' @export
.raster_crs.raster <- function(r_obj, ...) {

    suppressWarnings(
        as.character(raster::crs(x = r_obj))
    )
}

#' @keywords internal
#' @export
.raster_freq.raster <- function(r_obj, ...) {

    res <- raster::freq(x = r_obj)
    if (is.list(res)) {
        res <- lapply(seq_along(res), function(i) {
            cbind(i, res[[i]])
        })
        res <- do.call(rbind, args = res)
    } else {
        res <- cbind(1, res)
    }
    return(res)
}

#' @keywords internal
#' @export
.raster_focal.raster <- function(r_obj,
                                 window_size,
                                 fn, ...) {

    # set caller to show in errors
    .check_set_caller(".raster_focal.raster")

    # check fun parameter
    if (is.character(fn)) {

        if (fn == "modal")
            fn <- raster::modal
    }

    # check r_obj type
    if (inherits(r_obj, "RasterBrick")) {

        .check_that(
            x = raster::nlayers(r_obj) == 1,
            msg = "cannot use focal function in a RasterBrick object."
        )

        r_obj <- r_obj[[1]]
    }

    suppressWarnings(
        raster::focal(
            x   = r_obj,
            w   = matrix(1, nrow = window_size, ncol = window_size),
            fun = fn,
            na.rm = TRUE,
            pad = TRUE,
            padValue = NA, ...
        )
    )
}

#' @keywords internal
#' @export
.raster_resample_methods.raster <- function(sits_names = TRUE) {

    # raster package resample names
    return(c("ngb", "bilinear"))
}

