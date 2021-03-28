
#' @keywords internal
#' @export
.sits_raster_api_check_package.raster_package <- function() {

    # package namespace
    pkg_name <- "raster"

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
.sits_raster_api_get_values.raster_package <- function(r_obj, ...) {

    unname(as.matrix(raster::values(x = r_obj, ...)))
}

#' @keywords internal
#' @export
.sits_raster_api_set_values.raster_package <- function(r_obj, values, ...) {

    raster::values(x = r_obj) <- as.matrix(values)

    return(invisible(r_obj))
}

#' @keywords internal
#' @export
.sits_raster_api_extract.raster_package <- function(r_obj, xy, ...) {

    as.matrix(raster::extract(x = r_obj,
                              y = xy,
                              fun = NULL,
                              cellnumber = FALSE, ...))
}

#' @keywords internal
#' @export
.sits_raster_api_rast.raster_package <- function(r_obj, nlayers = 1, ...) {

    suppressWarnings(
        raster::brick(x = r_obj, values = FALSE, nl = nlayers, ...)
    )
}

#' @keywords internal
#' @export
.sits_raster_api_open_rast.raster_package <- function(file, ...) {

    # check for file length == 1
    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_open_rast: more than one file were informed"
    )

    suppressWarnings(
        raster::brick(x = file, ...)
    )
}

#' @keywords internal
#' @export
.sits_raster_api_read_rast.raster_package <- function(file,
                                                      extent = NULL, ...) {

    # check for files length == 1
    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_read_rast: more than one file were informed"
    )

    return(.sits_raster_api_read_stack.raster_package(files = file,
                                                      extent = extent))

}

#' @keywords internal
#' @export
.sits_raster_api_write_rast.raster_package <- function(r_obj,
                                                       file,
                                                       format,
                                                       data_type,
                                                       options,
                                                       overwrite, ...) {

    suppressWarnings(
        raster::writeRaster(
            x         = r_obj,
            filename  = file,
            format    = format,
            datatype  = data_type,
            options   = options,
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
.sits_raster_api_new_rast.raster_package <- function(nrows,
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
.sits_raster_api_open_stack.raster_package <- function(files, ...) {

    # check for files length > 0
    assertthat::assert_that(
        length(files) > 0,
        msg = ".sits_raster_api_open_stack: no file informed"
    )

    suppressWarnings(
        raster::stack(files, quick = TRUE, ...)
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
.sits_raster_api_read_stack.raster_package <- function(files,
                                                       extent = NULL, ...) {

    # create raster objects
    r_obj <- .sits_raster_api_open_stack.raster_package(files = files, ...)

    # start read
    if (!purrr::is_null(extent)) {

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

        # crop raster
        r_obj <- .sits_raster_api_crop(r_obj = r_obj, extent = extent)

    }

    # read values
    return(as.matrix(raster::values(x = r_obj, ...)))
}

#' @keywords internal
#' @export
.sits_raster_api_crop.raster_package <- function(r_obj, extent, ...) {

    assertthat::assert_that(
        c("row", "nrows", "col", "ncols") %in% names(extent),
        msg = ".sits_raster_api_crop: invalid extent parameter"
    )

    # generate range values
    ext <- raster::extent(
        x = r_obj,
        c1 = extent[["col"]],
        c2 = extent[["col"]] + extent[["ncols"]],
        r1 = extent[["row"]],
        r2 = extent[["row"]] + extent[["nrows"]]
    )

    # crop raster
    suppressWarnings(
        raster::crop(x = r_obj, y = ext)
    )
}

#' @keywords internal
#' @export
.sits_raster_api_nrows.raster_package <- function(r_obj, ...) {

    raster::nrow(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_ncols.raster_package <- function(r_obj, ...) {

    raster::ncol(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_nlayers.raster_package <- function(r_obj, ...) {

    raster::nlyr(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_xmax.raster_package <- function(r_obj, ...) {

    raster::xmax(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_xmin.raster_package <- function(r_obj, ...) {

    raster::xmin(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_ymax.raster_package <- function(r_obj, ...) {

    raster::ymax(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_ymin.raster_package <- function(r_obj, ...) {

    raster::ymin(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_xres.raster_package <- function(r_obj, ...) {

    raster::xres(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_yres.raster_package <- function(r_obj, ...) {

    raster::yres(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_crs.raster_package <- function(r_obj, ...) {

    suppressWarnings(
        as.character(raster::crs(x = r_obj))
    )
}

#' @keywords internal
#' @export
.sits_raster_api_freq.raster_package <- function(r_obj, ...) {

    raster::freq(x = r_obj)
}

#' @keywords internal
#' @export
.sits_raster_api_focal.raster_package <- function(r_obj,
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
            fun <- raster::modal
    }

    suppressWarnings(
        raster::focal(
            x   = r_obj,
            w   = matrix(1, nrow = window_size, ncol = window_size),
            fun = fun, ...
        )
    )
}
