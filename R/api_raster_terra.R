#' @title Check that terra package is available
#' @keywords internal
#' @noRd
#' @return Called for side effects
#' @export
.raster_check_package.terra <- function() {
    # package namespace
    pkg_name <- "terra"

    # check if terra package is available
    .check_require_packages(pkg_name)

    class(pkg_name) <- pkg_name

    return(invisible(pkg_name))
}
#' @title Get values of a terra object
#' @keywords internal
#' @noRd
#' @param r_obj Terra raster object
#' @param ...   Other parameters for terra functions
#' @return      Values from terra raster object
#' @export
.raster_get_values.terra <- function(r_obj, ...) {
    # read values and close connection
    terra::readStart(x = r_obj)
    res <- terra::readValues(x = r_obj, mat = TRUE, ...)
    terra::readStop(x = r_obj)
    return(res)
}
#' @title Set values of a terra object
#' @keywords internal
#' @noRd
#' @param r_obj Terra raster object
#' @param values Values to be set in raster object
#' @param ...   Other parameters for terra functions
#' @return      Terra raster object with new values
#' @export
.raster_set_values.terra <- function(r_obj, values, ...) {
    terra::values(x = r_obj) <- as.matrix(values)

    return(r_obj)
}
#' @title Extract values from terra object based on XY matrix
#' @keywords internal
#' @noRd
#' @param r_obj Terra raster object
#' @param xy    Matrix with XY positions
#' @param ...   Other parameters for terra functions
#' @return      Values extracted from terra raster object
#' @export
.raster_set_na.terra <- function(r_obj, na_value, ...) {
    terra::NAflag(x = r_obj) <- na_value

    return(r_obj)
}

#' @keywords internal
#' @noRd
#' @export
.raster_extract.terra <- function(r_obj, xy, ...) {
    terra::extract(x = r_obj, y = xy, ...)
}
#' @title Get block size from terra object
#' @keywords internal
#' @noRd
#' @param r_obj Terra raster object
#' @return      Block size extracted from terra raster object
#' @export
.raster_file_blocksize.terra <- function(r_obj) {
    block_size <- c(terra::fileBlocksize(r_obj[[1]]))
    names(block_size) <- c("nrows", "ncols")
    return(block_size)
}
#' @title Create a new raster object from an existing one
#' @keywords internal
#' @noRd
#' @param r_obj      Terra raster object
#' @param nlayers    Number of layers in terra object
#' @param ...        Other parameters for terra functions
#' @return           New terra raster object
#' @export
.raster_rast.terra <- function(r_obj, nlayers = 1, ...) {
    suppressWarnings(
        terra::rast(x = r_obj, nlyrs = nlayers, ...)
    )

}
#' @title Open a raster object based on a file
#' @keywords internal
#' @noRd
#' @param file       Raster file
#' @param ...        Other parameters for terra functions
#' @return           Terra raster object
#' @export
.raster_open_rast.terra <- function(file, ...) {
    r_obj <- suppressWarnings(
        terra::rast(x = .file_path_expand(file), ...)
    )
    .check_null_parameter(r_obj)
    # remove gain and offset applied by terra
    terra::scoff(r_obj) <- NULL
    r_obj
}
#' @title Write values to a terra raster object based on a file
#' @keywords internal
#' @noRd
#' @param r_obj      Terra raster object
#' @param file       Raster file
#' @param data_type  Data type of terra object
#' @param overwrite  Overwrite if file exists?
#' @param ...        Other parameters for terra functions
#' @param missing_value  Missing data value
#' @return           Called for side effects
#' @export
.raster_write_rast.terra <- function(r_obj,
                                     file,
                                     data_type,
                                     overwrite, ...,
                                     missing_value = NA) {
    # set caller to show in errors
    .check_set_caller(".raster_write_rast_terra")

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
    .check_file(file)
    return(invisible(r_obj))
}
#' @title Create raster object
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param nrows         Number of rows in the raster
#' @param ncols         Number of columns in the raster
#' @param xmin          X minimum of raster origin
#' @param xmax          X maximum of raster origin
#' @param ymin          Y minimum of raster origin
#' @param ymax          Y maximum of raster origin
#' @param nlayers       Number of layers of the raster
#' @param crs           Coordinate Reference System of the raster
#' @param ...           additional parameters to be passed to raster package
#' @param xres          X resolution
#' @param yres          Y resolution
#' @return              R object created by terra package
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
    # create new raster object if resolution is not provided
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
#' @title Read raster file
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param file    path to raster file(s) to be read
#' @param ...     additional parameters to be passed to terra package
#' @param block   a valid block with (\code{col}, \code{row},
#'                \code{ncols}, \code{nrows}).
#' @return Numeric matrix read from file based on parameter block
#' @export
.raster_read_rast.terra <- function(files, ..., block = NULL) {
    # create raster objects
    r_obj <- .raster_open_rast.terra(file = path.expand(files), ...)

    # start read
    if (.has_not(block)) {
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
#' @title Crop raster function
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj         Raster package object to be written
#' @param file          File name to save cropped raster.
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param overwrite     logical indicating if file can be overwritten
#' @param block         a valid block with (\code{col}, \code{row},
#'                      \code{ncols}, \code{nrows}).
#' @param missing_value A \code{integer} with image's missing value
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
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
#' @title Crop raster function
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object to be written
#' @param ...     additional parameters to be passed to raster package
#' @param block   a valid block with (\code{col}, \code{row},
#'                \code{ncols}, \code{nrows}).
#' @param bbox    numeric vector with (\code{xmin}, \code{xmax},
#'                \code{ymin}, \code{ymax}).
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
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
#' @title Raster package internal object properties
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object
#' @param ...      additional parameters to be passed to raster package
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
.raster_scale.terra <- function(r_obj, ...) {
    # check value
    i <- 1
    while (is.na(r_obj[i])) {
        i <- i + 1
    }
    value <- r_obj[i]
    if (value > 1.0 && value <= 10000)
        scale_factor <- 0.0001
    else
        scale_factor <- 1.0
    return(scale_factor)
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
#' @title Frequency values of terra object
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @param r_obj    raster package object to count values
#' @param ...      additional parameters to be passed to raster package
#'
#' @return matrix with layer, value, and count column
#' @export
#'
.raster_freq.terra <- function(r_obj, ...) {
    terra::freq(x = r_obj, bylayer = TRUE)
}

#' @title Raster package internal raster data type
#' @name .raster_datatype
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param r_obj     raster package object
#' @param by_layer  A logical value indicating the type of return
#' @param ...      additional parameters to be passed to raster package
#'
#' @return A character value with data type
.raster_datatype.terra <- function(r_obj, ..., by_layer = TRUE) {
    terra::datatype(x = r_obj, bylyr = by_layer)
}

#' @title Summary values of terra object
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @param r_obj    raster package object to count values
#' @param ...      additional parameters to be passed to raster package
#'
#' @return matrix with layer, value, and count column
#' @export
.raster_summary.terra <- function(r_obj, ...) {
    terra::summary(r_obj, ...)
}

#' @title Return col value given an X coordinate
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj  raster package object
#' @param x      X coordinate in raster projection
#'
#' @return integer with column
#' @export
.raster_col.terra <- function(r_obj, x) {
    terra::colFromX(r_obj, x)
}

#' @title Return row value given an Y coordinate
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj  raster object
#' @param y      Y coordinate in raster projection
#'
#' @return integer with row number
#' @export
.raster_row.terra <- function(r_obj, y) {
    terra::rowFromY(r_obj, y)
}

#' @keywords internal
#' @noRd
#' @export
.raster_extract_polygons.terra <- function(r_obj, dissolve = TRUE, ...) {
    terra::as.polygons(r_obj, dissolve = TRUE, ...)
}
