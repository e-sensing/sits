.sits_raster_api_check_package <- function(pkg) {

    UseMethod(".sits_raster_api_check_package", pkg)

}

.sits_raster_api_values <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_values", pkg_class)
}

`.sits_raster_api_values<-` <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_values<-", pkg_class)
}

.sits_raster_api_extract <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_extract", pkg_class)
}

.sits_raster_api_rast <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_rast", pkg_class)
}

.sits_raster_api_open_rast <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_open_rast", pkg_class)
}

.sits_raster_api_read_rast <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_read_rast", pkg_class)
}

.sits_raster_api_write_rast <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_write_rast", pkg_class)
}

.sits_raster_api_new_rast <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_new_rast", pkg_class)
}

.sits_raster_api_open_stack <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_open_stack", pkg_class)
}

.sits_raster_api_read_stack <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_read_stack", pkg_class)
}

.sits_raster_api_crop <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_crop", pkg_class)
}

.sits_raster_api_nrows <- function(r_obj, ...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_nrows", pkg_class)
}

.sits_raster_api_ncols <- function(r_obj, ...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_ncols", pkg_class)
}

.sits_raster_api_nlayers <- function(r_obj, ...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_nlayers", pkg_class)
}

.sits_raster_api_xmax <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_xmax", pkg_class)
}

.sits_raster_api_xmin <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_xmin", pkg_class)
}

.sits_raster_api_ymax <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_ymax", pkg_class)
}

.sits_raster_api_ymin <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_ymin", pkg_class)
}

.sits_raster_api_xres <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_xres", pkg_class)
}

.sits_raster_api_yres <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_yres", pkg_class)
}

.sits_raster_api_crs <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_crs", pkg_class)
}

.sits_raster_api_freq <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_freq", pkg_class)
}

.sits_raster_api_focal <- function(...) {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    # check package
    .sits_raster_api_check_package(pkg_class)

    UseMethod(".sits_raster_api_focal", pkg_class)
}

.sits_raster_api_package_error <- function()
    stop("No API defined for this raster package.")

.sits_raster_api_check_package.default <- function(...)
    .sits_raster_api_package_error()

.sits_raster_api_check_package.terra_package <- function(...) {

    pkg_name <- "terra"

    # check if raster package is available
    if (!requireNamespace(pkg_name)) {

        stop(paste(".sits_config_raster_package: package", pkg_name,
                   "not available. Install the package or change the",
                   "config file."), call. = FALSE)
    }

    return(invisible(NULL))
}

.sits_raster_api_values.terra_package <- function(r_obj, ...) {

    terra::values(x = r_obj, mat = TRUE, ...)
}

`.sits_raster_api_values<-.terra_package` <- function(r_obj, value, ...) {

    terra::values(x = r_obj) <- value
}

.sits_raster_api_extract.terra_package <- function(r_obj, xy, ...) {

    terra::extract(x = r_obj, y = xy, fun = NULL, cells = FALSE, ...)
}

.sits_raster_api_rast.terra_package <- function(r_obj, nlayers = 1, ...) {

    suppressWarnings(terra::rast(x = r_obj, nlyrs = nlayers, ...))
}

.sits_raster_api_open_rast.terra_package <- function(file, ...) {

    # check for file length == 1
    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_open_rast: more than one file were informed"
    )

    suppressWarnings(terra::rast(x = file, ...))
}

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

.sits_raster_api_open_stack.terra_package <- function(files, ...) {

    # check for files length > 0
    assertthat::assert_that(
        length(files) > 0,
        msg = ".sits_raster_api_open_stack: no file informed"
    )

    suppressWarnings(terra::rast(files, ...))
}

#' @title Read a part of a raster file and return a matrix
#' @name .sits_raster_api_read_extent
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  files          Files associated to the raster object
#' @param  extent         Image extent to be read.
#' @return                Data.table of values
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

.sits_raster_api_crop.terra_package <- function(r_obj, extent, ...) {

    assertthat::assert_that(
        c("row", "nrows", "col", "ncols") %in% names(extent),
        msg = ".sits_raster_api_crop: invalid extent parameter"
    )

    x1 <- terra::xFromCol(object = r_obj,
                          col    = c(extent[["col"]]))
    x2 <- terra::xFromCol(object = r_obj,
                          col    = extent[["col"]] + extent[["ncols"]])
    y1 <- terra::yFromRow(object = r_obj,
                          row    = c(extent[["row"]]))
    y2 <- terra::yFromRow(object = r_obj,
                          row    = extent[["row"]] + extent[["nrows"]])

    # xmin, xmax, ymin, ymax
    extent <- terra::ext(x = c(min(x1, x2),
                               max(x1, x2),
                               min(y1, y2),
                               max(y1, y2)))

    terra::crop(x = r_obj, y = extent)
}

.sits_raster_api_nrows.terra_package <- function(r_obj, ...) {

    terra::nrow(x = r_obj)
}

.sits_raster_api_ncols.terra_package <- function(r_obj, ...) {

    terra::ncol(x = r_obj)
}

.sits_raster_api_nlayers.terra_package <- function(r_obj, ...) {

    terra::nlyr(x = r_obj)
}

.sits_raster_api_xmax.terra_package <- function(r_obj, ...) {

    terra::xmax(x = r_obj)
}

.sits_raster_api_xmin.terra_package <- function(r_obj, ...) {

    terra::xmin(x = r_obj)
}

.sits_raster_api_ymax.terra_package <- function(r_obj, ...) {

    terra::ymax(x = r_obj)
}

.sits_raster_api_ymin.terra_package <- function(r_obj, ...) {

    terra::ymin(x = r_obj)
}

.sits_raster_api_xres.terra_package <- function(r_obj, ...) {

    terra::xres(x = r_obj)
}

.sits_raster_api_yres.terra_package <- function(r_obj, ...) {

    terra::yres(x = r_obj)
}

.sits_raster_api_crs.terra_package <- function(r_obj, ...) {

    suppressWarnings(
        as.character(terra::crs(x = r_obj))
    )
}

.sits_raster_api_freq.terra_package <- function(r_obj, ...) {

    terra::freq(x = r_obj)
}

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

    return(invisible(NULL))
}

#' @title Determine the file params to write in the metadata
#' @name .sits_raster_api_params_file
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its params
#' @param file     A valid raster image
#' @return A tibble with the cube params
.sits_raster_api_params_file <- function(file) {

    # open file
    r_obj <- .sits_raster_api_open_rast(file = file)

    params <- tibble::tibble(
        nrows = .sits_raster_api_nrows(r_obj = r_obj),
        ncols = .sits_raster_api_ncols(r_obj = r_obj),
        xmin  = .sits_raster_api_xmin(r_obj = r_obj),
        xmax  = .sits_raster_api_xmax(r_obj = r_obj),
        ymin  = .sits_raster_api_ymin(r_obj = r_obj),
        ymax  = .sits_raster_api_ymax(r_obj = r_obj),
        xres  = .sits_raster_api_xres(r_obj = r_obj),
        yres  = .sits_raster_api_yres(r_obj = r_obj),
        crs   = .sits_raster_api_crs(r_obj = r_obj)
    )

    return(params)
}
#' @title Given a band, return a set of values for chosen location
#' @name .sits_cube_extract
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          Given a data cube, retrieve the time series
#'                       of XY locations
#'
#' @param cube           Metadata about a data cube
#' @param band_cube      Name of the band to the retrieved
#' @param xy             Matrix with XY location
#' @return               Matrix with values extracted from image files
#'
.sits_cube_extract <- function(cube, band_cube, xy) {


    # precondition 2
    assertthat::assert_that(
        band_cube %in% sits_bands(cube),
        msg = paste(".sits_raster_api_extract: band", band_cube,
                    "is not available in the cube ", cube$name)
    )

    # filter the files that contain the band
    band <- dplyr::filter(cube$file_info[[1]], band == band_cube)

    # create a stack object
    r_obj <- .sits_raster_api_open_stack(band$path)

    # extract the values
    values <- .sits_raster_api_extract(r_obj, xy)

    # is the data valid?
    assertthat::assert_that(
        nrow(values) == nrow(xy),
        msg = ".sits_raster_api_extract: error in retrieving data"
    )
    return(values)
}

#' @title Given a labelled cube, return the band information
#' @name .sits_cube_area_freq
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Metadata about a data cube
#' @return               Frequency of each label in the data cube
#'
.sits_cube_area_freq <- function(cube) {

    # precondition
    assertthat::assert_that(
        inherits(cube, "classified_image"),
        msg = ".sits_cube_area_freq: requires a labelled cube"
    )

    # retrieve the r object associated to the labelled cube
    file_info <- cube$file_info[[1]]

    # open first raster
    r_obj <- .sits_raster_api_open_rast(file_info$path[[1]])

    # retrieve the frequency
    freq <- tibble::as_tibble(.sits_raster_api_freq(r_obj))

    return(freq)
}

#' @title Merge all input files into one raster file
#' @name .sits_raster_api_merge
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param in_files       Input file paths
#' @param out_file       Output raster file path
#' @param datatype       Data type
#' @param format         Format to write the file
#' @param compress       Compression method to be used
#' @param filename       File name of the raster image file.
#' @param overwrite      Overwrite the file?
#'
#' @return Output file path
#'
.sits_raster_api_merge <- function(in_files,
                                   out_file,
                                   datatype,
                                   format = "GTiff",
                                   compress = "LZW",
                                   overwrite = TRUE) {

    # precondition
    assertthat::assert_that(
        all(file.exists(in_files)),
        msg = ".sits_raster_api_merge: file does not exist"
    )

    # retrieve the r object associated to the labelled cube
    gdalUtilities::gdalwarp(srcfile = in_files, dstfile = out_file,
                            ot = datatype, of = format,
                            co = paste0("COMPRESS=", compress),
                            overwrite = overwrite)

    return(out_file)
}
