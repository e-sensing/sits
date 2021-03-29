#' @keywords internal
.sits_raster_api_check_package <- function() {

    pkg_class <- paste0(.sits_config_raster_package(), "_package")
    class(pkg_class) <- pkg_class

    UseMethod(".sits_raster_api_check_package", pkg_class)
}

#' @keywords internal
#' @export
.sits_raster_api_check_package.default <- function() {

    stop("No API defined for this raster package.")
}

#' @keywords internal
.sits_raster_api_get_values <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    # call function
    UseMethod(".sits_raster_api_get_values", pkg_class)
}

#' @keywords internal
.sits_raster_api_set_values <- function(r_obj, values, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_set_values", pkg_class)
}

#' @keywords internal
.sits_raster_api_extract <- function(r_obj, xy, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_extract", pkg_class)
}

#' @keywords internal
.sits_raster_api_rast <- function(r_obj, nlayers = 1, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_rast", pkg_class)
}

#' @keywords internal
.sits_raster_api_open_rast <- function(file, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_open_rast", pkg_class)
}

#' @keywords internal
.sits_raster_api_read_rast <- function(file,
                                       extent = NULL, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_read_rast", pkg_class)
}

#' @keywords internal
.sits_raster_api_write_rast <- function(r_obj,
                                        file,
                                        format,
                                        data_type,
                                        options,
                                        overwrite, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_write_rast", pkg_class)
}

#' @keywords internal
.sits_raster_api_new_rast <- function(nrows,
                                      ncols,
                                      xmin,
                                      xmax,
                                      ymin,
                                      ymax,
                                      nlayers,
                                      crs, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_new_rast", pkg_class)
}

#' @keywords internal
.sits_raster_api_open_stack <- function(files, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_open_stack", pkg_class)
}

#' @keywords internal
.sits_raster_api_read_stack <- function(files,
                                        extent = NULL, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_read_stack", pkg_class)
}

#' @keywords internal
.sits_raster_api_crop <- function(r_obj, extent, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_crop", pkg_class)
}

#' @keywords internal
.sits_raster_api_nrows <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_nrows", pkg_class)
}

#' @keywords internal
.sits_raster_api_ncols <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_ncols", pkg_class)
}

#' @keywords internal
.sits_raster_api_nlayers <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_nlayers", pkg_class)
}

#' @keywords internal
.sits_raster_api_xmax <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_xmax", pkg_class)
}

#' @keywords internal
.sits_raster_api_xmin <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_xmin", pkg_class)
}

#' @keywords internal
.sits_raster_api_ymax <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_ymax", pkg_class)
}

#' @keywords internal
.sits_raster_api_ymin <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_ymin", pkg_class)
}

#' @keywords internal
.sits_raster_api_xres <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_xres", pkg_class)
}

#' @keywords internal
.sits_raster_api_yres <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_yres", pkg_class)
}

#' @keywords internal
.sits_raster_api_crs <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_crs", pkg_class)
}

#' @keywords internal
.sits_raster_api_freq <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_freq", pkg_class)
}

#' @keywords internal
.sits_raster_api_focal <- function(r_obj,
                                   window_size,
                                   fun, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_focal", pkg_class)
}

#' @title Determine the file params to write in the metadata
#' @name .sits_raster_api_params_file
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its params
#' @param file     A valid raster image
#'
#' @return A tibble with the cube params
.sits_raster_api_params_file <- function(file) {

    # preconditions
    assertthat::assert_that(
        length(file) > 0,
        msg = ".sits_raster_api_params_file: no file was informed"
    )

    # use first file
    file <- file[[1]]

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
#' @param gdal_datatype  Data type in gdal format
#' @param format         Format to write the file
#' @param compress       Compression method to be used
#' @param filename       File name of the raster image file.
#' @param overwrite      Overwrite the file?
#'
#' @return Output file path
#'
.sits_raster_api_merge <- function(in_files,
                                   out_file,
                                   gdal_datatype,
                                   format = "GTiff",
                                   compress = "LZW",
                                   overwrite = TRUE) {

    # precondition
    assertthat::assert_that(
        all(file.exists(in_files)),
        msg = ".sits_raster_api_merge: file does not exist"
    )
    if (file.exists(out_file))
        unlink(out_file)

    # retrieve the r object associated to the labelled cube
    gdalUtilities::gdalwarp(srcfile = in_files, dstfile = out_file,
                            ot = gdal_datatype, of = format,
                            co = paste0("COMPRESS=", compress),
                            overwrite = overwrite)
    unlink(in_files)

    return(invisible(TRUE))
}

#' @title Determine the block params to write in the metadata
#' @name .sits_raster_api_params_block
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its params
#' @param cube     A valid cube
#' @param block    A block insider the cube
#' @return A tibble with the cube params
.sits_raster_api_params_block <- function(cube, block) {

    ymax  <-  cube$ymax - (block["row"] - 1)*(cube$yres)
    ymin  <-  ymax - block["nrows"]*(cube$yres)
    params <- tibble::tibble(
        nrows = unname(block["nrows"]),
        ncols = unname(block["ncols"]),
        xmin  = cube$xmin,
        xmax  = cube$xmax,
        ymin  = ymin,
        ymax  = ymax,
        xres  = cube$xres,
        yres  = cube$yres,
        crs   = cube$crs
    )
    return(params)
}
