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

    # create a terra object
    t_obj <- terra::rast(file)

    # post conditions
    assertthat::assert_that(
        terra::nrow(t_obj) > 0 && terra::ncol(t_obj) > 0,
        msg = ".sits_raster_api_params_file: invalid raster object"
    )
    assertthat::assert_that(
        terra::xmax(t_obj) > terra::xmin(t_obj) &&
            terra::ymax(t_obj) > terra::ymin(t_obj),
        msg = ".sits_raster_api_params_file: invalid raster object"
    )
    assertthat::assert_that(
        terra::xres(t_obj) > 0 && terra::yres(t_obj) > 0,
        msg = ".sits_raster_api_params_file: invalid raster object"
    )

    params <- tibble::tibble(
        nrows = terra::nrow(t_obj),
        ncols = terra::ncol(t_obj),
        xmin  = terra::xmin(t_obj),
        xmax  = terra::xmax(t_obj),
        ymin  = terra::ymin(t_obj),
        ymax  = terra::ymax(t_obj),
        xres  = terra::xres(t_obj),
        yres  = terra::yres(t_obj),
        crs   = as.character(suppressWarnings(terra::crs(t_obj)))
    )

    return(params)
}
#' @title Determine the cube params to write in the metadata
#' @name .sits_raster_api_params_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its params
#' @param file     A valid raster image
#' @return A tibble with the cube params
.sits_raster_api_params_cube <- function(cube) {

    params <- tibble::tibble(
        nrows = cube$nrows,
        ncols = cube$ncols,
        xmin  = cube$xmin,
        xmax  = cube$xmax,
        ymin  = cube$ymin,
        ymax  = cube$ymax,
        xres  = cube$xres,
        yres  = cube$yres,
        crs   = cube$crs
    )
    return(params)
}
#' @title Check if the raster files are accessible by GDAL
#' @name .sits_raster_api_check_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  file         file associated to the raster data
#' @return TRUE         true if files are accessible
.sits_raster_api_check_access <- function(file) {

    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_check_access: works with single files"
    )

    # verify if all files are reachable
    r <- suppressWarnings(terra::rast(file))
    assertthat::assert_that(
        all(!purrr::is_null(r)),
        msg = paste(".sits_raster_api_check_access: file", file,
                    "cannot be accessed")
    )
    return(TRUE)
}

#' @title Given a band, return a set of values for chosen location
#' @name .sits_raster_api_extract
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          Given a data cube, retrieve the timeline
#'                       (uses the terra package which is faster than raster)
#' @param cube           Metadata about a data cube
#' @param band_cube      Name of the band to the retrieved
#' @param xy             Matrix with XY location
#' @return               Matrix with values extracted from image files
#'
.sits_raster_api_extract <- function(cube, band_cube, xy) {

    # preconditions
    assertthat::assert_that(
        band_cube %in% sits_bands(cube),
        msg = paste0(
            ".sits_raster_api_extract: band ",
            band_cube,
            " is not available in the cube ",
            cube$name
        )
    )

    # filter the files that contain the band
    band <- dplyr::filter(cube$file_info[[1]], band == band_cube)

    # create a terra object
    rast <- suppressWarnings(terra::rast(band$path))

    # extract the values
    values <- terra::extract(rast, xy)

    # get the timeline
    timeline <- sits_timeline(cube)

    # terra includes an ID (remove it)
    if (ncol(values) > length(timeline))
        values <- values[, -1]

    # is the data valid?
    assertthat::assert_that(
        nrow(values) == nrow(xy),
        msg = ".sits_raster_api_extract: error in retrieving data"
    )
    return(values)
}
#' @title Read a part of a raster file and return a matrix
#' @name .sits_raster_api_read_extent
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  r_files        Files associated to the raster object
#' @param  extent         Image extent to be read.
#' @return                Data.table of values
.sits_raster_api_read_extent <- function(r_files, extent = NULL) {

    # create terra objects
    t_obj <- terra::rast(r_files)

    # start read
    terra::readStart(t_obj)
    if (purrr::is_null(extent)) {
        values  <- terra::readValues(x     = t_obj,
                                     mat   = TRUE)
    } else {
        values <- terra::readValues(x      = t_obj,
                                    row    = extent[["row"]],
                                    nrows  = extent[["nrows"]],
                                    col    = extent[["col"]],
                                    ncols  = extent[["ncols"]],
                                    mat    = TRUE)
    }
    # end read
    terra::readStop(t_obj)

    return(values)
}

#' @title Set the values of a raster object matrix
#' @name .sits_raster_api_write
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param params         Output file params
#' @param num_layers     Number of layers
#' @param values         Data.table with values to be written
#' @param datatype       Data type
#' @param format         Format to write the file
#' @param compress       Compression method to be used
#' @param filename       File name of the raster image file.
#' @param overwrite      Overwrite the file?
#'
#' @return               Data cube values.
.sits_raster_api_write <- function(params,
                                   num_layers,
                                   values,
                                   filename,
                                   datatype,
                                   format = "GTiff",
                                   compress = "LZW",
                                   overwrite = TRUE) {
    # preconditions
    assertthat::assert_that(
        num_layers >= 1,
        msg = ".sits_raster_api_write_raster: invalid number of layers"
    )

    # create a raster object
    r_obj <- suppressWarnings(
        terra::rast(
            nrows = params$nrows,
            ncols = params$ncols,
            nlyrs = num_layers,
            xmin = params$xmin,
            xmax = params$xmax,
            ymin = params$ymin,
            ymax = params$ymax,
            crs = params$crs
        )
    )

    assertthat::assert_that(
        terra::nrow(r_obj) == params$nrows,
        msg = ".sits_raster_api_write: unable to create raster object"
    )

    # include the values in the raster object
    terra::values(r_obj) <- as.matrix(values)

    # options for compression
    opt_comp <- paste0("COMPRESS=", compress)

    suppressWarnings(terra::writeRaster(
        r_obj,
        filename = filename,
        overwrite = overwrite,
        wopt = list(
            gdal = opt_comp,
            filetype = format,
            datatype = datatype
        )
    ))

    # was the file written correctly?
    assertthat::assert_that(
        file.exists(filename),
        msg = ".sits_raster_api_write_raster: unable to write raster object"
    )

    return(invisible(TRUE))
}


#' @title Given a labelled cube, return the band information
#' @name .sits_raster_api_area_freq
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Metadata about a data cube
#' @return               Frequency of each label in the data cube
#'
.sits_raster_api_area_freq <- function(cube) {
    # precondition
    assertthat::assert_that(
        inherits(cube, "classified_image"),
        msg = ".sits_raster_api_area_freq: requires a labelled cube"
    )
    # retrieve the r object associated to the labelled cube
    file_info <- cube$file_info[[1]]
    assertthat::assert_that(
        nrow(file_info) == 1,
        msg = ".sits_raster_api_area_freq: more than one classified image"
    )
    r_obj <- terra::rast(file_info$path)
    # retrieve the frequency
    freq <- tibble::as_tibble(terra::freq(r_obj))

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
