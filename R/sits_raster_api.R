#' @title Determine the cube params to write in the metadata
#' @name .sits_raster_api_params
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its params
#' @param file     A valid raster image
#' @return A tibble with the cube params
.sits_raster_api_params <- function(file) {

    rg_obj <- suppressWarnings(rgdal::GDALinfo(file))

    # extract parameters from gdal
    nrows <- as.numeric(rg_obj["rows"])
    ncols <- as.numeric(rg_obj["columns"])
    xres  <- as.numeric(rg_obj["res.x"])
    yres  <- as.numeric(rg_obj["res.y"])
    xmin  <- as.numeric(rg_obj["ll.x"])
    ymin  <- as.numeric(rg_obj["ll.y"])
    xmax  <- floor(xmin + ncols*xres + 0.5)
    ymax  <- floor(ymin + nrows*yres + 0.5)
    crs   <- attr(rg_obj,"projection")

    if (0) {
        # using gdalUtils instead
        r <- jsonlite::fromJSON(gdalUtils::gdalinfo(datasetname = file,
                                                    json = TRUE,
                                                    proj4 = TRUE))


        # extract parameters from gdal
        nrows <- r$size[1]
        ncols <- r$size[2]
        xres  <- r$geoTransform[2]
        yres  <- r$geoTransform[2]
        xmin  <- r$cornerCoordinates$lowerLeft[1]
        ymin  <- r$cornerCoordinates$lowerLeft[2]
        xmax  <- r$cornerCoordinates$upperRight[1]
        ymax  <- r$cornerCoordinates$upperRight[2]
        crs   <- r$coordinateSystem$proj4
    }

    # precondition
    assertthat::assert_that(nrows > 0 & ncols > 0,
                msg = ".sits_raster_api_params: invalid raster object")

    params.tb <- tibble::tibble(
        nrows = nrows,
        ncols = ncols,
        xres  = xres,
        yres  = yres,
        xmin  = xmin,
        ymin  = ymin,
        xmax  = xmax,
        ymax  = ymax,
        crs   = crs)
    return(params.tb)
}

#' @title Check if file is a brick
#' @name .sits_raster_api_check_brick
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Check if file is a valid raster brick
#' @param file     A valid raster image
#' @return TRUE/FALSE
#'
.sits_raster_api_check_brick <- function(file) {
    # temporarily disabled because of rgdal bug in macOS BigSur

    rg_obj <- suppressWarnings(rgdal::GDALinfo(file))

    # object parameters
    bands <- as.numeric(rg_obj["bands"])
    nrows <- as.numeric(rg_obj["rows"])
    ncols <- as.numeric(rg_obj["columns"])

    if (0) {
        # using gdalUtils instead
        r <- jsonlite::fromJSON(gdalUtils::gdalinfo(datasetname = file,
                                                    json = TRUE,
                                                    proj4 = TRUE))

        # object parameters
        nbands <- nrow(r$bands)
        nrows  <- r$size[1]
        ncols  <- r$size[2]
    }

    assertthat::assert_that(bands >= 1 & ncols > 1 & nrows > 1,
                            msg = ".sits_raster_brick_check: bad raster data")

    return(bands > 1)
}
#' @title Check if the raster files are accessible by GDAL
#' @name .sits_raster_api_check_gdal_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  file         file associated to the raster data
#' @return TRUE         true if filles are acessible
.sits_raster_api_check_gdal_access <- function(file){

    assertthat::assert_that(length(file) == 1,
                            msg = ".sits_raster_api_check_gdal_access works with single files")

    # verify if all files are reacheable
    r <- suppressWarnings(rgdal::GDALinfo(file, silent = FALSE))
    assertthat::assert_that(all(!purrr::is_null(r)),
                            msg = "sits_cube: raster files cannot be accessed")
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
#' @return               Tibble with values extracted from image files
#'
.sits_raster_api_extract <- function(cube, band_cube, xy) {

    # preconditions
    assertthat::assert_that(band_cube %in% sits_bands(cube),
            msg = paste0(".sits_cube_raster_api_extract: band ", band_cube,
                        " is not available in the cube ", cube$name))

    # filter the files that contain the band
    band.tb <- dplyr::filter(cube$file_info[[1]], band == band_cube)
    # create a terra object
    rast <- suppressWarnings(terra::rast(band.tb$path))
    # extract the values
    values <- tibble::as_tibble(terra::extract(rast, xy))
    # terra includes an ID (remove it)
    values <- values[,-1]

    # is the data valid?
    assertthat::assert_that(nrow(values) > 0,
            msg = ".sits_raster_api_extract - no data retrieved")
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
.sits_raster_api_read_extent <- function(r_files, extent) {

    # verify if the files can be read
    rg_obj <- suppressWarnings(rgdal::GDALinfo(r_files[1]))
    assertthat::assert_that(!purrr::is_null(rg_obj),
                            msg = ".sits_raster_api_read_extent; invalid file")
    # get number of bands in the file
    n_bands_file <- rg_obj["bands"]

    assertthat::assert_that(n_bands_file >= 1,
                            msg = ".sits_raster_api_read_extent; invalid number of bands")

    # set the offset and the region dimensions (rgdal starts in (0,0))
    first_row <- extent["row"] - 1
    first_col <- extent["col"] - 1

    # we have to consider three situations
    # (1) one file, one band per file (a.k.a. "raster layer")
    # (2) one file, many bands per file (a.k.a. "raster brick")
    # (3) many files, one bands per file (a.k.a. "raster stack")

    if (length(r_files) == 1) {
        if (n_bands_file == 1) {
            # case (1) - "raster layer"
            values.mx <- matrix(as.matrix(
                        suppressWarnings(
                            rgdal::readGDAL(fname = r_files[1],
                                            offset = c(first_row, first_col),
                                            region.dim = c(extent["nrows"], extent["ncols"]),
                                            silent = TRUE)@data)),
                    nrow = extent["nrows"], byrow = TRUE)

            assertthat::assert_that(nrow(values.mx) == extent["nrows"] &
                                    ncol(values.mx) == extent["ncols"],
                        msg = ".sits_raster_api_read_extent: error in reading a raster file")
        }
        else {
            # case (2) - "raster brick"
            values.mx <- as.matrix(
                suppressWarnings(
                    rgdal::readGDAL(fname = r_files[1],
                                    offset = c(first_row, first_col),
                                    region.dim = c(extent["nrows"], extent["ncols"]),
                                    silent = TRUE)@data)
            )
            assertthat::assert_that(nrow(values.mx) == extent["nrows"]*extent["ncols"] &
                                    ncol(values.mx) == n_bands_file,
                        msg = ".sits_raster_api_read_extent: error in reading a raster file")
        }
    }
    else {
        # case (3) - "raster stack"
        values.lst <- purrr::map(r_files, function(f){
                data <- as.matrix(
                    suppressWarnings(
                        rgdal::readGDAL(fname = f,
                                        offset = c(first_row, first_col),
                                        region.dim = c(extent["nrows"], extent["ncols"]),
                                        silent = TRUE)@data))
        })
        values.mx <- do.call(cbind, values.lst)
        assertthat::assert_that(nrow(values.mx) == extent["nrows"]*extent["ncols"] &
                                ncol(values.mx) == length(r_files),
                        msg = ".sits_raster_api_read_extent: error in reading a raster file")
    }

    return(values.mx)
}


#' @title Set the values of a raster object matrix
#' @name .sits_raster_api_write
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Data cube
#' @param num_layers     Number of layers
#' @param values         Data.table with values to be written
#' @param filename       Filename of the raster image file
#' @param datatype       Data type
#' @param format         Format to write the file
#' @param compress       Compression method to be used
#' @param overwrite      Overwrite the file
#' @return               Data cube
#'
.sits_raster_api_write <-  function(cube,
                                    num_layers,
                                    values,
                                    filename,
                                    datatype,
                                    format     = "GTiff",
                                    compress   = "LZW",
                                    overwrite  = TRUE){
    # preconditions
    assertthat::assert_that(num_layers >= 1,
                msg = ".sits_raster_api_write_raster: invalid number of layers")

    # create a raster object
    r_obj <- suppressWarnings(terra::rast(nrows  = cube$nrows,
                                          ncols  = cube$ncols,
                                          nlyrs  = num_layers,
                                          xmin   = cube$xmin,
                                          xmax   = cube$xmax,
                                          ymin   = cube$ymin,
                                          ymax   = cube$ymax,
                                          crs    = cube$crs))

    assertthat::assert_that(terra::nrow(r_obj) == cube$nrows,
                msg = ".sits_raster_api_write: unable to create raster object")

    # include the values in the raster object
    terra::values(r_obj) <- as.matrix(values)

    # options for compression
    opt_comp <- paste0("COMPRESS =", compress)

    suppressWarnings(terra::writeRaster(r_obj,
                                         filename  = filename,
                                         overwrite = overwrite,
                                         wopt      = list(
                                                     gdal = opt_comp,
                                                     format    = format,
                                                     datatype  = datatype)
                                         ))

    # was the file written correctly?
    assertthat::assert_that(file.info(filename)$size > 0,
        msg = ".sits_raster_api_write_raster: unable to wriye raster object")

    return(cube)
}

#' @title Set the values of a labelled raster object
#' @name .sits_raster_api_write_labelled
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Data cube
#' @param values         Matrix with values to be written
#' @param smoothing      Smoothing procedure to apply
#' @param filename       Filename of the raster image file
#' @return               Data cube
#'
.sits_raster_api_write_labelled  <- function(cube,
                                             values,
                                             smoothing = "none",
                                             filename){

    # create a raster object to write
    layer <- suppressWarnings(terra::rast(nrows = cube$nrows,
                         ncols = cube$ncols,
                         xmin  = cube$xmin,
                         xmax  = cube$xmax,
                         ymin  = cube$ymin,
                         ymax  = cube$ymax,
                         crs   = cube$crs))


    # select the best class by choosing the maximum value
    layer[] <- apply(values, 1, which.max)

    # apply majority filter
    if (smoothing == "majority" || smoothing == "bayesian+majority") {
        layer <- terra::focal(x = layer, w = 3,
                               na.rm = TRUE, fun = terra::modal)
    }
    # save raster output to file
    suppressWarnings(terra::writeRaster(layer,
                                        filename = filename,
                                        wopt     = list(
                                            filetype  = "GTiff",
                                            datatype = "INT1U",
                                            gdal = c("COMPRESS=LZW")),
                                        overwrite = TRUE))
    # was the file written correctly?
    assertthat::assert_that(file.info(filename)$size > 0,
            msg = "sits_label_classification : unable to save raster object")

    return(cube)
}

#' @title Given a labelled cube, return the band information
#' @name .sits_raster_api_area_frequency
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Metadata about a data cube
#' @return               Frequency of each label in the data cube
#'
.sits_raster_api_area_frequency <- function(cube){

    # precondition
    assertthat::assert_that("classified_image" %in% class(cube),
        msg = ".sits_raster_api_area_frequency requires a labelled cube")
    # retrieve the r object associated to the labelled cube
    file_info <- cube$file_info[[1]]
    assertthat::assert_that(nrow(file_info) == 1,
            msg = ".sits_raster_api_area_frequency: more than one classified image")
    r_obj <- terra::rast(file_info$path)
    # retrieve the frequency
    freq <- tibble::as_tibble(terra::freq(r_obj))

    return(freq)
}

