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
    xres <- as.numeric(rg_obj["res.x"])
    yres <- as.numeric(rg_obj["res.y"])
    xmin <- as.numeric(rg_obj["ll.x"])
    ymin <- as.numeric(rg_obj["ll.y"])
    xmax <- floor(xmin + ncols * xres + 0.5)
    ymax <- floor(ymin + nrows * yres + 0.5)
    crs <- attr(rg_obj, "projection")

    # post conditions
    assertthat::assert_that(nrows > 0 & ncols > 0,
        msg = ".sits_raster_api_params: invalid raster object"
    )
    assertthat::assert_that(xmax > xmin & ymax > ymin,
        msg = ".sits_raster_api_params: invalid raster object"
    )

    params <- tibble::tibble(
        nrows = nrows,
        ncols = ncols,
        xres = xres,
        yres = yres,
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax,
        crs = crs
    )
    return(params)
}

#' @title Check if file is a brick
#' @name .sits_raster_api_check_brick
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Check if file is a valid raster brick
#' @param file     A valid raster image
#' @param timeline Timeline for the brick
#' @return TRUE/FALSE
#'
.sits_raster_api_check_brick <- function(file, timeline) {

    # read the information from the file using GDAL
    rg_obj <- suppressWarnings(rgdal::GDALinfo(file))

    # object parameters
    nbands <- as.numeric(rg_obj["bands"])
    nrows <- as.numeric(rg_obj["rows"])
    ncols <- as.numeric(rg_obj["columns"])

    assertthat::assert_that(nbands >= 1 & ncols > 1 & nrows > 1,
        msg = ".sits_raster_api_check_brick: invalid bricks"
    )
    assertthat::assert_that(nbands == length(timeline),
        msg = ".sits_raster_api_check_brick: mismatch btw timeline and bricks"
    )

    return(invisible(TRUE))
}
#' @title Check if the raster files are accessible by GDAL
#' @name .sits_raster_api_check_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  file         file associated to the raster data
#' @return TRUE         true if filles are acessible
.sits_raster_api_check_access <- function(file) {
    assertthat::assert_that(length(file) == 1,
        msg = ".sits_raster_api_check_access works with single files"
    )

    # verify if all files are reachable
    r <- suppressWarnings(rgdal::GDALinfo(file, silent = FALSE))
    assertthat::assert_that(all(!purrr::is_null(r)),
        msg = "sits_cube: raster files cannot be accessed"
    )
    return(TRUE)
}
#' @title Check if the raster files are on the web
#' @name .sits_raster_api_check_url
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return files        Updated files with  information for GDAL access
.sits_raster_api_check_url <- function(files) {
    # are there webfiles?
    if (grepl("http", c(files[1]))) {
        # append "vsicurl" prefix for all web files if it is not there
        if (!grepl("vsicurl", c(files[1]))) {
              files <- paste0("/vsicurl/", files)
          }
    }
    return(files)
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
    values <- tibble::as_tibble(terra::extract(rast, xy))
    # terra includes an ID (remove it)
    values <- values[, -1]

    # is the data valid?
    assertthat::assert_that(nrow(values) == nrow(xy),
        msg = ".sits_raster_api_extract - error in retrieving data"
    )
    return(values)
}
#' @title Define a filename associated to one classified raster layer
#' @name .sits_raster_api_filename
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a filename for a raster layer
#'                 with associated temporal information,
#'                 given a basic filename.
#' @param output_dir     Output directory
#' @param version        Output version
#' @param name           Original cube name (without temporal information).
#' @param type           Type of output
#' @param start_date     Starting date of the time series classification.
#' @param end_date       End date of the time series classification.
#' @return               Classification file for the required interval.
.sits_raster_api_filename <- function(output_dir,
                                      version,
                                      name,
                                      type,
                                      start_date,
                                      end_date) {
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    file_name <- paste0(output_dir, "/", name, "_", type,
                        "_", y1, "_", m1, "_", y2, "_", m2, "_",
                        version, ".tif"
    )

    return(file_name)
}

#' @title Create a tibble with file information to include in the cube
#' @name  .sits_raster_api_file_info
#' @keywords internal
#'
#' @param  bands    List of spectral bands
#' @param  timeline Cube timeline
#' @param  files    List of files associated to the
.sits_raster_api_file_info <- function(bands, timeline, files) {
    # create a tibble to store the file info
    # iterate through the list of bands and files
    assertthat::assert_that(length(bands) == length(timeline) &
                            length(files) == length(timeline),
        msg = ".sits_raster_api_file_info: unmatched bands, files and timeline")

    # create the file info
    file_info_lst <- purrr::pmap(
        list(bands, timeline, files),
        function(b, t, f) {
            fil <- tibble::tibble(
                band = b,
                date = lubridate::as_date(t),
                path = f
            )
            return(fil)
        })
    # join the list into a tibble
    file_info <- dplyr::bind_rows(file_info_lst)

    return(file_info)
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
        msg = ".sits_raster_api_read_extent; invalid file"
    )
    # get number of bands in the file
    n_bands_file <- rg_obj["bands"]

    assertthat::assert_that(n_bands_file >= 1,
        msg = ".sits_raster_api_read_extent; invalid number of bands"
    )

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
            values <- matrix(as.matrix(suppressWarnings(
                rgdal::readGDAL(
                    fname = r_files[1],
                    offset = c(first_row, first_col),
                    region.dim = c(extent["nrows"], extent["ncols"]),
                    silent = TRUE
                )@data
            )),
            nrow = extent["nrows"],
            byrow = TRUE
            )

            assertthat::assert_that(nrow(values) == extent["nrows"] &
                ncol(values) == extent["ncols"],
            msg = ".sits_raster_api_read_extent: error in reading a raster file"
            )
        }
        else {
            # case (2) - "raster brick"
            values <- as.matrix(suppressWarnings(
                rgdal::readGDAL(
                    fname = r_files[1],
                    offset = c(first_row, first_col),
                    region.dim = c(extent["nrows"], extent["ncols"]),
                    silent = TRUE
                )@data
            ))
            assertthat::assert_that(
                nrow(values) == extent["nrows"] * extent["ncols"] &
                    ncol(values) == n_bands_file,
                msg = ".sits_raster_api_read_extent: error reading file"
            )
        }
    }
    else {
        # case (3) - "raster stack"
        values_lst <- purrr::map(r_files, function(f) {
            data <- as.matrix(suppressWarnings(
                rgdal::readGDAL(
                    fname = f,
                    offset = c(first_row, first_col),
                    region.dim = c(extent["nrows"], extent["ncols"]),
                    silent = TRUE
                )@data
            ))
            return(data)
        })
        values <- do.call(cbind, values_lst)
        assertthat::assert_that(
            nrow(values) == extent["nrows"] * extent["ncols"] &
            ncol(values) == length(r_files),
        msg = ".sits_raster_api_read_extent: error in reading a raster file"
        )
    }

    return(values)
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
#' @return               Data cubevalues
#'
.sits_raster_api_write <- function(cube,
                                   num_layers,
                                   values,
                                   filename,
                                   datatype,
                                   format = "GTiff",
                                   compress = "LZW",
                                   overwrite = TRUE) {
    # preconditions
    assertthat::assert_that(num_layers >= 1,
        msg = ".sits_raster_api_write_raster: invalid number of layers"
    )

    # create a raster object
    r_obj <- suppressWarnings(
        terra::rast(
            nrows = cube$nrows,
            ncols = cube$ncols,
            nlyrs = num_layers,
            xmin = cube$xmin,
            xmax = cube$xmax,
            ymin = cube$ymin,
            ymax = cube$ymax,
            crs = cube$crs
        )
    )

    assertthat::assert_that(terra::nrow(r_obj) == cube$nrows,
                            msg = ".sits_raster_api_write: unable to create raster object"
    )

    # include the values in the raster object
    terra::values(r_obj) <- as.matrix(values)

    # options for compression
    opt_comp <- paste0("COMPRESS =", compress)

    suppressWarnings(terra::writeRaster(
        r_obj,
        filename = filename,
        overwrite = overwrite,
        wopt = list(
            gdal = opt_comp,
            format = format,
            datatype = datatype
        )
    ))

    # was the file written correctly?
    assertthat::assert_that(file.info(filename)$size > 0,
        msg = ".sits_raster_api_write_raster: unable to wriye raster object"
    )

    return(invisible(TRUE))
}

#' @title Set the values of a labelled raster object
#' @name .sits_raster_api_write_lab
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Data cube
#' @param values         Matrix with values to be written
#' @param smoothing      Smoothing procedure to apply
#' @param filename       Filename of the raster image file
#' @return               Data cube
#'
.sits_raster_api_write_lab <- function(cube,
                                            values,
                                            smoothing = "none",
                                            filename) {
    # create a raster object to write
    layer <- suppressWarnings(
        terra::rast(
            nrows = cube$nrows,
            ncols = cube$ncols,
            xmin = cube$xmin,
            xmax = cube$xmax,
            ymin = cube$ymin,
            ymax = cube$ymax,
            crs = cube$crs
        )
    )


    # select the best class by choosing the maximum value
    layer[] <- apply(values, 1, which.max)

    # apply majority filter
    if (smoothing == "majority" ||
        smoothing == "bayesian+majority") {
        layer <- terra::focal(
            x = layer,
            w = 3,
            na.rm = TRUE,
            fun = terra::modal
        )
    }
    # save raster output to file
    suppressWarnings(terra::writeRaster(
        layer,
        filename = filename,
        wopt = list(
            filetype = "GTiff",
            datatype = "INT1U",
            gdal = c("COMPRESS=LZW")
        ),
        overwrite = TRUE
    ))
    # was the file written correctly?
    assertthat::assert_that(file.info(filename)$size > 0,
        msg = "sits_label_classification : unable to save raster object"
    )

    return(cube)
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
    assertthat::assert_that("classified_image" %in% class(cube),
        msg = ".sits_raster_api_area_freq requires a labelled cube"
    )
    # retrieve the r object associated to the labelled cube
    file_info <- cube$file_info[[1]]
    assertthat::assert_that(nrow(file_info) == 1,
        msg = ".sits_raster_api_area_freq: more than one classified image"
    )
    r_obj <- terra::rast(file_info$path)
    # retrieve the frequency
    freq <- tibble::as_tibble(terra::freq(r_obj))

    return(freq)
}
