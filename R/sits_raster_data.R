#' @title Check if the raster files are on the web
#' @name .sits_raster_check_webfiles
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return files        Updated files with  information for GDAL access
.sits_raster_check_webfiles <- function(files) {
    # are there webfiles?
    if (all(grepl("http", c(files[1])))) {
        # append "vsicurl" prefix for all web files if it is not there
        if (!grepl("vsicurl", c(files[1])))
            files <- paste0("/vsicurl/", files)
    }
    return(files)
}

#' @title Check if the raster files are accessible by GDAL
#' @name .sits_raster_check_gdal_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return TRUE         true if filles are acessible
.sits_raster_check_gdal_access <- function(files){
    # verify if all files are reacheable
    r <- suppressWarnings(rgdal::GDALinfo(files, silent = FALSE))
    assertthat::assert_that(all(!purrr::is_null(r)),
                            msg = "sits_cube: raster files cannot be accessed")
    return(TRUE)
}

#' @title Define a filename associated to one classified raster layer
#' @name .sits_raster_filename
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
#' @return               Name of the classification file for the required interval.
.sits_raster_filename <- function(output_dir,
                                  version,
                                  name,
                                  type,
                                  start_date,
                                  end_date) {
    y1 <- lubridate::year(start_date)
    m1 <- lubridate::month(start_date)
    y2 <- lubridate::year(end_date)
    m2 <- lubridate::month(end_date)

    file_name <- paste0(output_dir,"/", name, "_", type, "_",
                        y1, "_", m1, "_", y2, "_", m2, "_", version,".tif")

    return(file_name)
}
#' @title Determine the cube params to write in the metadata
#' @name .sits_raster_params
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its params
#' @param r_obj    A valid raster object
#' @return A tibble with the cube params
.sits_raster_params <- function(r_obj) {

    params.tb <- tibble::tibble(
        nrows = raster::nrow(r_obj),
        ncols = raster::ncol(r_obj),
        xmin  = raster::xmin(r_obj),
        xmax  = raster::xmax(r_obj),
        ymin  = raster::ymin(r_obj),
        ymax  = raster::ymax(r_obj),
        xres  = raster::xres(r_obj),
        yres  = raster::yres(r_obj),
        crs   = as.character(suppressWarnings(raster::crs(r_obj)))
    )
    return(params.tb)
}
#' @title Raster object from file
#' @name .sits_raster_files_robj
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given the vector of files and the name of the service,
#'                  return the Raster object for the file
#' @param files     Vector of files
#' @return          Raster object associated to the first file
#'
.sits_raster_files_robj <- function(files){
    return(suppressWarnings(raster::brick(files[1])))
}
#' @title Tests if an XY position is inside a ST Raster Brick
#' @name .sits_raster_xy_inside
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Compares an XY position to the extent of a RasterBrick
#'              described by a raster metadata tibble.
#'              Returns TRUE if the point is
#'              inside the extent of the RasterBrick object.
#'
#' @param xy         XY extent compatible with the R raster package.
#' @param raster.tb  Tibble with metadata information about a raster data set.
#' @return TRUE if XY is inside the raster extent, FALSE otherwise.
.sits_raster_xy_inside <- function(xy, raster.tb){
    if (xy[1, "X"] < raster.tb[1, ]$xmin) return(FALSE)
    if (xy[1, "X"] > raster.tb[1, ]$xmax) return(FALSE)
    if (xy[1, "Y"] < raster.tb[1, ]$ymin) return(FALSE)
    if (xy[1, "Y"] > raster.tb[1, ]$ymax) return(FALSE)
    return(TRUE)
}

#' @title Tests if satellite and sensor are supported by SITS
#' @name .sits_raster_satellite_sensor
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Check if the satellite and sensor are supported by SITS
#'              looking at the configuration file
#' @param satellite     Name of the satellite
#' @param sensor        Name of the sensor
#' @return              TRUE/FALSE
#'
.sits_raster_satellite_sensor  <- function(satellite, sensor) {

    satellite <- toupper(satellite)
    sats <- .sits_config_satellites()
    my_sats <- paste0(sats, collapse = ", ")
    assertthat::assert_that(satellite %in% sats,
                            msg = paste0("satellite ", satellite, " not supported - ",
                                         "use one of ", my_sats))
    # is the sensor supported by SITS?
    sensor <- toupper(sensor)
    sensors <- .sits_config_sensors(satellite)
    my_sensors <- paste0(sensors, collapse = ", ")
    assertthat::assert_that(sensor %in% sensors,
                            msg = paste0("sensor ", sensors, " not supported - ",
                                         "use one of ", my_sensors))

    return(invisible(TRUE))
}

#' @title Create a tibble with file information to include in the cube
#' @name  .sits_raster_file_info
#' @keywords internal
#'
#' @param  bands    List of spectral bands
#' @param  timeline Cube timeline
#' @param  files    List of files associated to the
.sits_raster_file_info <- function(bands, timeline, files) {

    # create a tibble to store the file info
    # iterate through the list of bands and files
    file_info.lst <- purrr::pmap(list(bands, timeline, files),
                                 function(b, t, f) {
                                     fil.tb <- tibble::tibble(
                                         band = b,
                                         date = lubridate::as_date(t),
                                         path = f)
                                 })
    # join the list into a tibble
    file_info.tb  <- dplyr::bind_rows(file_info.lst)

    return(file_info.tb)

}
