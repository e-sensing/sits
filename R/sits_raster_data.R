#' @title Check if the raster files are on the web
#' @name .sits_raster_check_webfiles
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param files         files associated to the raster data
#' @return files        Updated files with  information for GDAL access
.sits_raster_check_webfiles <- function(files) {
    # are there webfiles?
    if (all(grepl("http", c(files[1])))) {
        # append "vsicurl" prefix for all web files if it is not there
        if (!grepl("vsicurl", c(files[1])))
            files <- paste("/vsicurl", files, sep = "/")
    }
    return(files)
}

#' @title Check if the raster files are accessible by GDAL
#' @name .sits_raster_check_gdal_access
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

#' @title Check if the raster files are bricks
#' @name .sits_raster_check_bricks
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  satellite             Name of satellite
#' @param  sensor                Name of sensor
#' @param  name                  Name of the data cube.
#' @param  timeline              Vector of dates with the timeline of the bands.
#' @param  bands                 Vector of bands contained in the Raster Brick
#'                               set (in the same order as the files).
#' @param  files                 Vector with the file paths of the raster files
#' @return TRUE                  passed the check?
.sits_raster_check_bricks <- function(satellite, sensor, name,
                                      timeline, bands, files){

    assertthat::assert_that(!purrr::is_null(files),
                    msg = "sits_cube: for type = BRICK, files must be provided")
    assertthat::assert_that(!purrr::is_null(satellite),
                    msg = "sits_cube: for type = BRICK satelite must be provided")
    assertthat::assert_that(!purrr::is_null(sensor),
                    msg = "sits_cube: for type = BRICK sensor must be provided")
    assertthat::assert_that(!purrr::is_null(bands),
                    msg = "sits_cube: for type = BRICK bands must be provided")
    assertthat::assert_that(length(bands) == length(files),
                    msg = "sits_cube: bands do not match files")
    assertthat::assert_that(!purrr::is_null(timeline),
                    msg = "sits_cube: for type = BRICK timeline must be provided")
    # Tests is satellite and sensor are known to SITS
    .sits_raster_satellite_sensor(satellite, sensor)

    # raster files
    assertthat::assert_that(!("function" %in% class(files)),
                            msg = "a valid set of files should be provided")
    # check if the files begin with http =:// or with vsicurl/
    files <- .sits_raster_check_webfiles(files)
    # check if the raster files can be read by GDAL
    .sits_raster_check_gdal_access(files)

    # are the files bricks?
    tryCatch({
        brick <- suppressWarnings(raster::brick(files[1]))
    }, error = function(e){
        msg <- paste0("Raster files are not bricks")
        .sits_log_error(msg)
        message(msg)
    })
    return(TRUE)
}

#' @title Create a raster brick data cube
#' @name .sits_raster_brick_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  Builds a BRICK cube
#'
#' @param  satellite             Name of satellite
#' @param  sensor                Name of sensor
#' @param  name                  Name of the data cube.
#' @param  timeline              Vector of dates with the timeline of the bands.
#' @param  bands                 Vector of bands contained in the Raster Brick
#'                               set (in the same order as the files).
#' @param  files                 Vector with the file paths of the raster files.
#' @return A tibble with metadata information about a raster data set.
#'
.sits_raster_brick_cube <- function(satellite,
                                    sensor,
                                    name,
                                    timeline,
                                    bands,
                                    files){

    # transform the timeline to date format
    timeline <- lubridate::as_date(timeline)

    # set the labels
    labels <- c("NoClass")

    # check if the files begin with http =:// or with vsicurl/
    files <- .sits_raster_check_webfiles(files)

    # obtain the parameters
    params <- .sits_raster_params(.sits_raster_files_robj(files))

    # bands in SITS are uppercase
    bands <- toupper(bands)
    # get scale factors
    scale_factors  <- .sits_config_scale_factors(sensor, bands)
    # get missing values
    missing_values <- .sits_config_missing_values(sensor, bands)
    # get minimum values
    minimum_values <- .sits_config_minimum_values(sensor, bands)
    # get maximum values
    maximum_values <- .sits_config_maximum_values(sensor, bands)

    times_brick <- rep(timeline[1], time = length(files))

    # get the file information
    file_info <- .sits_raster_file_info(params$xres, bands, times_brick, files)

    # create a tibble to store the metadata
    cube <- .sits_cube_create(type           = "BRICK",
                              satellite      = satellite,
                              sensor         = sensor,
                              name           = name,
                              bands          = bands,
                              labels         = labels,
                              scale_factors  = scale_factors,
                              missing_values = missing_values,
                              minimum_values = minimum_values,
                              maximum_values = maximum_values,
                              timelines      = list(timeline),
                              nrows = params$nrows,
                              ncols = params$ncols,
                              xmin  = params$xmin,
                              xmax  = params$xmax,
                              ymin  = params$ymin,
                              ymax  = params$ymax,
                              xres  = params$xres,
                              yres  = params$yres,
                              crs   = params$crs,
                              file_info = file_info)

    class(cube) <- c("brick_cube", class(cube))
    return(cube)
}

#' @title Define a filename associated to one classified raster layer
#' @name .sits_raster_filename
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a filename for a raster layer
#'                 with associated temporal information,
#'                 given a basic filename.
#' @param output_dir     Output directory
#' @param version        Output version
#' @param name           Original cube name (without temporal information).
#' @param type           Type of output
#' @param start_date    Starting date of the time series classification.
#' @param end_date      End date of the time series classification.
#' @return Name of the classification file for the required interval.
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
#'
#' @param  res      Cube spatial resolution
#' @param  bands    List of spectral bands
#' @param  timeline Cube timeline
#' @param  files    List of files associated to the
.sits_raster_file_info <- function(res, bands, timeline, files) {

    # create a tibble to store the file info
    # iterate through the list of bands and files
    file_info.lst <- purrr::pmap(list(bands, timeline, files),
                                 function(b, t, f) {
                                     fil.tb <- tibble::tibble(
                                         res = as.character(round(res)),
                                         band = b,
                                         date = lubridate::as_date(t),
                                         path = f)
                                 })
    # join the list into a tibble
    file_info.tb  <- dplyr::bind_rows(file_info.lst)

    return(file_info.tb)

}
