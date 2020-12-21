#' @title Check if the raster files are bricks
#' @name .sits_raster_brick_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  satellite             Name of satellite
#' @param  sensor                Name of sensor
#' @param  name                  Name of the data cube.
#' @param  timeline              Vector of dates with the timeline of the bands.
#' @param  bands                 Vector of bands contained in the Raster Brick
#'                               set (in the same order as the files).
#' @param  files                 Vector with the file paths of the raster files
#' @return TRUE                  passed the check?
.sits_raster_brick_check <- function(satellite, sensor, name,
                                     timeline, bands, files) {
    assertthat::assert_that(!purrr::is_null(files),
        msg = "sits_cube: files must be provided"
    )
    assertthat::assert_that(!purrr::is_null(satellite),
        msg = "sits_cube: satelite must be provided"
    )
    assertthat::assert_that(!purrr::is_null(sensor),
        msg = "sits_cube: sensor must be provided"
    )
    assertthat::assert_that(!purrr::is_null(bands),
        msg = "sits_cube: bands must be provided"
    )
    assertthat::assert_that(length(bands) == length(files),
        msg = "sits_cube: bands do not match files"
    )
    assertthat::assert_that(!purrr::is_null(timeline),
        msg = "sits_cube: timeline must be provided"
    )
    # Tests is satellite and sensor are known to SITS
    .sits_config_satellite_sensor(satellite, sensor)

    # raster files
    assertthat::assert_that(!("function" %in% class(files)),
        msg = "a valid set of files should be provided"
    )
    # check if the files begin with http =:// or with vsicurl/
    files <- .sits_raster_api_check_url(files)
    # check if the raster files can be read by GDAL
    .sits_raster_api_check_access(files[1])

    # are the files bricks?
    # does the timeline match the number of bands?
    .sits_raster_api_check_brick(files[1], timeline)
    return(invisible(TRUE))
}

#' @title Create a raster brick data cube
#' @name .sits_raster_brick_cube
#' @keywords internal
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
                                    files) {

    # transform the timeline to date format
    timeline <- lubridate::as_date(timeline)

    # set the labels
    labels <- c("NoClass")

    # check if the files begin with http =:// or with vsicurl/
    files <- .sits_raster_api_check_url(files)

    # obtain the parameters
    params <- .sits_raster_api_params_file(files[1])
    assertthat::assert_that(nrow(params) > 0,
        msg = ".sits_raster_brick_cube: error in retrieving raster params"
    )

    # bands in SITS are uppercase
    bands <- toupper(bands)
    # get scale factors
    scale_factors <- .sits_config_scale_factors(sensor, bands)
    # get missing values
    missing_values <- .sits_config_missing_values(sensor, bands)
    # get minimum values
    minimum_values <- .sits_config_minimum_values(sensor, bands)
    # get maximum values
    maximum_values <- .sits_config_maximum_values(sensor, bands)

    times_brick <- rep(timeline[1], time = length(files))

    # get the file information
    file_info <- .sits_raster_api_file_info(bands, times_brick, files)

    # create a tibble to store the metadata
    cube <- .sits_cube_create(
        type = "BRICK",
        satellite = satellite,
        sensor = sensor,
        name = name,
        bands = bands,
        labels = labels,
        scale_factors = scale_factors,
        missing_values = missing_values,
        minimum_values = minimum_values,
        maximum_values = maximum_values,
        timelines = list(timeline),
        nrows = params$nrows,
        ncols = params$ncols,
        xmin = params$xmin,
        xmax = params$xmax,
        ymin = params$ymin,
        ymax = params$ymax,
        xres = params$xres,
        yres = params$yres,
        crs = params$crs,
        file_info = file_info
    )

    return(cube)
}
