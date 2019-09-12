#' @title Provides information about one cube of the WTSS service
#' @name .sits_wtss_cube
#'
#' @description Uses the WTSS services to print information and save metadata about a
#' chosen cube.
#'
#' @param wtss.obj   R WTSS object associated to the service.
#' @param service    Name of the service.
#' @param URL        URL of the service provider.
#' @param name       Name of the cube.
#' @param bands      Name of the bands.
.sits_wtss_cube <- function(wtss.obj, service, URL, name, bands) {
    # obtains information about the available cubes
    cubes.vec    <- wtss::listCoverages(wtss.obj)

    # is the cube in the list of cubes?
    ensurer::ensure_that(name, (.) %in% cubes.vec,
                         err_desc = ".sits_wtss_cube: cube is not available in the WTSS server")

    # describe the cube based on the WTSS API
    cov.lst    <- wtss::describeCoverage(wtss.obj, name)
    cov        <- cov.lst[[name]]

    # retrieve the satellite associated to the cube
    satellite <- .sits_config_satellite(name)
    # retrieve the sensor associated to the cube
    sensor <- .sits_config_sensor(name)
    # temporal extent
    timeline <- lubridate::as_date(cov$timeline)

    # retrieve information about the bands
    band_info <- cov$attributes

    attr <- as.data.frame(band_info)
    bands_wtss <- as.vector(attr[,"name"])

    # verify if requested bands is in provided bands
    if (!purrr::is_null(bands)) {
        ensurer::ensure_that(bands_wtss, all(bands %in% .),
                             err_desc = ".sits_wtss_cube: requested band not provided by WTSS service.")
    } else
        bands <- bands_wtss

    b <- bands_wtss %in% bands
    bands_wtss <- bands_wtss[b]

    missing_values <- as.vector(attr[,"missing_value"])[b]
    names(missing_values) <- bands_wtss
    scale_factors  <- as.vector(attr[,"scale_factor"])[b]
    names(scale_factors)  <- bands_wtss
    minimum_values <- as.vector(attr[,"valid_range"][["min"]])[b]
    names(minimum_values) <- bands_wtss
    maximum_values <- as.vector(attr[,"valid_range"][["max"]])[b]
    names(maximum_values) <- bands_wtss

    # Spatial extent
    xmin <- cov$spatial_extent$xmin
    ymin <- cov$spatial_extent$ymin
    xmax <- cov$spatial_extent$xmax
    ymax <- cov$spatial_extent$ymax

    # Spatial resolution
    xres <- cov$spatial_resolution$x
    yres <- cov$spatial_resolution$y

    # Size (rows and cols)
    nrows <- cov$dimension$y$max_idx - cov$dimensions$y$min_idx + 1
    ncols <- cov$dimension$x$max_idx - cov$dimensions$x$min_idx + 1

    # Projection CRS
    crs <- cov$crs$proj4

    #labels
    labels <- c("NoClass")

    # create a tibble to store the metadata
    cube_wtss <- .sits_cube_create(service, URL, satellite, sensor, name, bands_wtss, labels,
                                   scale_factors, missing_values, minimum_values, maximum_values,
                                   list(timeline), nrows, ncols, xmin, xmax, ymin, ymax,
                                   xres, yres, crs)

    # return the tibble with cube info
    return(cube_wtss)
}

#' @title Obtain one timeSeries from WTSS server and load it on a sits tibble
#' @name .sits_from_wtss
#'
#' @description Returns one set of time series provided by a WTSS server
#' Given a location (lat/long), and start/end period, and the WTSS server information
#' retrieve a time series and include it on a stis tibble.
#' A Web Time Series Service (WTSS) is a light-weight service that
#' retrieves one or more time series in JSON format from a data base.
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177
#'
#' @param cube            Metadata about the cube where the data is to be retrived.
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param start_date      Date with the start of the period.
#' @param end_date        Date with the end of the period.
#' @param bands           A list of string with the names of the bands of the cube.
#' @param label           A string with the label to attach to the time series (optional).
#' @return A sits tibble.
.sits_from_wtss <- function(cube,
                           longitude,
                           latitude,
                           start_date = NULL,
                           end_date   = NULL,
                           bands      = NULL,
                           label      = "NoClass") {
    # if bands are not provided, use all bands available in the cube
    # check the bands are available
    cb_bands <- .sits_cube_bands(cube)
    if (purrr::is_null(bands))
        bands <- cb_bands
    else
        ensurer::ensure_that(bands, all((.) %in% cb_bands),
                             err_desc = "sits_from_wtss: requested bands are not available in the cube")

    # check start and end dates
    timeline <- sits_timeline(cube)
    if (purrr::is_null(start_date))
        start_date <- lubridate::as_date(timeline[1])
    if (purrr::is_null(end_date))
        end_date  <- lubridate::as_date(timeline[length(timeline)])

    # try to get a time series from the WTSS server
    tryCatch({
        # get the WTSS object associated to the URL
        wtss.obj <- wtss::WTSS(cube$URL)
        # retrieve the time series from the service
        ts <- wtss::timeSeries(object     = wtss.obj,
                               coverages  = cube$name,
                               attributes = bands,
                               longitude  = longitude,
                               latitude   = latitude,
                               start_date = start_date,
                               end_date   = end_date)

        # retrieve the time series information
        time_series <- ts[[cube$name]]$attributes

        # determine the missing value for each band
        missing_values <- .sits_cube_missing_values(cube)
        # update missing values to NA
        bands %>%
            purrr::map(function (b) {
                time_series[, b][time_series[, b] == missing_values[b]] <<- NA
            })

        # interpolate missing values
        time_series[, bands] <- zoo::na.spline(time_series[, bands])

        # scale the time series
        scale_factors <- .sits_cube_scale_factors(cube)
        bands %>%
            purrr::map(function(b) {
                time_series[, b] <<- time_series[, b]*scale_factors[b]
            })

        # convert the series to a tibble
        ts.tb <- tibble::as_tibble(zoo::fortify.zoo(time_series))

        # create a list to store the time series coming from the WTSS service
        ts.lst <- list()
        ts.lst[[1]] <- ts.tb

        # create a tibble to store the WTSS data
        data <- .sits_tibble()
        # add one row to the tibble
        data <-    tibble::add_row(data,
                                   longitude,
                                   latitude,
                                   start_date  = start_date,
                                   end_date    = end_date,
                                   label       = label,
                                   cube        = cube$name,
                                   time_series = ts.lst)

        # return the tibble with the time series
        return(data)

    }, error = function(e){
        msg <- paste0("WTSS - unable to retrieve point (", longitude, ", ", latitude, ", ", start_date," ,", end_date,")")
        .sits_log_error(msg)
        message("WTSS - unable to retrieve point - see log file for details" )
        return(NULL)
    })
}
#' @title Check that the URL of WTSS service is working
#' @name .sits_wtss_check
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param URL        URL of the WTSS service.
#' @return check     TRUE or FALSE
.sits_wtss_check <- function(URL) {
    check <- tryCatch({
        # tries to connect to the WTSS service
        wtss.obj   <- wtss::WTSS(URL)
    }, error = function(e){
        msg <- paste0("WTSS service not available at URL ", URL)
        .sits_log_error(msg)
        message(msg)
    })
    # did we get an error?
    if (!inherits(check, "error"))
        return(TRUE)
    else
        return(FALSE)
}
