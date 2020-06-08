#' @title Provides information about one cube of the WTSS service
#' @name .sits_wtss_cube
#'
#' @description Uses the WTSS services to print information and save metadata
#' about a chosen cube.
#'
#' @param wtss.obj   R WTSS object associated to the service.
#' @param service    Name of the service.
#' @param URL        URL of the service provider.
#' @param name       Name of the cube.
#' @param bands      Name of the bands.
.sits_wtss_cube <- function(wtss.obj, service, URL, name, bands) {
    # obtains information about the available cubes
    # is the cube in the list of cubes?
    assertthat::assert_that(name %in% wtss.obj$coverages,
        msg = ".sits_wtss_cube: cube not available in the WTSS server")

    # describe the cube based on the WTSS API
    wtss::describe_coverage(wtss.obj, name, .print = FALSE)
    cov.tb    <- wtss.obj$description

    # temporal extent
    timeline <- lubridate::as_date(cov.tb$timeline[[1]])

    # retrieve information about the bands
    bands_wtss <- cov.tb$bands[[1]]

    # verify if requested bands is in provided bands
    if (!purrr::is_null(bands)) {
        assertthat::assert_that(all(bands %in% bands_wtss),
            msg = ".sits_wtss_cube: band not available from WTSS")
    }
    else
        bands <- bands_wtss

    # create a tibble to store the metadata
    cube_wtss <- .sits_cube_create(service   = service,
                                   URL       = URL,
                                   satellite = cov.tb$satellite,
                                   sensor    = cov.tb$sensor,
                                   name      = cov.tb$name,
                                   bands     = bands,
                                   labels    = c("NoClass"),
                                   scale_factors  = cov.tb$scale_factors[[1]],
                                   missing_values = cov.tb$missing_values[[1]],
                                   minimum_values = cov.tb$minimum_values[[1]],
                                   maximum_values = cov.tb$maximum_values[[1]],
                                   timelines      = list(timeline),
                                   nrows = cov.tb$nrows,
                                   ncols = cov.tb$ncols,
                                   xmin  = cov.tb$xmin,
                                   xmax  = cov.tb$xmax,
                                   ymin  = cov.tb$ymin,
                                   ymax  = cov.tb$ymax,
                                   xres  = cov.tb$xres,
                                   yres  = cov.tb$yres,
                                   crs   = cov.tb$crs)

    # return the tibble with cube info
    return(cube_wtss)
}

#' @title Obtain one timeSeries from WTSS server and load it on a sits tibble
#' @name .sits_from_wtss
#'
#' @description Returns one set of time series provided by a WTSS server
#' Given a location (lat/long), and start/end period, and WTSS server info,
#' retrieve a time series and include it on a stis tibble.
#' A Web Time Series Service (WTSS) is a light-weight service that
#' retrieves one or more time series in JSON format from a data base.
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016, p.166-177.
#'
#' @param cube            Metadata about the cube associated to the WTSS.
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param start_date      Date with the start of the period.
#' @param end_date        Date with the end of the period.
#' @param bands           Names of the bands of the cube.
#' @param label           Label to attach to the time series (optional).
#' @return                A sits tibble.
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
        assertthat::assert_that(all(bands %in% cb_bands),
            msg = "sits_from_wtss: bands are not available in the cube")

    # check start and end dates
    timeline <- sits_timeline(cube)
    if (purrr::is_null(start_date))
        start_date <- lubridate::as_date(timeline[1])
    if (purrr::is_null(end_date))
        end_date  <- lubridate::as_date(timeline[length(timeline)])


    # try to get a time series from the WTSS server
    # get the WTSS object associated to the URL
    # retrieve the time series from the service
    tryCatch({
        ts <- suppressMessages(wtss::time_series(cube$URL,
                                                 name        = cube$name,
                                                 attributes  = bands,
                                                 longitude   = longitude,
                                                 latitude    = latitude,
                                                 start_date  = start_date,
                                                 end_date    = end_date))
        # add a label column
        ts <- tibble::add_column(ts, label = label, .after = "end_date")
        # convert name
        data <- .sits_tibble_rename(ts)
        # return the tibble with the time series
        return(data)
    }, error = function(e){
        message("WTSS unable to recover time series for longitude ",
                     longitude, " latitude ", latitude)
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
    wtss.obj <- suppressMessages(wtss::WTSS(URL))
    # check return from WTSS
    assertthat::assert_that(!purrr::is_null(wtss.obj),
        msg = "sits_cube - WTSS service not responding - check URL")

    return(TRUE)
}
