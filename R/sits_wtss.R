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
        data.tb <- .sits_tibble()
        # add one row to the tibble
        data.tb <- tibble::add_row(data.tb,
                                   longitude,
                                   latitude,
                                   start_date  = start_date,
                                   end_date    = end_date,
                                   label       = label,
                                   cube        = cube$name,
                                   time_series = ts.lst)

        # return the tibble with the time series
        return(data.tb)

    }, error = function(e){
        msg <- paste0("WTSS - unable to retrieve point (", longitude, ", ", latitude, ", ", start_date," ,", end_date,")")
        .sits_log_error(msg)
        message("WTSS - unable to retrieve point - see log file for details" )
        return(NULL)
    })
}
