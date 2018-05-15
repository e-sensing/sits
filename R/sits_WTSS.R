#' @title Provides information about WTSS service
#' @name sits_infoWTSS
#' @author Gilberto Camara
#'
#' @description obtains information about the WTSS server
#' and about the coverages.
#'
#' The Web Time Series Service is a lightweight web service the allow remote access to satellite
#'  image time series and provides three operations:
#'
#'  1. list coverages: this operation allows clients to retrieve the capabilities provided
#'     by any server that implements WTSS. It returns a list of coverage
#'     names available in a server instance.
#'
#'  2. describe coverage: this operation returns the metadata for a given coverage
#'     identified by its name. It includes its range in the spatial and temporal dimensions.
#'
#'  3. time series: this operation requests the time series of values of a coverage attribute
#'    at a given location.
#'
#' @return wtss.obj       an R object containing the information about the WTSS server
#'
#' @examples
#' \donttest{
#' # Obtain information about the coverages available
#' sits_infoWTSS()
#' }
#' @export

sits_infoWTSS <- function() {

    wtss.obj <- NULL
    # obtains information about the WTSS service
    services <- .sits_get_services(protocol = "WTSS")

    services %>%
        purrr::map(function (service) {
            URL       <- .sits_get_server(service)
            tryCatch({
                wtss.obj  <- wtss::WTSS(URL)
                cat(paste("-----------------------------------------------------------", "\n",sep = ""))
                cat(paste("The WTSS server URL is ", wtss.obj@serverUrl, "\n", sep = ""))

                # obtains information about the coverages
                coverages.obj    <- wtss::listCoverages(wtss.obj)
                cat(paste("Available coverages: \n"))
                coverages.obj %>%
                    purrr::map(function(c) cat(paste(c, "\n", sep = "")))
                cat(paste("------------------------------------------------------------", "\n",sep = ""))

            }, error = function(e) {
                msg <- paste0("WTSS service not available at URL ", URL)
                .sits_log_error(msg)
                message(msg)
            })

        })

    return(invisible(wtss.obj))
}

#' @title Provides information about one coverage of the WTSS service
#' @name .sits_coverageWTSS
#'
#' @description uses the WTSS services to print information and save metadata about a
#' chosen coverage
#'
#' @param wtss.obj   R WTSS object associated to the service
#' @param service    name of the service
#' @param name       name of the coverage
#'
.sits_coverageWTSS <- function(wtss.obj, service, name) {

    # obtains information about the available coverages
    coverages.vec    <- wtss::listCoverages(wtss.obj)

    # is the coverage in the list of coverages?
    ensurer::ensure_that(name, (.) %in% coverages.vec,
                         err_desc = ".sits_coverageWTSS: coverage is not available in the WTSS server")

    # describe the coverage
    cov.lst    <- wtss::describeCoverage(wtss.obj, name)
    cov        <- cov.lst[[name]]

    # temporal extent
    timeline <- cov$timeline

    # retrieve information about the bands
    band_info <- cov$attributes

    attr <- as.data.frame(band_info)
    bands <- as.vector(attr[,"name"])
    missing_values <- as.vector(attr[,"missing_value"])
    names(missing_values) <- bands
    scale_factors  <- as.vector(attr[,"scale_factor"])
    names(scale_factors) <- bands

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

    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_objs         = list(wtss.obj),
                                  name           = name,
                                  service        = service,
                                  bands          = list(bands),
                                  scale_factors  = list(scale_factors),
                                  missing_values = list(missing_values),
                                  start_date     = as.Date(timeline[1]),
                                  end_date       = as.Date(timeline[length(timeline)]),
                                  timeline       = list(timeline),
                                  nrows          = nrows,
                                  ncols          = ncols,
                                  xmin           = xmin,
                                  xmax           = xmax,
                                  ymin           = ymin,
                                  ymax           = ymax,
                                  xres           = xres,
                                  yres           = yres,
                                  crs            = crs,
                                  files          = NA)

    # return the tibble with coverage info
    return(coverage.tb)
}

#' @title Obtain one timeSeries from WTSS server and load it on a sits tibble
#' @name .sits_fromWTSS
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
#' @param coverage        metadata about the coverage where the data is to be retrived
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param bands           list of string - a list of the names of the bands of the coverage
#' @param label           string - the label to attach to the time series (optional)
#' @return data.tb        a SITS tibble
#'
.sits_fromWTSS <- function(coverage,
                           longitude,
                           latitude,
                           start_date = NULL,
                           end_date   = NULL,
                           bands      = NULL,
                           label      = "NoClass") {

    # if bands are not provided, use all bands available in the coverage
    # check the bands are available
    cov_bands <- coverage$bands[[1]]
    if (purrr::is_null(bands))
        bands <- cov_bands
    else
        ensurer::ensure_that(bands, all((.) %in% cov_bands),
                             err_desc = "sits_fromWTSS: requested bands are not available in the coverage")

    # check start and end dates
    if (purrr::is_null(start_date))
        start_date <- coverage$start_date
    if (purrr::is_null(end_date))
        end_date <- coverage$end_date

    # try to get a time series from the WTSS server
    tryCatch({
        # get the WTSS object associated to the URL
        wtss.obj <- coverage$r_objs[[1]]
        # retrieve the time series from the service
        ts <- wtss::timeSeries(object     = wtss.obj,
                               coverages  = coverage$name,
                               attributes = bands,
                               longitude  = longitude,
                               latitude   = latitude,
                               start_date = start_date,
                               end_date   = end_date)

        # retrieve the time series information
        time_series <- ts[[coverage$name]]$attributes

        # determine the missing value for each band
        missing_values <- coverage$missing_values[[1]]
        # update missing values to NA
        bands %>%
            purrr::map(function (b) {
                time_series[, b][time_series[, b] == missing_values[b]] <<- NA
            })

        # interpolate missing values
        time_series[, bands] <- zoo::na.spline(time_series[, bands])

        # scale the time series
        scale_factors <- coverage$scale_factors[[1]]
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
        data.tb <- sits_tibble()
        # add one row to the tibble
        data.tb <- tibble::add_row(data.tb,
                                   longitude,
                                   latitude,
                                   start_date  = start_date,
                                   end_date    = end_date,
                                   label       = label,
                                   coverage    = coverage$name,
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

