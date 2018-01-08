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
#' wtss_inpe <- sits_infoWTSS()
#' }
#' @export

sits_infoWTSS <- function() {

    # load the configuration file
    if (!exists("config_sys"))
        config_sits <- sits_config()

    # load the configuration file
    if (!exists("logger"))
        logger <- sits_log()

    wtss.obj <- NULL
    # obtains information about the WTSS service
    URL       <- config_sits$WTSS_server
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
        msg <- paste0("WTSS service not available at URL ", config_sits$WTSS_server)
        log4r::error(logger, msg)
        message(msg)
        }
    )
    return(invisible(wtss.obj))
}
#' @title Provides information about one coverage of the WTSS service
#' @name sits_coverageWTSS
#'
#' @description uses the WTSS services to print information and save metadata about a
#' chosen coverage:
#'  bands          - the bands of the data to be retrieved from the WTSS
#'  start_date     - the start date for the time series data in the coverage
#'  end_date       - the end date for the time series data in the coverage
#'  xres           - spatial resolution (x dimension)
#'  yres           - spatial resolution (y dimension)
#'  start_date     - initial date of the coverage time series
#'  end_date       - final date of the coverage time series
#'  xmin           - spatial extent (xmin)
#'  ymin           - spatial extent (ymin)
#'  xmax           - spatial extent (xmax)
#'  ymax           - spatial extent (ymin)
#'  scale_factor   - scale factor to covert bands to a [0..1] scale
#'  crs            - Projection crs
#'
#' @param coverage   the name of the coverage
#' @param .show      show information about the coverage
#' @export
#'
sits_coverageWTSS <- function(coverage = NULL, .show = TRUE) {

    # load the configuration file
    if (!exists("config_sys"))
        config_sits <- sits_config()

    # obtains information about the WTSS service
    URL <- config_sits$WTSS_server
    wtss.obj         <- wtss::WTSS(URL)

    # obtains information about the available coverages
    coverages.vec    <- wtss::listCoverages(wtss.obj)

    # is the coverage in the list of coverages?
    ensurer::ensure_that(coverage, . %in% coverages.vec,
                         err_desc = "sits_coverageWTSS: coverage is not available in the WTSS server")

    # describe the coverage
    cov.lst    <- wtss::describeCoverage(wtss.obj, coverage)
    cov        <- cov.lst[[coverage]]

    # temporal extent
    timeline <- cov$timeline

    # retrieve information about the bands
    band_info <- cov$attributes
    b <- tibble::as.tibble(band_info[, -(3:4)])
    coverage.tb <-  .sits_tibble_coverage()
    coverage.tb <-  tibble::add_row(coverage.tb,
                                    wtss.obj       = list(wtss.obj),
                                    name           = cov$name,
                                    bands          = list(b),
                                    start_date     = as.Date(cov$timeline[1]),
                                    end_date       = as.Date(cov$timeline[length(timeline)]),
                                    timeline       = list(cov$timeline),
                                    xmin           = cov$spatial_extent$xmin,
                                    xmax           = cov$spatial_extent$xmax,
                                    ymin           = cov$spatial_extent$ymin,
                                    ymax           = cov$spatial_extent$ymax,
                                    xres           = cov$spatial_resolution$x,
                                    yres           = cov$spatial_resolution$y,
                                    crs            = cov$crs$proj4
    )

    if (.show)
        .print_coverage_attrs(cov)

    return(coverage.tb)
}

#' @title Obtain information about one coverage of the WTSS service
#' @name sits_getcovWTSS
#'
#' @description uses the WTSS services to retrieve information about a given coverage:
#'  name            - coverage name
#'  description     - description
#'  detail          - more information (source)
#'  attributes      - spectral bands (name, description, datatype, valid_range.min, valid_range.max, scale_factor, missing_value)
#'  spatial_extent  - bounding box in space (xmin, ymin, xmax, ymax)
#'  crs             - Projection CRS
#'  timeline        - dates of images contained in the coverage
#'
#' @param coverage   the name of the coverage
#' @return cov       a list with descriptive information about the coverage
#' @export
sits_getcovWTSS <- function(coverage = NULL) {
    # is the coverage name provided?
    ensurer::ensure_that(coverage, !purrr::is_null(.),
                         err_desc = "sits_getcovWTSS: Coverage name must be provided")

    # load the configuration file
    if (!exists("config_sys"))
        config_sits <- sits_config()

    # obtains information about the WTSS service
    URL <- config_sits$WTSS_server
    wtss.obj         <- wtss::WTSS(URL)
    # obtains information about the coverages
    coverages.vec    <- wtss::listCoverages(wtss.obj)
    # is the coverage in the list of coverages?
    ensurer::ensure_that(coverage, . %in% coverages.vec,
                         err_desc = "sits_getcovWTSS: coverage is not available in the WTSS server")
    #retrive the coverage information
    cov.lst    <- wtss::describeCoverage(wtss.obj, coverage)
    cov <- cov.lst[[coverage]]

    return(cov)
}
#' @title Obtain one timeSeries from WTSS server and load it on a sits tibble
#' @name sits_fromWTSS
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
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param coverage        name of the coverage
#' @param bands           list of string - a list of the names of the bands of the coverage
#' @param label           string - the label to attach to the time series (optional)
#' @return data.tb        a SITS tibble
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from a WTSS server
#' point.tb <- sits_fromWTSS (longitude = -55.50563, latitude = -11.71557)
#' }
#' @export
sits_fromWTSS <- function(longitude,
                          latitude,
                          start_date = NULL,
                          end_date   = NULL,
                          coverage   = "mod13q1_512",
                          bands      = NULL,
                          label      = "NoClass") {

    # load the configuration file
    if (!exists("config_sys"))
        config_sits <- sits_config()

    # obtains an R object that represents the WTSS service
    URL <- config_sits$WTSS_server
    wtss.obj <- wtss::WTSS(URL)

    #retrieve coverage information
    cov <- sits_getcovWTSS(coverage)

    # set the start and end dates from the coverage
    if (purrr::is_null(start_date)) start_date <- lubridate::as_date(cov$timeline[1])
    if (purrr::is_null(end_date))   end_date   <- lubridate::as_date(cov$timeline[length(cov$timeline)])

    if (purrr::is_null(bands))
        bands <- cov$attributes[, 1]

    # retrieve the logger
    logger <- sits_log()

    # try to get a time series from the WTSS server
    tryCatch({
        ts <- wtss::timeSeries(wtss.obj,
                               cov$name,
                               bands,
                               longitude,
                               latitude,
                               start_date,
                               end_date)
    }, warning = function(w){

    }, error = function(e){
        msg <- paste0("WTSS - unable to retrieve point (", longitude, ", ", latitude, ", ", start_date," ,", end_date,")")
        log4r::error(logger, msg)
        log4r::write.message(logger, msg)
        return (NULL)
    })


    # retrieve the time series information
    time_series <- ts[[cov$name]]$attributes

    # retrieve information about the bands
    band_info <- cov$attributes

    # determine the missing value for each band
    miss_value <- function(band) {
        return(band_info[which(band == band_info[, "name"]), "missing_value"])
    }
    # update missing values to NA
    for (b in bands) {
        time_series[, b][time_series[, b] == miss_value(b)] <- NA
    }

    # interpolate missing values
    time_series[, bands] <- zoo::na.spline(time_series[, bands])

    # calculate the scale factor for each band
    scale_factor <- function(band) {
        return(band_info[which(band == band_info[, "name"]), "scale_factor"])
    }
    # scale the time series
    bands %>%
        purrr::map(function(b) {
            time_series[, b] <<- time_series[, b]*scale_factor(b)
        })

    # convert the series to a tibble
    ts.tb <- tibble::as_tibble(zoo::fortify.zoo(time_series))

    # adjust the dates
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date)) {
        start_date <- as.Date(ts.tb$Index[1])
        end_date   <- as.Date(ts.tb$Index[NROW(ts.tb)])
    }

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
                               coverage    = cov$name,
                               time_series = ts.lst)

    # return the tibble with the time series
    return(data.tb)
}

#' @title Provides information about one coverage of the WTSS service
#' @name .sits_print_coverage_attrs
#' @description Prints information about the attributes of a WTSS coverage
#'
#' @param cov coverage information provided by the WTSS service
#'
.print_coverage_attrs <- function(cov) {

    # name, description and source of coverage
    cat(paste("------------------------------------------------------------------", "\n",sep = ""))
    cat(paste("Coverage: ", cov$name, "\n",sep = ""))
    cat(paste("Description: ", cov$description, "\n", sep = ""))
    cat(paste("Source: ", cov$detail, "\n", sep = ""))

    # information about the bands
    cat(paste("Bands: ", "\n", sep = "", collapse = ""))
    # retrieve information about the bands
    band_info <- cov$attributes
    attr <- as.data.frame(band_info)
    print(attr[1:2])

    # spatial extent and resolution, projection CRS
    cat(paste("\nSpatial extent: ", "(",
              cov$spatial_extent$xmin, ", ",
              cov$spatial_extent$ymin, ") - (",
              cov$spatial_extent$xmax, ", ",
              cov$spatial_extent$ymax, ")", sep = ""))
    cat(paste("\nSpatial resolution: ", "(",
              cov$spatial_resolution$x, ", ",
              cov$spatial_resolution$y, ")", sep = ""))
    cat(paste("\nProjection CRS: ", cov$crs$proj4, sep = ""))

    # get the timeline
    timeline <- cov$timeline

    cat(paste("\nTime range: ", timeline[1], " to ", timeline[length(timeline)], "\n", sep = ""))

    # temporal resolution is approximate, taken as the difference between first and second date

    temporal_resolution <- as.integer((lubridate::as_date(timeline[2])
                                       - lubridate::as_date(timeline[1]))/lubridate::ddays(1))

    cat(paste("Temporal resolution: ", temporal_resolution, " days ", "\n", sep = ""))
    cat(paste("----------------------------------------------------------------", "\n",sep = ""))
}



