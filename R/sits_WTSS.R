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
    URL       <- .sits_get_server(service = "WTSS")
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
        }
    )
    return(invisible(wtss.obj))
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
#' @param product         the image product where the coverage depends
#' @param coverage        name of the coverage
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
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
sits_fromWTSS <- function(product    = "MOD13Q1",
                          coverage   = "mod13q1_512",
                          longitude,
                          latitude,
                          start_date = NULL,
                          end_date   = NULL,
                          bands      = NULL,
                          label      = "NoClass") {

    # if bands are not provided, use all bands available in the coverage
    cov_bands <- .sits_get_bands("WTSS", product = product)
    if (purrr::is_null(bands))
        bands <- cov_bands
    else
        ensurer::ensure_that(bands, all((.) %in% cov_bands),
                             err_desc = "sits_fromWTSS: requested bands are not available in the coverage")

    # try to get a time series from the WTSS server
    tryCatch({
        URL <- .sits_get_server("WTSS")
        wtss.obj <- wtss::WTSS(URL)
        ts <- wtss::timeSeries(wtss.obj,
                               coverage,
                               bands,
                               longitude,
                               latitude,
                               start_date,
                               end_date)

        # retrieve the time series information
        time_series <- ts[[coverage]]$attributes

        # determine the missing value for each band
        miss_value <- vector()
        for (b in bands)
            miss_value[b] <- .sits_get_missing_value(product, b)

        # update missing values to NA
        for (b in bands) {
            time_series[, b][time_series[, b] == miss_value[b]] <- NA
        }

        # interpolate missing values
        time_series[, bands] <- zoo::na.spline(time_series[, bands])

        scale_factor <- vector()
        for (b in bands)
            scale_factor[b] <- .sits_get_scale_factor(product, b)

        # scale the time series
        bands %>%
            purrr::map(function(b) {
                time_series[, b] <<- time_series[, b]*scale_factor[b]
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
                                   coverage    = coverage,
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

#' @title Provides information about one coverage of the WTSS service
#' @name .sits_print_coverage_attrs
#' @description Prints information about the attributes of a WTSS coverage
#'
#' @param cov coverage information provided by the WTSS service
#'
.sits_print_coverage_attrs <- function(cov) {

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



