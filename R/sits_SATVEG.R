#' @title Obtain one timeSeries from the EMBRAPA SATVEG server and load it on a sits tibble
#' @name sits_fromSATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns one set of MODIS time series provided by the EMBRAPA server (SATVEG)
#' Given a location (lat/long), the function retrieves the "ndvi" and "evi" bands from SATVEG
#' and inclues the data on a stis tibble. If start and end date are given, the function
#' filter the data to limit the temporal interval.
#'
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param satellite       the satellite ("terra", "aqua", "comb")
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series (optional)
#' @return data.tb        a SITS tibble
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from the SATVEG server
#' point.tb <- sits_fromSATVEG (longitude = -55.50563, latitude = -11.71557)
#' }
#' @export
sits_fromSATVEG <- function(longitude  = NULL,
                            latitude    = NULL,
                            start_date  = NULL,
                            end_date    = NULL,
                            satellite   = "terra",
                            prefilter   = "1",
                            label       = "NoClass") {

    # load the configuration file
    if (purrr::is_null(sits.env$config))
        sits_config()

    # check parameters
    .sits_check_SATVEG(longitude, latitude, satellite, prefilter)

    # retrieve the time series
    ts.tb <- .sits_ts_from_SATVEG(longitude, latitude, satellite, prefilter)

    # filter the dates
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date))
        ts.tb <- dplyr::filter(ts.tb, dplyr::between(ts.tb$Index, start_date, end_date))
    else {
        start_date <- as.Date(ts.tb$Index[1])
        end_date   <- as.Date(ts.tb$Index[NROW(ts.tb)])
    }
    # set the name of the coverage
    coverage <- .sits_coverage_SATVEG(satellite)

    # use a list to store the time series
    ts.lst <- list()
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the SATVEG data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = start_date,
                               end_date     = end_date,
                               label        = label,
                               coverage     = coverage,
                               time_series  = ts.lst
    )
    return(data.tb)
}
#' @title Check that SATVEG function parameters are correct
#' @name .sits_check_SATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Checks each parameters against the SATVEG capabilties
#'
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param satellite       the satellite ("terra", "aqua", "comb")
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @return status         TRUE if no problems are detected
#'
.sits_check_SATVEG <- function(longitude, latitude, satellite, prefilter){

    ensurer::ensure_that(longitude, !purrr::is_null(.),
                         err_desc = "sits_fromSATVEG: Missing longitude info")
    ensurer::ensure_that(latitude,  !purrr::is_null(.),
                         err_desc = "sits_fromSATVEG: Missing latitude info")
    ensurer::ensure_that(satellite, (.) %in% sits.env$config$SATVEG_satellites,
                         err_desc = "sits_fromSATVEG: Invalid satellite param")
    ensurer::ensure_that(prefilter, (.) %in% sits.env$config$SATVEG_prefilter,
                         err_desc = "sits_fromSATVEG: prefilter choice is not available")

    status <- TRUE
    return(invisible(status))

}
#' @title Retrieve a time series from the SATVEG service
#' @name .sits_ts_from_SATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service
#'
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param satellite       the satellite ("terra", "aqua", "comb")
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @return status         TRUE if no problems are detected
#'
.sits_ts_from_SATVEG <- function(longitude, latitude, satellite, prefilter){

    # the parameter filter is not used
    filter <- ""
    filter_par <- ""
    # read the timeline only once
    has_timeline <- FALSE

    # URL to access SATVEG services
    URL <- paste0(sits.env$config$SATVEG_server, sits.env$config$SATVEG_account)

    # bands available in SATVEG
    bands <- sits.env$config$SATVEG_bands

    # read each of the bands separately
    for (b in bands) {
        # Build the URL to retrieve the time series
        URL_ts <- paste0(URL, b, "/ponto", "/", longitude, "/", latitude, "/", satellite, "/",
                         prefilter, "/", filter, "/", filter_par)

        # Get the data from SATVEG service
        satveg.txt <-  RCurl::getURL(URL_ts)

        # Retrieve the time series
        # find the place where the series ends and the dates start
        pos1 <- regexpr("listaDatas", satveg.txt)
        # find the position where dates (in text format) end
        pos1 <- pos1[1] - 4
        # extract the time series in text format
        ts.txt <- substr(satveg.txt, 16, pos1)
        # convert the time series to vector format
        ts_b <- tibble::tibble(as.double(unlist(strsplit(ts.txt, ","))))
        names(ts_b) <- b

        if (!has_timeline) {
            # Retrieve the time line
            # find the end of the dates
            pos2 <- regexpr("]}", satveg.txt)
            # extract the time line in text format
            timeline <- substr(satveg.txt, pos1 + 17, pos2 - 2)
            # convert to vector of text dates
            timeline <- unlist(strsplit(timeline, '\",\"'))
            # convert to a vector of timelines
            Sys.setlocale("LC_TIME", "C")
            timeline <- lubridate::as_date(lubridate::parse_date_time(timeline, c("%b %d, %Y")))

            # create a tibble to store the data
            ts.tb <- tibble::tibble(Index = timeline)

            has_timeline <- TRUE
        }
        # store the band in the tibble
        ts.tb <- dplyr::bind_cols(ts.tb, ts_b)
    }
    return(ts.tb)
}

#' @title Retrieve a coverage name from the SATVEG service
#' @name .sits_coverage_SATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a coverage name based on the capabilities of the SATVEG service
#'
#' @param satellite       the satellite ("terra", "aqua", "comb")
#' @return coverage       the name of the coverage
#'
.sits_coverage_SATVEG <- function(satellite) {
    if (satellite == "terra")
        coverage <- "MOD13Q1_C6_Terra"
    else {
        if (satellite == "aqua")
            coverage <- "MOD13Q1_C6_Aqua"
        else
            coverage <- "MOD13Q1_C6_Terra_Aqua"
    }
    return(coverage)
}
