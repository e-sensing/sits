#' @title Retrieve a time series from the SATVEG service
#' @name .sits_ts_from_SATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service
#'
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param name            name of the desired coverage in SATVEG (see configuration file)
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param product         the SATVEG product we are using
#' @return status         TRUE if no problems are detected
#'
.sits_ts_from_SATVEG <- function(longitude, latitude, name, prefilter){

    # the parameter filter is not used
    filter <- ""
    filter_par <- ""
    # read the timeline only once
    has_timeline <- FALSE

    # URL to access SATVEG services
    URL <- .sits_get_account("SATVEG", name)

    # bands available in SATVEG
    bands <- .sits_get_bands("SATVEG", name)

    # read each of the bands separately
    for (b in bands) {
        # Build the URL to retrieve the time series
        URL_ts <- paste0(URL, b, "/ponto", "/", longitude, "/", latitude, "/", name, "/",
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
            timeline <- .sits_getSATVEG_timeline_from_txt(satveg.txt)

            # create a tibble to store the data
            ts.tb <- tibble::tibble(Index = timeline)

            has_timeline <- TRUE
        }
        # store the band in the tibble
        ts.tb <- dplyr::bind_cols(ts.tb, ts_b)
    }
    return(ts.tb)
}
#' @title Retrieve a timeline from the SATVEG service based on text expression
#' @name .sits_get_SATVEG_timeline_from_txt
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service
#'
#' @param satveg.txt   Information retrieved from SATVEG (in text format)
.sits_getSATVEG_timeline_from_txt <- function(satveg.txt){

    # Retrieve the time series
    # find the place where the series ends and the dates start
    pos1 <- regexpr("listaDatas", satveg.txt)
    # find the position where dates (in text format) end
    pos1 <- pos1[1] - 4

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

    return(timeline)
}

#' @title Retrieve a timeline for the SATVEG service
#' @name .sits_SATVEG_timeline
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service
#'
#'
.sits_SATVEG_timeline <- function() {

    # set a dummy longitude and latitude
    longitude <-  -55.50563
    latitude  <-  -11.71557

    # set filter parameters
    filter <- ""
    filter_par <- ""
    prefilter <- "1"

    # set the name of one of the bands
    name <- "ndvi"
    # URL to access SATVEG services
    URL <- .sits_get_account("SATVEG", name)

    # Build the URL to retrieve the time series
    URL_ts <- paste0(URL, name, "/ponto", "/", longitude, "/", latitude, "/", name, "/",
                     prefilter, "/", filter, "/", filter_par)

    # Get the data from SATVEG service
    satveg.txt <-  RCurl::getURL(URL_ts)

    timeline <- .sits_getSATVEG_timeline_from_txt(satveg.txt)

    return (timeline)
}

