#' @title Provides information about one coverage of the SATVEG time series service
#' @name .sits_coverage_SATVEG
#'
#' @description creates a tibble with metadata about a given coverage
#'
#' @param name       name of the coverage
#' @param timeline   timeline of the coverage
#'
.sits_coverage_SATVEG <- function(name, timeline) {

    service <- "SATVEG"
    # get the bands
    bands <- .sits_get_bands(service, name)

    # get the timeline
    if (purrr::is_null(timeline))
        timeline <- .sits_SATVEG_timeline()

    # get the size of the coverage
    size <- .sits_get_size(service, name)
    # get the bounding box of the coverage
    bbox <- .sits_get_bbox(service, name)
    # get the resolution of the product
    res <- .sits_get_resolution(service, name)
    # get the CRS projection
    crs <- .sits_get_projection(service, name)

    scale_factors <- .sits_get_scale_factors(service, name, bands)

    missing_values <- .sits_get_missing_values(service, name, bands)

    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_objs         = NA,
                                  name           = name,
                                  service        = service,
                                  bands          = list(bands),
                                  scale_factors  = list(scale_factors),
                                  missing_values = list(missing_values),
                                  timeline       = list(timeline),
                                  nrows          = as.integer(size["nrows"]),
                                  ncols          = as.integer(size["ncols"]),
                                  xmin           = as.numeric(bbox["xmin"]),
                                  xmax           = as.numeric(bbox["xmax"]),
                                  ymin           = as.numeric(bbox["ymin"]),
                                  ymax           = as.numeric(bbox["ymax"]),
                                  xres           = as.numeric(res["xres"]),
                                  yres           = as.numeric(res["yres"]),
                                  crs            = crs,
                                  files          = NA)


    return(coverage.tb)
}
#' @title Obtain one timeSeries from the EMBRAPA SATVEG server and load it on a sits tibble
#' @name .sits_fromSATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns one set of MODIS time series provided by the EMBRAPA server (SATVEG)
#' Given a location (lat/long), the function retrieves the "ndvi" and "evi" bands from SATVEG
#' and inclues the data on a stis tibble. If start and end date are given, the function
#' filter the data to limit the temporal interval.
#'
#' @param coverage        the coverage metadata with the SATVEG information
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series (optional)
#' @return data.tb        a SITS tibble
#'
.sits_fromSATVEG <- function(coverage,
                             longitude,
                             latitude,
                             start_date  = NULL,
                             end_date    = NULL,
                             prefilter   = "1",
                             label       = "NoClass") {

    # check parameters
    ensurer::ensure_that(longitude, !purrr::is_null(.),
                         err_desc = "sits_fromSATVEG: Missing longitude info")
    ensurer::ensure_that(latitude,  !purrr::is_null(.),
                         err_desc = "sits_fromSATVEG: Missing latitude info")

    # retrieve the time series
    ts.tb <- .sits_ts_from_SATVEG(longitude, latitude, coverage$name, prefilter)

    # filter the dates
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date))
        ts.tb <- dplyr::filter(ts.tb, dplyr::between(ts.tb$Index, start_date, end_date))
    else {
        start_date <- as.Date(ts.tb$Index[1])
        end_date   <- as.Date(ts.tb$Index[NROW(ts.tb)])
    }

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
                               coverage     = coverage$name,
                               time_series  = ts.lst
    )
    return(data.tb)
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
    URL <- .sits_get_server("SATVEG")

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
    band <- "ndvi"
    coverage <- "terra"
    # URL to access SATVEG services
    URL <- .sits_get_server("SATVEG")

    # Build the URL to retrieve the time series
    URL_ts <- paste0(URL, band, "/ponto", "/", longitude, "/", latitude, "/", coverage, "/",
                     prefilter, "/", filter, "/", filter_par)

    # Get the data from SATVEG service
    satveg.txt <-  RCurl::getURL(URL_ts)

    timeline <- .sits_getSATVEG_timeline_from_txt(satveg.txt)

    return (timeline)
}

