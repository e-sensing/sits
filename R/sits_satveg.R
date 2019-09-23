#' @title Provides information about one cube of the SATVEG time series service
#' @name .sits_satveg_cube
#'
#' @description Creates a tibble with metadata about a given cube.
#'
#' @param name       Name of the cube.
.sits_satveg_cube <- function(name) {

    service   <- "SATVEG"
    satellite <- "TERRA"
    sensor    <- "MODIS"
    # get the bands
    bands <- .sits_config_bands(service, name)

    # the data in unlabelled
    labels <- c("NoClass")

    # get scale factors, missing values and minimum values
    scale_factors  <- .sits_config_scale_factors(sensor,  bands)
    missing_values <- .sits_config_missing_values(sensor, bands)
    minimum_values <- .sits_config_minimum_values(sensor, bands)
    maximum_values <- .sits_config_maximum_values(sensor, bands)

    # get the timeline
    timeline <- lubridate::as_date(.sits_satveg_timeline())

    # get the size of the cube
    size <- .sits_config_size(service, name)
    nrows <- as.integer(size["nrows"])
    ncols <- as.integer(size["ncols"])

    # get the bounding box of the cube
    bbox <- .sits_config_bbox(service, name)
    xmin <-  as.numeric(bbox["xmin"])
    xmax <-  as.numeric(bbox["xmax"])
    ymin <-  as.numeric(bbox["ymin"])
    ymax <-  as.numeric(bbox["ymax"])

    # get the resolution of the product
    res  <- .sits_config_resolution(sensor)
    xres <-  as.numeric(res["xres"])
    yres <-  as.numeric(res["yres"])

    # get the CRS projection
    crs <- .sits_config_projection(service, name)


    URL <- .sits_config_providers(service)

    # create a tibble to store the metadata
    cube_satveg <- .sits_cube_create(service, URL, satellite, sensor, name, bands, labels,
                                     scale_factors, missing_values, minimum_values, maximum_values,
                                     list(timeline), nrows, ncols, xmin, xmax, ymin, ymax,
                                     xres, yres, crs)

    return(cube_satveg)
}

#' @title Obtain one timeSeries from the EMBRAPA SATVEG server and load it on a sits tibble
#' @name .sits_from_satveg
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns one set of MODIS time series provided by the EMBRAPA server (SATVEG)
#' Given a location (lat/long), the function retrieves the "ndvi" and "evi" bands from SATVEG
#' and inclues the data on a stis tibble. If start and end date are given, the function
#' filter the data to limit the temporal interval.
#'
#' @param cube            The data cube metadata that describes the SATVEG data.
#' @param longitude       A double value with the longitude of the chosen location.
#' @param latitude        A double value with the latitude of the chosen location.
#' @param start_date      The start date of the period.
#' @param end_date        The end date of the period.
#' @param bands           The bands to be retrieved.
#' @param label           A string with the label to attach to the time series (optional).
#' @param .prefilter      A string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @return A sits tibble.
.sits_from_satveg <- function(cube,
                             longitude,
                             latitude,
                             start_date  = NULL,
                             end_date    = NULL,
                             bands       = NULL,
                             label       = "NoClass",
                             .prefilter   = "1")
{

    # check parameters
    ensurer::ensure_that(longitude, !purrr::is_null(.),
                         err_desc = "sits_from_satveg: Missing longitude info")
    ensurer::ensure_that(latitude,  !purrr::is_null(.),
                         err_desc = "sits_from_satveg: Missing latitude info")

    # retrieve the time series
    ts.tb <- .sits_ts_from_satveg(longitude, latitude, cube$name, .prefilter)

    # filter the dates
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date))
        ts.tb <- dplyr::filter(ts.tb, dplyr::between(ts.tb$Index, start_date, end_date))
    else {
        start_date <- as.Date(ts.tb$Index[1])
        end_date   <- as.Date(ts.tb$Index[NROW(ts.tb)])
    }

    # filter bands
    bands <- .sits_cube_bands(cube)

    ts.tb <- ts.tb[, c("Index", bands)]

    # use a list to store the time series
    ts.lst <- list()
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the SATVEG data
    data <- .sits_tibble()
    # add one row to the tibble
    data    <- tibble::add_row(data,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = start_date,
                               end_date     = end_date,
                               label        = label,
                               cube         = cube$name,
                               time_series  = ts.lst
    )
    return(data)
}

#' @title Retrieve a time series from the SATVEG service
#' @name .sits_ts_from_satveg
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service.
#'
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param name            Name of the desired data cube in SATVEG (see configuration file).
#' @param .prefilter       String ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @return TRUE if no problems are detected.
.sits_ts_from_satveg <- function(longitude, latitude, name, .prefilter){
    # the parameter filter is not used
    filter <- ""
    filter_par <- ""
    # read the timeline only once
    has_timeline <- FALSE

    # URL to access SATVEG services
    URL <- .sits_config_server("SATVEG")

    # bands available in SATVEG
    bands <- .sits_config_bands("SATVEG", name)

    # read each of the bands separately
    for (b in bands) {
        # Build the URL to retrieve the time series
        URL_ts <- paste0(URL, b, "/ponto", "/", longitude, "/", latitude, "/", name, "/",
                         .prefilter, "/", filter, "/", filter_par)

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
            timeline <- .sits_satveg_timeline_from_txt(satveg.txt)

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
#' @name .sits_satveg_timeline_from_txt
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service.
#'
#' @param satveg.txt   Information retrieved from SATVEG (in text format).
.sits_satveg_timeline_from_txt <- function(satveg.txt){
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
#' @name .sits_satveg_timeline
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service.
.sits_satveg_timeline <- function() {
    # set a dummy longitude and latitude
    longitude <-  -55.50563
    latitude  <-  -11.71557

    # set filter parameters
    filter <- ""
    filter_par <- ""
    prefilter <- "1"

    # set the name of one of the bands
    band <- "ndvi"
    cube <- "terra"
    # URL to access SATVEG services
    URL <- .sits_config_server("SATVEG")

    # Build the URL to retrieve the time series
    URL_ts <- paste0(URL, band, "/ponto", "/", longitude, "/", latitude, "/", cube, "/",
                     prefilter, "/", filter, "/", filter_par)

    # Get the data from SATVEG service
    satveg.txt <-  RCurl::getURL(URL_ts)

    timeline <- .sits_satveg_timeline_from_txt(satveg.txt)

    return(timeline)
}

#' @title Check that the SATVEG service is working
#' @name .sits_satveg_check
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return Boolean that indicates if SATVEG is operating
.sits_satveg_check <- function() {
    # Retrieve the URL to test for SATVEG access
    URL_test <- .sits_config_satveg_access()
    check <- tryCatch({
        # tries to connect to the SATVEG service
        satveg.txt <-  RCurl::getURL(URL_test)
        ensurer::ensure_that(satveg.txt, length(.) > 0,
                             err_desc = "SATVEG service not available")
    }, error = function(e){
        msg <- paste0("SATVEG service not available")
        .sits_log_error(msg)
        message(msg)
    })
    # did we get an error?
    if (inherits(check, "error"))
        return(FALSE)
    else
        return(TRUE)
}
