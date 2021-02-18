#' @title Provides information about one cube of the SATVEG time series service
#' @name .sits_satveg_cube
#' @keywords internal
#'
#' @description Creates a tibble with metadata about a given cube.
#'
#' @param name       Name of the cube.
.sits_satveg_cube <- function(name) {
    satellite <- "TERRA"
    sensor <- "MODIS"
    # get the bands
    bands <- .sits_config_satveg_bands()

    # get scale factors, missing values and minimum values
    scale_factors <- .sits_config_scale_factors(sensor, bands)
    missing_values <- .sits_config_missing_values(sensor, bands)
    minimum_values <- .sits_config_minimum_values(sensor, bands)
    maximum_values <- .sits_config_maximum_values(sensor, bands)

    # get the timeline
    timeline <- lubridate::as_date(.sits_satveg_timeline())

    # get the size of the cube
    size <- .sits_config_satveg_size(name)
    nrows <- as.integer(size["nrows"])
    ncols <- as.integer(size["ncols"])

    # get the bounding box of the cube
    bbox <- .sits_config_satveg_bbox(name)
    xmin <- as.numeric(bbox["xmin"])
    xmax <- as.numeric(bbox["xmax"])
    ymin <- as.numeric(bbox["ymin"])
    ymax <- as.numeric(bbox["ymax"])

    # get the resolution of the product
    res <- .sits_config_resolution(sensor)
    xres <- as.numeric(res["xres"])
    yres <- as.numeric(res["yres"])

    # get the projection of the SATVEG data
    crs <- .sits_config_satveg_projection(name)

    URL <- .sits_config_satveg_access()

    # create a tibble to store the metadata
    cube_satveg <- .sits_cube_create(
        type = "SATVEG",
        URL = URL,
        satellite = satellite,
        sensor = sensor,
        name = name,
        bands = bands,
        scale_factors = scale_factors,
        missing_values = missing_values,
        minimum_values = minimum_values,
        maximum_values = maximum_values,
        timelines = list(timeline),
        nrows = nrows,
        ncols = ncols,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        xres = xres,
        yres = yres,
        crs = crs
    )

    class(cube_satveg) <- c("satveg_cube", class(cube_satveg))

    return(cube_satveg)
}

#' @title Obtain one timeSeries from the EMBRAPA SATVEG server
#' @name .sits_from_satveg
#' @keywords internal
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns one set of MODIS time series provided by the EMBRAPA
#' Given a location (lat/long), retrieve the "ndvi" and "evi" bands from SATVEG
#' If start and end date are given, the function
#' filters the data to limit the temporal interval.
#'
#' @param cube            The data cube metadata that describes the SATVEG data.
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      The start date of the period.
#' @param end_date        The end date of the period.
#' @param label           Label to attach to the time series (optional).
#' @return A sits tibble.
.sits_from_satveg <- function(cube,
                              longitude,
                              latitude,
                              start_date = NULL,
                              end_date = NULL,
                              label = "NoClass") {

    # check parameters
    assertthat::assert_that(!purrr::is_null(longitude),
        msg = "sits_from_satveg: Missing longitude info"
    )
    assertthat::assert_that(!purrr::is_null(latitude),
        msg = "sits_from_satveg: Missing latitude info"
    )

    # retrieve the time series
    ts <- .sits_ts_from_satveg(longitude, latitude, cube$name)

    # filter the dates
    if (!purrr::is_null(start_date) & !purrr::is_null(end_date)) {
          ts <- dplyr::filter(ts, dplyr::between(
              ts$Index,
              start_date, end_date
          ))
      } else {
        start_date <- as.Date(ts$Index[1])
        end_date <- as.Date(ts$Index[NROW(ts)])
    }

    # create a tibble to store the SATVEG data
    data <- .sits_tibble()
    # add one row to the tibble
    data <- tibble::add_row(data,
        longitude = longitude,
        latitude = latitude,
        start_date = start_date,
        end_date = end_date,
        label = label,
        cube = cube$name,
        time_series = list(ts)
    )
    # rename the SATVEG bands to uppercase
    sits_bands(data) <- .sits_config_satveg_bands()
    return(data)
}

#' @title Retrieve a time series from the SATVEG service
#' @name .sits_ts_from_satveg
#' @keywords internal
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service.
#'
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param name            Name of the desired data cube in SATVEG
#' @return                A tibble containing a time series
.sits_ts_from_satveg <- function(longitude, latitude, name) {
    # verifies if RCurl package is installed
    if (!requireNamespace("RCurl", quietly = TRUE)) {
        stop("RCurl required for this function to work.
             Please install it.", call. = FALSE)
    }
    # set the prefilter
    .prefilter <- 1
    # the parameter filter is not used
    filter <- ""
    filter_par <- ""

    # URL to access SATVEG services
    url <- .sits_config_satveg_url()

    # bands available in SATVEG
    bands <- .sits_config_satveg_bands()
    # bands in SATVEG are lowercase
    bands <- tolower(bands)
    # vector to hold the timeline (used once only)
    get_times <- rep(FALSE,  times = length(bands))
    get_times[1] <- TRUE

    # read each of the bands separately
    ts_bands_lst <- purrr::map2(bands, get_times,
                                function(b, gt) {
        # Build the URL to retrieve the time series
        url_ts <- paste0(
          url, b, "/ponto", "/", longitude, "/", latitude, "/",
          name, "/", .prefilter, "/", filter, "/", filter_par
        )
        # Get the data from SATVEG service
        satveg <- RCurl::getURL(url_ts)

        # did we get the data?
        if (grepl("Acesso", satveg)) {
          message("SATVEG service not accessible")
          return(NULL)
        }
        # Retrieve the time series
        # find the place where the series ends and the dates start
        pos1 <- regexpr("listaDatas", satveg)
        # find the position where dates (in text format) end
        pos1 <- pos1[1] - 4
        # extract the time series in text format
        t <- substr(satveg, 16, pos1)
        # convert the time series to vector format
        ts <- tibble::tibble(as.double(unlist(strsplit(t, ","))))
        names(ts) <- b
        # read the timeline only once
        if (gt) {
          timeline <- .sits_satveg_timeline_from_txt(satveg)
          # create a tibble to store the data
          index <- tibble::tibble(Index = timeline)
          # store the band in the tibble
          ts <- dplyr::bind_cols(index, ts)
        }
        return(ts)
      })

    ts_satveg <- tibble::as_tibble(do.call(cbind, ts_bands_lst))
    return(ts_satveg)
}

#' @title Retrieve a timeline from the SATVEG service based on text expression
#' @name .sits_satveg_timeline_from_txt
#' @keywords internal
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service.
#'
#' @param satveg   Information retrieved from SATVEG (in text format).
.sits_satveg_timeline_from_txt <- function(satveg) {
    # Retrieve the time series
    # find the place where the series ends and the dates start
    pos1 <- regexpr("listaDatas", satveg)
    # find the position where dates (in text format) end
    pos1 <- pos1[1] - 4

    # Retrieve the time line
    # find the end of the dates
    pos2 <- regexpr("]}", satveg)
    # extract the time line in text format
    timeline <- substr(satveg, pos1 + 17, pos2 - 2)
    # convert to vector of text dates
    timeline <- unlist(strsplit(timeline, '\",\"'))
    # convert to a vector of timelines
    Sys.setlocale("LC_TIME", "C")
    timeline <- lubridate::as_date(lubridate::parse_date_time(
        timeline,
        c("%b %d, %Y")
    ))

    return(timeline)
}

#' @title Retrieve a timeline for the SATVEG service
#' @name .sits_satveg_timeline
#' @keywords internal
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service.
.sits_satveg_timeline <- function() {
    # set a dummy longitude and latitude
    longitude <- -55.50563
    latitude <- -11.71557

    # set filter parameters
    filter <- ""
    filter_par <- ""
    prefilter <- "1"

    # set the name of one of the bands
    band <- "ndvi"
    cube <- "terra"
    # URL to access SATVEG services
    URL <- .sits_config_satveg_url()

    # Build the URL to retrieve the time series
    URL_ts <- paste0(
        URL, band, "/ponto", "/", longitude, "/",
        latitude, "/", cube, "/",
        prefilter, "/", filter, "/", filter_par
    )

    # Get the data from SATVEG service
    satveg <- RCurl::getURL(URL_ts)

    timeline <- .sits_satveg_timeline_from_txt(satveg)

    return(timeline)
}

#' @title Check that the SATVEG service is working
#' @name .sits_satveg_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return Boolean that indicates if SATVEG is operating
.sits_satveg_check <- function() {
    # Retrieve the URL to test for SATVEG access
    url_test <- .sits_config_satveg_access()
    check <- tryCatch({
        # tries to connect to the SATVEG service
        satveg <- RCurl::getURL(url_test)
        assertthat::assert_that(length(satveg) > 0,
                              msg = "SATVEG service not available"
                               )
        },
        error = function(e) {
            msg <- paste0("SATVEG service not available")
            message(msg)
        }
    )
    # did we get an error?
    if (inherits(check, "error")) {
          return(FALSE)
      } else {
          return(TRUE)
      }
}
