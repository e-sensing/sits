
#' @title Retrieve a time series from the SATVEG service
#' @name .sits_satveg_ts_from_txt
#' @keywords internal
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a time series from the SATVEG service.
#'
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param cube            SATVEG cube.
#' @return                A tibble containing a time series.
.sits_satveg_ts_from_txt <- function(longitude, latitude, cube) {
    # set the prefilter
    .prefilter <- 1
    # the parameter filter is not used
    filter <- ""
    filter_par <- ""

    # URL to access SATVEG services
    url <- .source_url(source = .cube_source(cube = cube))

    # bands available in SATVEG
    bands <- .source_bands_band_name(
        source = .cube_source(cube = cube),
        collection = .cube_collection(cube = cube)
    )
    # bands in SATVEG are lowercase
    bands <- tolower(bands)
    # vector to hold the timeline (used once only)
    get_times <- rep(FALSE, times = length(bands))
    get_times[1] <- TRUE

    # read each of the bands separately
    ts_bands_lst <- purrr::map2(bands, get_times, function(b, gt) {
        # Build the URL to retrieve the time series
        url_ts <- paste(url, b, "ponto", longitude, latitude,
                        .cube_collection(cube = cube), .prefilter,
                        filter, filter_par,
                        sep = "/"
        )

        # Get the data from SATVEG service
        satveg <- httr::GET(url_ts)

        # did we get the data?
        if (httr::http_error(satveg)) {
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
        names(ts) <- toupper(b)
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
    # hack - SATVEG has different timelines for EVI and NDVI bands - 15 June 2021
    if (nrow(ts_bands_lst[[1]]) == nrow(ts_bands_lst[[2]])) {
        ts_satveg <- tibble::as_tibble(do.call(cbind, ts_bands_lst))
    } else {
        ts_satveg <- ts_bands_lst[[1]]
    }

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
