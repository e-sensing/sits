#' @title Provides information about one cube of the WTSS service
#' @name .sits_wtss_cube
#' @keywords internal
#'
#' @description Uses the WTSS services to print information and save metadata
#' about a chosen cube.
#'
#' @param URL        URL of the service provider.
#' @param name       Name of the cube.
.sits_wtss_cube <- function(URL, name) {

    # verifies if wtss package is installed
    if (!requireNamespace("wtss", quietly = TRUE)) {
        stop("Please install package wtss.", call. = FALSE)
    }

    # describe the cube based on the WTSS API
    cov <- wtss::describe_coverage(URL, name, .print = FALSE)
    assertthat::assert_that(!purrr::is_null(cov),
        msg = ".sits_wtss_cube: failed to get cube description in WTSS"
    )

    # temporal extent
    timeline <- lubridate::as_date(cov$timeline[[1]])

    # retrieve information about the bands
    # all bands in SITS are uppercase
    bands_wtss <- toupper(cov$bands[[1]])

    # create a tibble to store the metadata
    cube_wtss <- .sits_cube_create(
        type = "WTSS",
        URL = URL,
        satellite = cov$satellite,
        sensor = cov$sensor,
        name = cov$name,
        bands = bands_wtss,
        scale_factors = cov$scale_factors[[1]],
        missing_values = cov$missing_values[[1]],
        minimum_values = cov$minimum_values[[1]],
        maximum_values = cov$maximum_values[[1]],
        timelines = list(timeline),
        nrows = cov$nrows,
        ncols = cov$ncols,
        xmin = cov$xmin,
        xmax = cov$xmax,
        ymin = cov$ymin,
        ymax = cov$ymax,
        xres = cov$xres,
        yres = cov$yres,
        crs  = cov$crs
    )

    class(cube_wtss) <- c("wtss_cube", class(cube_wtss))
    # return the tibble with cube info
    return(cube_wtss)
}

#' @title Obtain one timeSeries from WTSS server and load it on a sits tibble
#' @name .sits_from_wtss
#' @keywords internal
#'
#' @description Returns one set of time series provided by a WTSS server
#' Given a location (lat/long), and start/end period, and WTSS server info,
#' retrieve a time series and include it on a stis tibble.
#' A Web Time Series Service (WTSS) is a light-weight service that
#' retrieves one or more time series in JSON format from a data base.
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016, p.166-177.
#'
#' @param cube            Metadata about the cube associated to the WTSS.
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param start_date      Date with the start of the period.
#' @param end_date        Date with the end of the period.
#' @param bands           Names of the bands of the cube.
#' @param label           Label to attach to the time series (optional).
#' @return                A sits tibble.
.sits_from_wtss <- function(cube,
                            longitude,
                            latitude,
                            start_date = NULL,
                            end_date = NULL,
                            bands = NULL,
                            label = "NoClass") {

    # verifies if wtss package is installed
    if (!requireNamespace("wtss", quietly = TRUE)) {
        stop("Please install package wtss.", call. = FALSE)
    }

    # check start and end dates
    timeline <- sits_timeline(cube)
    if (purrr::is_null(start_date)) {
          start_date <- lubridate::as_date(timeline[1])
      }
    if (purrr::is_null(end_date)) {
          end_date <- lubridate::as_date(timeline[length(timeline)])
      }

    # Temporary hack - WTSS in URL "http://www.esensing.dpi.inpe.br/wtss"
    # is configured for lowercase bands
    wtss_attributes <- tolower(bands)

    # retrieve the time series from the service
    ts <- wtss::time_series(cube$URL,
        name = cube$name,
        attributes = wtss_attributes,
        longitude = longitude,
        latitude = latitude,
        start_date = start_date,
        end_date = end_date
    )
    # change the class of the data
    # before - class "wtss"
    # now - class "sits"
    if (!purrr::is_null(ts)) {
        class(ts) <- setdiff(class(ts), "wtss")
        class(ts) <- c("sits", class(ts))
        # add a label column
        if (label != "NoClass") {
            ts$label <- label
        }
        # convert name
        ts <- .sits_tibble_rename(ts)
        # band names are uppercase in SITS
        ts <- sits_rename(ts, toupper(sits_bands(ts)))
    }
    # return the tibble with the time series
    return(ts)
}
#' @title Check that the URL of WTSS service is working
#' @name .sits_wtss_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param URL        URL of the WTSS service.
#' @param name       name of the converage
#' @return check     TRUE or FALSE
.sits_wtss_check <- function(URL, name) {
    # verifies if wtss package is installed
    if (!requireNamespace("wtss", quietly = TRUE)) {
        stop("Please install package wtss.", call. = FALSE)
    }

    # check that URL of the WTSS service has been provided
    assertthat::assert_that(!purrr::is_null(URL),
        msg = "sits_cube: WTSS service needs URL"
    )

    # is the WTSS service working?
    coverages <- wtss::list_coverages(URL)
    if (purrr::is_null(coverages)) {
          return(FALSE)
      }
    # is the cube in the list of cubes?
    assertthat::assert_that(name %in% coverages,
        msg = paste0(
            "sits_cube: ", name,
            " not available in the WTSS server"
        )
    )

    return(TRUE)
}
