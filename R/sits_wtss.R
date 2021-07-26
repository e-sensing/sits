#' @title Obtain one time series from WTSS server and load it on a sits tibble
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
    if (!requireNamespace("Rwtss", quietly = TRUE)) {
        stop("Please install package Rwtss.", call. = FALSE)
    }

    # Try to find the access key as an environment variable
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    assertthat::assert_that(
        nchar(bdc_access_key) != 0,
        msg = "sits_cube: BDC_ACCESS_KEY needs to be provided"
    )

    # check start and end dates
    timeline <- sits_timeline(cube)
    if (purrr::is_null(start_date)) {
        start_date <- lubridate::as_date(timeline[1])
    }
    if (purrr::is_null(end_date)) {
        end_date <- lubridate::as_date(timeline[length(timeline)])
    }


    bands <- .source_bands_to_source(source = .cube_source(cube),
                                     collection = .cube_collection(cube),
                                     bands = bands)

    # retrieve the time series from the service
    tryCatch({
        ts <- Rwtss::time_series(URL = cube$file_info[[1]]$path[[1]],
                                 name = cube$collection,
                                 attributes = bands,
                                 longitude = longitude,
                                 latitude = latitude,
                                 start_date = start_date,
                                 end_date = end_date,
                                 token = bdc_access_key
        )
    },
    warning = function(e){
        paste("sits_get_data:", e)
    })

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
    }
    # return the tibble with the time series
    return(ts)
}
