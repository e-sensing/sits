#' @title Create a sits table to store the time series information
#' @name sits_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function returns an empty sits table.
#' SITS tibbles are the main structures of the "sits" package.
#  They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#' Most functions on the sits package use a sits tibble as input (with additional parameters)
#' and a sits tibble as output. This allows for chaining of operation on time series.
#'
#' @return result.tb  a tibble in SITS format
#' @export

sits_tibble <- function () {
    result.tb <- tibble::tibble(longitude   = double(),
                                latitude    = double (),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character(),
                                coverage    = character(),
                                time_series = list()
    )
    class (result.tb) <- append (class(result.tb), "sits_tibble")
    return (result.tb)
}


#' @title Tests if a sits tibble is valid
#' @name .sits_test_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a SITS tibble exists or has data inside
#'
#' @param data.tb  a SITS tibble
#' @return returns TRUE if data.tb has data.
#'
.sits_test_tibble <- function (data.tb) {
    ensurer::ensure_that(data.tb, !purrr::is_null(.),
                         err_desc = "input data not provided")
    ensurer::ensure_that(data.tb, NROW(.) > 0,
                         err_desc = "input data is empty")
    return (TRUE)
}

#' @title Create an empty tibble to store the results of predictions
#' @name sits_tibble_prediction
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create atibble to store the results of predictions
#' @param  distances.tb    a tibble with distances
#' @param  timeline        the timeline of the data set
#' @param  time_index.lst  The subsets of the timeline
#'
#' @return distances.tb   a tibble to store the predictions
#' @export
#'
sits_tibble_prediction <- function (distances.tb, timeline, time_index.lst){

    nrows <- nrow(distances.tb) * length (time_index.lst)

    from_dates <- vector()
    to_dates   <- vector()

    for (i in 1:nrow(distances.tb)){
        time_index.lst %>%
            purrr::map (function (idx){
               from_dates <<-  c(as.Date(from_dates), as.Date(timeline [idx[1] - 2]))
               to_dates   <<-  c(as.Date(to_dates),   as.Date(timeline [idx[2] - 2]))
            })
    }
    # save the results
    predict.tb   <- tibble::tibble(from      = as.Date(from_dates),
                                   to        = as.Date(to_dates),
                                   distance  = rep(0.0, nrows),
                                   predicted = rep("NoClass", nrows)
    )
    return (predict.tb)
}
#' @title Create an empty tibble to store the results of classifications
#' @name sits_tibble_classification
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty tibble to store the results of classification
#'
#' @return result.tb   a tibble to store the result of classifications
#' @export
#'
sits_tibble_classification <- function () {
    result.tb <- tibble::tibble(longitude   = double(),
                                latitude    = double (),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character(),
                                coverage    = character(),
                                time_series = list(),
                                predicted   = list()
    )
    return (result.tb)
}

#' @title Create an empty tibble to store the metadata of a coverage
#' @name sits_tibble_coverage
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create an empty tibble to store the metadata about a coverage
#'
#' @return coverage.tb   a tibble to store the metadata
#' @export
#'
sits_tibble_coverage <- function () {
    result.tb <- tibble::tibble(wtss.obj       = list(),
                                name           = character(),
                                bands          = list(),
                                start_date     = as.Date(character()),
                                end_date       = as.Date(character()),
                                timeline       = list(),
                                xmin           = double(),
                                xmax           = double(),
                                ymin           = double(),
                                ymax           = double(),
                                xres           = double(),
                                yres           = double(),
                                crs            = character()
    )
    return (result.tb)
}

#' @title Create one line of metadata tibble to store the description of a spatio-temporal raster
#' @name sits_tibble_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates one line of tibble containing the metadata for
#'               a set of spatio-temporal raster files.
#'
#' @param raster.obj     Valid Raster object (associated to filename)
#' @param band           Name of band (either raw or classified)
#' @param timeline       Timeline of data collection
#' @param scale_factor   Scale factor to correct data
#' @return raster.tb     A tibble for storing metadata about a spatio-temporal raster
#' @export

sits_tibble_raster <- function (raster.obj, band, timeline, scale_factor){

    raster.tb <- tibble::tibble (
        r_obj           = list(raster.obj),
        ncols           = raster.obj@ncols,
        nrows           = raster.obj@nrows,
        band            = band,
        start_date      = lubridate::as_date(timeline[1]),
        end_date        = lubridate::as_date(timeline[length(timeline)]),
        timeline        = list(timeline),
        xmin            = raster.obj@extent@xmin,
        xmax            = raster.obj@extent@xmax,
        ymin            = raster.obj@extent@ymin,
        ymax            = raster.obj@extent@ymax,
        xres            = raster::xres (raster.obj),
        yres            = raster::yres (raster.obj),
        scale_factor    = scale_factor,
        crs             = raster.obj@crs@projargs,
        name            = raster.obj@file@name
    )
    return (raster.tb)
}
