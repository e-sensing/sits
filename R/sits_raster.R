#' @title Relabel raster stack object
#' @name sits_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param file       file name of a raster file
#' @param timeline   a vector of dates with the timeline of the bands
#' @param bands      the bands contained in the file
#'
#' @description This function creates a tibble to store the information
#' about a raster time series
#'
#' @export
sits_raster <- function (file = NULL, timeline = NULL, bands = NULL){
    ensurer::ensure_that(file, !purrr::is_null(.), err_desc = "sits_raster: file name must be provided")
    ensurer::ensure_that(timeline, !purrr::is_null(.), err_desc = "sits_raster: timeline must be provided")
    ensurer::ensure_that(bands, !purrr::is_null(.), err_desc = "sits_raster: bands must be provided")

    raster.obj <- raster::raster (file)

    ntimes    <-  raster.obj@file@nbands

    ensurer::ensure_that(ntimes, (.) == length(timeline),
                         err_desc = "duration of timeline is not matched by raster size")

    raster.tb <- tibble::tibble (

        r_obj    = list(raster.obj),
        ncols    = raster.obj@ncols,
        nrows    = raster.obj@nrows,
        bands    = list(bands),
        timeline = list(timeline),
        min      = raster.obj@data@min,
        max      = raster.obj@data@max,
        xmin     = raster.obj@extent@xmin,
        xmax     = raster.obj@extent@xmax,
        ymin     = raster.obj@extent@ymin,
        ymax     = raster.obj@extent@ymax,
        crs      = raster.obj@crs@projargs,
        name     = raster.obj@file@name
    )
    return (raster.tb)
}

#' @title Relabel raster stack object
#' @name sits_raster_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function creates a raster tibble
#'
#' @export
#'
sits_raster_tibble <- function () {
    result.tb <- tibble::tibble(name        = character(),
                                r_obj       = list(),
                                ncols       = integer(),
                                nrows       = integer(),
                                bands       = list(),
                                ntimes      = integer(),
                                timeline    = list(),
                                xmax        = double(),
                                xmin        = double(),
                                ymax        = double(),
                                ymin        = double(),
                                crs         = character()
    )
    class (result.tb) <- append (class(result.tb), "sits_tibble")
    return (result.tb)
}
