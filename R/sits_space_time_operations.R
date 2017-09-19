#' @title Adjust the dates of an input data set with a reference data set
#' @name sits_match_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For correct classification, the time series of the input data set
#'              should be aligned to that of the reference data set (usually a set of patterns).
#'              This function aligns these data sets so that shape matching works correctly
#'
#' @param  input_start_date  the starting date of the input data set
#' @param  input_end_date    the end date of the input data set
#' @param  ref_start_date    the starting date of the reference data set
#' @param  ref_end_date      the end date of the reference data set
#' @param  interval          the interval that shape matching will be performed
#' @return breaks            the breaks that will be applied to the input data set
#'
#' @export
#'
sits_match_dates <- function (input_start_date, input_end_date, ref_start_date, ref_end_date, interval = "12 month"){

    #define the start and end days of the year based on the patterns
    ref_start_day  <- lubridate::yday(ref_start_date)
    ref_end_day    <- lubridate::yday(ref_end_date)

    # first match the start and end dates to the pattern start and end date for all years
    start_date  <- lubridate::as_date(paste0(lubridate::year(input_start_date), "-01-01")) + ref_start_day
    end_date    <- lubridate::as_date(paste0(lubridate::year(input_end_date),   "-01-01")) + ref_end_day

    # set the interval for increments
    p <-  lubridate::period(interval)

    # Check if the estimated starting date of the input is earlier than the starting date of the patterns
    # if this is not the case, try to start in the next interval
    if (start_date < input_start_date)
        start_date  <- lubridate::as_date(paste0(lubridate::year(input_start_date) + p, "-01-01")) + ref_start_day

    # Check if the estimated end date of the input is later than the end date of the patterns
    # if this is not the case, try to start in the previous interval
    if (end_date > input_end_date)
        end_date <- lubridate::as_date(paste0(lubridate::year(input_end_date) - p, "-01-01")) + ref_end_day

    # Now dates should be aligned, unless... the input data cannot fit inside the desired interval
    ensurer::ensures_that(start_date, (.) < end_date, err_desc = "data cannot fit inside pattern interval")

    # we have to break the input date into intervals that fit the patterns
    # so the temporal intervals will be correct for classification
    breaks <- seq(from = as.Date(start_date), to = as.Date(end_date), by = interval)

    return (breaks)
}

#' @title Tests if an XY position is inside a ST Raster Brick
#' @name sits_XY_inside_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function compares an XY position to the extent of a RasterBrick
#'              described by a raster metadata tibble, and return TRUE if the point is
#'              inside the extent of the RasterBrick object.
#'
#' @param xy         XY extent compatible with the R raster package
#' @param raster.tb  Tibble with metadata information about a raster data set
#' @return bool      TRUE if XY is inside the raster extent, FALSE otherwise
#'
#' @export
sits_XY_inside_raster <- function (xy, raster.tb){

    if (xy[1,"X"] < raster.tb[1,]$xmin) return (FALSE)
    if (xy[1,"X"] > raster.tb[1,]$xmax) return (FALSE)
    if (xy[1,"Y"] < raster.tb[1,]$ymin) return (FALSE)
    if (xy[1,"Y"] > raster.tb[1,]$Ymax) return (FALSE)
    return (TRUE)
}
