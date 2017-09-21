#' @title Obtain the dates of subset of an input data set for classification
#' @name .sits_subset_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a start and end date for classification,
#'              an interval for classification and an interval for data retrieval,
#'              produce a list of start and end dates inside a classification period
#'              that matches the expected start and end dates
#'
#' @param  start_date      the starting date of the classification
#' @param  end_date        the end date of the classification
#' @param  interval        the period between two classifications
#' @param  data_interval   the period of the input data to be extracted for each classification
#' @return subset_dates.lst     a list of start and end points of the input time series to be extracted
#'                         for classification
#'
.sits_subset_dates <- function (start_date, end_date, interval, data_interval){

    subset_dates.lst <- list()

    subset_start_date <- lubridate::as_date(start_date)
    while (subset_start_date < end_date){
        subset_end_date <- lubridate::as_date(subset_start_date + lubridate::as.period (data_interval))
        subset_dates.lst [[length(subset_dates.lst) + 1 ]] <- c(subset_start_date, subset_end_date)
        subset_start_date <- lubridate::as_date(subset_start_date + lubridate::as.period (interval))
    }
    return (subset_dates.lst)
}

#' @title Tests if an XY position is inside a ST Raster Brick
#' @name .sits_XY_inside_raster
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
.sits_XY_inside_raster <- function (xy, raster.tb){

    if (xy[1,"X"] < raster.tb[1,]$xmin) return (FALSE)
    if (xy[1,"X"] > raster.tb[1,]$xmax) return (FALSE)
    if (xy[1,"Y"] < raster.tb[1,]$ymin) return (FALSE)
    if (xy[1,"Y"] > raster.tb[1,]$Ymax) return (FALSE)
    return (TRUE)
}

#' @title Find the nearest date to a set of reference dates in a sorted input
#' @name .sits_nearest_date
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a vector of dates (typically coming from a time series)
#'              and a set of reference dates (typically coming from a set of patterns) and
#'              finds the set of dates in the input that are nearest to the each
#'              reference date.
#'
#' @param dates      a vector of dates
#' @param ref_dates   a vector of reference dates
#' @return bool      TRUE if XY is inside the raster extent, FALSE otherwise
#'
.sits_nearest_date <- function (dates, ref_dates){

    # convert all dates to julian
    first_date   <- lubridate::as_date(paste0(lubridate::year(dates[1]), "-01-01"))
    julian_dates <- as.integer (dates - first_date)
    julian_refs  <- as.integer (lubridate::as_date(ref_dates) - first_date)

    nearest_dates <- vector()

    julian_refs %>%
        purrr::map (function (jday){
            julian_ref  <- .sits_binary_search (julian_dates, jday)
            nearest_date <- lubridate::as_date (first_date + julian_ref)
            nearest_dates[length (nearest_dates) + 1] <<- nearest_date
        })
     return (nearest_dates)
}


#' @title Implement a binary search to find the nearest date
#' @name .sits_binary_search
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Performs a binary search in a ordered set of integer values and
#'              returns the values closest to a reference
#'
#' @param values     a vector of ordered integers
#' @param val        a reference value
#' @return nearest   the input value closest to the reference one
.sits_binary_search <- function (values, val){
    if (length(values) == 1) return (values[1])
    if (length(values) == 2) {
        if ((values[1] - val) < (values[2] - val))
            return (values[1])
        else
            return (values[2])
    }
    mid <- as.integer(length(values)/2)
    if (val < values[mid])
        .sits_binary_search(values[1:mid], val)
    else
        .sits_binary_search(values[mid + 1 : length(values)], val)

}


