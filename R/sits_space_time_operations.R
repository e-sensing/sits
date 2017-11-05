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
    if (xy[1,"Y"] > raster.tb[1,]$ymax) return (FALSE)
    return (TRUE)
}

#' @title Find the nearest date to a set of reference dates in a sorted input
#' @name .sits_nearest_date
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a date (typically coming from a set of patterns)
#'              and a timeline (typically coming from a time series) and
#'              finds the date in the timeline that is nearest to the reference date.
#'
#' @param date        a dates
#' @param timeline    a vector of reference dates
#' @return n_date     nearest date
#'
.sits_nearest_date <- function (date, timeline){

    # convert all dates to julian
    first_date    <- lubridate::as_date(paste0(lubridate::year(timeline[1]), "-01-01"))
    timeline_jul  <- as.integer (timeline - first_date)
    julian_date   <- as.integer (date - first_date)

    # find the closest julian day in the interval
    julian_ref  <- .sits_binary_search (timeline_jul, julian_date)
    nearest_date <- lubridate::as_date (first_date + julian_ref)

    return (nearest_date)
}
#' @title Test if starting date fits with the timeline
#' @name .sits_is_valid_start_date
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description A timeline is a list of dates where observations are available. This
#' functions estimates if a date is valid by comparing it to the timeline. If the date's estimate
#' is not inside the timeline and the difference between the date and the first date of timeline is
#' greater than the acquisition interval of the timeline, then we conclude the date is not valid.
#'
#' @param date        a dates
#' @param timeline    a vector of reference dates
#' @return bool       is this is valid starting date?
#'
.sits_is_valid_start_date <- function (date, timeline){

    # is the date inside the timeline?
    if (date %within% lubridate::interval (timeline[1], timeline[length(timeline)])) return (TRUE)
    # what is the difference in days between the last two days of the timeline?
    timeline_diff <- as.integer (timeline[2] - timeline[1])
    # if the difference in days in the timeline is smaller than the difference
    # between the reference date and the first date of the timeline, then
    # we assume the date is valid
    if (abs(as.integer(date - timeline[1]))  <= timeline_diff ) return (TRUE)

    return (FALSE)
}
#' @title Test if end date fits inside the timeline
#' @name .sits_is_valid_end_date
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description A timeline is a list of dates where observations are available. This
#' functions estimates if a date is valid by comparing it to the timeline. If the date's estimate
#' is not inside the timeline and the difference between the date and the last date of timeline is
#' greater than the acquisition interval of the timeline, then we conclude the date is not valid.
#'
#' @param date        a dates
#' @param timeline    a vector of reference dates
#' @return n_date     nearest date
#'
.sits_is_valid_end_date <- function (date, timeline){

    # is the date inside the timeline?

    if (date %within% lubridate::interval (timeline[1], timeline[length(timeline)])) return (TRUE)
    # what is the difference in days between the last two days of the timeline?
    timeline_diff <- as.integer (timeline[length(timeline)] - timeline[length(timeline) - 1])
    # if the difference in days in the timeline is smaller than the difference
    # between the reference date and the last date of the timeline, then
    # we assume the date is valid
    if (abs(as.integer(date - timeline[length(timeline)]))  <= timeline_diff ) return (TRUE)

    return (FALSE)
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


#' @title Create a list of time indexes from the dates index
#' @name  .sits_time_index
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  dates_index.lst  A list of dates with the subsets of the input data
#' @param  timeline        The timeline of the data set
#' @param  bands           Bands used for classification
#' @return  time_index.lst  The subsets of the timeline
#' @export
.sits_time_index <- function (dates_index.lst, timeline, bands) {

    # create an empty list of time index
    time_index.lst <- list()

    # transform the dates index (a list of dates) to a list of indexes
    # this speeds up extracting the distances for classification
    dates_index.lst %>%
        purrr::map (function (idx){
            index_ts <- vector()
            for (i in 1:length(bands)){
                idx1 <- idx[1] + (i - 1)*length(timeline)
                index_ts [length(index_ts) + 1 ] <- idx1
                idx2 <- idx[2] + (i - 1)*length(timeline)
                index_ts [length(index_ts) + 1 ] <- idx2
            }
            time_index.lst[[length(time_index.lst) + 1]] <<- index_ts
        })
    return (time_index.lst)
}
