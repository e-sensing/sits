#' @title Obtains the timeline for a coverage
#' @name .sits_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns the timeline for a given coverage
#'
#' @param  data.tb  A sits tibble (either a SITS tibble or a raster metadata)
.sits_timeline <- function (data.tb){

    timeline <-  NULL
    # is this a raster metadata?
    if ("timeline" %in% names(data.tb))
        timeline <- as.Date(data.tb[1,]$timeline[[1]])

    # is this a SITS tibble with the time series?
    if ("time_series" %in% names(data.tb))
        timeline <- lubridate::as_date(data.tb[1,]$time_series[[1]]$Index)

    ensurer::ensure_that (timeline, !purrr::is_null(.), err_desc = "sits_timeline: input does not contain a valid timeline")

    return (timeline)
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
#' @title Find dates in the input coverage that match those of the patterns
#' @name .sits_match_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For correct classification, the time series of the input data set
#'              should be aligned to that of the reference data set (usually a set of patterns).
#'              This function aligns these data sets so that shape matching works correctly
#'
#' @param timeline              Timeline of input observations (vector)
#' @param ref_start_date        The day of the year to be taken as reference for starting the classification
#' @param ref_end_date          The day of the year to be taken as reference for end the classification
#' @param interval              Period between two classification
#' @return subset_dates.lst     A list of breaks that will be applied to the input data set
#'
.sits_match_timeline <- function (timeline, ref_start_date, ref_end_date, interval = "12 month"){

    # make sure the timelines is a valid set of dates
    timeline <- lubridate::as_date (timeline)

    #define the input start and end dates
    input_start_date <- timeline [1]
    input_end_date   <- timeline [length(timeline)]

    # how many samples are there per interval?
    num_samples <- .sits_num_samples(timeline, interval)

    # what is the expected start and end dates based on the patterns?
    ref_st_mday  <- as.character(lubridate::mday(ref_start_date))
    ref_st_month <- as.character(lubridate::month(ref_start_date))
    year_st_date  <- as.character(lubridate::year(input_start_date))
    est_start_date    <- lubridate::as_date(paste0(year_st_date,"-",ref_st_month,"-",ref_st_mday))
    # find the actual starting date by searching the timeline
    start_date <- timeline[which.min(abs(est_start_date - timeline))]

    # is the start date a valid one?
    ensurer::ensure_that(start_date, .sits_is_valid_start_date(., timeline),
                         err_desc = ".sits_match_timelines: expected start date in not inside timeline of observations")

    # obtain the subset dates to break the input data set
    # adjust the dates to match the timeline
    subset_dates.lst <- list()

    # what is the expected end date of the classification?

    end_date <- timeline[which(timeline == start_date) + (num_samples - 1)]

    # go through the timeline of the data and find the reference dates for the classification
    while (!is.na(end_date)){
        # add the start and end date
        subset_dates.lst [[length(subset_dates.lst) + 1 ]] <- c(start_date, end_date)

        # estimate the next end date based on the interval
        next_start_date <- as.Date (start_date + lubridate::as.duration(interval))
        # define the estimated end date of the input data based on the patterns

        # find the actual start date by searching the timeline
        start_date <- timeline[which.min(abs(next_start_date - timeline))]
        # estimate
        end_date <- timeline[which(timeline == start_date) + (num_samples -1)]
    }
    # is the end date a valid one?
    end_date   <- subset_dates.lst[[length(subset_dates.lst)]][2]
    ensurer::ensure_that(end_date, .sits_is_valid_end_date(., timeline),
                         err_desc = ".sits_match_timelines: expected end date in not inside timeline of observations")

    return (subset_dates.lst)
}

#' @title Find indexes in a timeline that match the reference dates
#' @name .sits_match_indexes
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For correct classification, the time series of the input data set
#'              should be aligned to that of the reference data set (usually a set of patterns).
#'              This function aligns these data sets so that shape matching works correctly
#'
#' @param timeline              Timeline of input observations (vector)
#' @param ref_dates.lst         A list of breaks that will be applied to the input data set
#' @return dates_index.lst     A list of indexes that match the reference dates to the timelines
#'
.sits_match_indexes <- function (timeline, ref_dates.lst){
    dates_index.lst <- list()

    ref_dates.lst %>%
        purrr::map (function (date_pair){
            start_index <- which (timeline == date_pair[1])
            end_index   <- which (timeline == date_pair[2])

            dates_index.lst[[length(dates_index.lst) + 1]] <<- c(start_index, end_index)
        })

    return (dates_index.lst)
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
#' @title Find number of samples, given a timeline and an interval
#' @name .sits_num_samples
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function retrieves the number of samples
#'
#' @param timeline              Timeline of input observations (vector)
#' @param interval              Period to match the data to the patterns
#' @return num_samples          The number of measures during the chosen interval
#'
.sits_num_samples <- function (timeline, interval = "12 month"){

    start_date <- timeline[1]
    next_interval_date <- lubridate::as_date (lubridate::as_date(start_date) + lubridate::as.duration(interval))

    times <- timeline [(next_interval_date - timeline) >  0]
    end_date <- timeline[which.max(times)]

    return (which(timeline == end_date) - which(timeline == start_date) + 1)
}
#' @title Create a list of time indexes from the dates index
#' @name  .sits_time_index
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  dates_index.lst  A list of dates with the subsets of the input data
#' @param  timeline        The timeline of the data set
#' @param  bands           Bands used for classification
#' @return  time_index.lst  The subsets of the timeline
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
