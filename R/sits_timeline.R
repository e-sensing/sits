#' @title Obtains the timeline for a coverage
#' @name sits_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function ontains the timeline for a given coverage
#'
#' @param  data.tb  A sits tibble
#'
#' @export

sits_timeline <- function (data.tb){

    ensurer::ensure_that(data.tb$timeline, !purrr::is_null(.), err_desc = "sits_timeline -
                         data tibble does not contain timeline")

    return (data.tb[1,]$timeline[[1]])
}
#' @title Find dates in the input coverage that match those of the patterns
#' @name .sits_match_timelines
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For correct classification, the time series of the input data set
#'              should be aligned to that of the reference data set (usually a set of patterns).
#'              This function aligns these data sets so that shape matching works correctly
#'
#' @param timeline              Timeline of input observations (vector)
#' @param ref_start_date        A date that defines a the start of a reference period
#' @param ref_end_date          A date that defines a the end of a reference period
#' @param interval              Period to match the data to the patterns
#' @return subset_dates.lst     A list of breaks that will be applied to the input data set
#'
.sits_match_timelines <- function (timeline, ref_start_date, ref_end_date, interval = "12 month"){

    ensurer::ensure_that(interval, lubridate::as.period(ref_end_date - ref_start_date) <= lubridate::period(.),
                         err_desc = "sits_match_timelines - pattern reference dates are greater than specified interval")

    timeline <- lubridate::as_date (timeline)
    #define the input start and end dates
    input_start_date <- timeline [1]
    input_end_date   <- timeline [length(timeline)]

    num_samples <- sits_num_samples(timeline, interval)

    if (num_samples != 23) {
        print (ref_start_date)
        print (ref_end_date)
    }

    # define the estimated start date of the input data based on the patterns
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

    end_date <- timeline[which(timeline == start_date) + (num_samples - 1)]

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

#' @title Find number of samples, given a timeline and an interval
#' @name sits_num_samples
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function retrieves the number of samples
#'
#' @param timeline              Timeline of input observations (vector)
#' @param interval              Period to match the data to the patterns
#' @return num_samples          The number of measures during the chosen interval
#'
#' @export
sits_num_samples <- function (timeline, interval = "12 month"){

    start_date <- timeline[1]
    next_interval_date <- lubridate::as_date (lubridate::as_date(start_date) + lubridate::as.duration(interval))

    times <- timeline [(next_interval_date - timeline) >  0]
    end_date <- timeline[which.max(times)]

    return (which(timeline == end_date) - which(timeline == start_date) + 1)
}
