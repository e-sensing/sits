#' @title Define the information required for classifying time series
#'
#' @name .timeline_class_info
#'
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Time series classification requires a series of steps:
#' (a) Provide labelled samples that will be used as training data.
#' (b) Provide information on how the classification will be performed,
#'     including data timeline,and start and end dates per interval.
#' (c) Clean the training data to ensure it meets the specifications
#'     of the classification info.
#' (d) Use the clean data to train a machine learning classifier.
#' (e) Classify non-labelled data sets.
#'
#' In this set of steps, this function provides support for step (b).
#' It requires the user to provide a timeline, the classification interval,
#' and the start and end dates of the reference period.
#' The result is a tibble with information that allows the user
#' to perform steps (c) to (e).
#'
#' @param  data            Description on the data being classified.
#' @param  samples         Samples used for training the classification model.
#'
#' @return A tibble with the classification information.
#'
.timeline_class_info <- function(data, samples) {
    .check_set_caller(".timeline_class_info")
    # find the timeline
    timeline <- .samples_timeline(data)
    # precondition is the timeline correct?
    .check_that(.has(timeline))
    # find the labels
    labels <- .samples_labels(samples)
    # find the bands
    bands <- .samples_bands(samples)
    # what is the reference start date?
    ref_start_date <- lubridate::as_date(samples[1L, ][["start_date"]])
    # what is the reference end date?
    ref_end_date <- lubridate::as_date(samples[1L, ][["end_date"]])
    # number of samples
    num_samples <- nrow(samples[1L, ][["time_series"]][[1L]])
    # obtain the reference dates that match the patterns in the full timeline
    ref_dates <- .timeline_match(
        timeline,
        ref_start_date,
        ref_end_date,
        num_samples
    )
    # obtain the indexes of the timeline that match the reference dates
    dates_index <- .timeline_match_indexes(timeline, ref_dates)
    # find the number of the samples
    nsamples <- dates_index[[1L]][[2L]] - dates_index[[1L]][[1L]] + 1L
    # create a class_info tibble to be used in the classification
    tibble::tibble(
        bands = list(bands),
        labels = list(labels),
        timeline = list(timeline),
        num_samples = nsamples,
        ref_dates = list(ref_dates),
        dates_index = list(dates_index)
    )
}
#' @title Test if date fits with the timeline
#'
#' @name .timeline_valid_date
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description A timeline is a list of dates where
#' observations are available. This function estimates if a date is valid
#' by  comparing it to the timeline.
#' If the date's estimate is not inside the timeline
#' and the difference between the date and the first date of
#' timeline is greater than the acquisition
#' interval of the timeline, the date is invalid.
#'
#' @param date        A date.
#' @param timeline    A vector of reference dates.
#'
#' @return Is this is valid starting date?
#'
.timeline_valid_date <- function(date, timeline) {
    # is the date inside the timeline?
    if (date %within% lubridate::interval(
        timeline[[1L]],
        timeline[[length(timeline)]]
    )) {
        return(TRUE)
    }

    # what is the difference in days between the first two days of the timeline?
    timeline_diff <- as.integer(timeline[[2L]] - timeline[[1L]])
    # if the difference in days in the timeline is smaller than the difference
    # between the reference date and the first date of the timeline, then
    # we assume the date is valid
    if (abs(as.integer(date - timeline[[1L]])) <= timeline_diff) {
        return(TRUE)
    }
    # what is the difference in days between the last two days of the timeline?
    timeline_diff <- as.integer(timeline[[length(timeline)]] -
        timeline[[length(timeline) - 1L]])
    # if the difference in days in the timeline is smaller than the difference
    # between the reference date and the last date of the timeline, then
    # we assume the date is valid
    abs(as.integer(date - timeline[length(timeline)])) <= timeline_diff
}

#' @title Find dates in the input data cube that match those of the patterns
#' @name .timeline_match
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For correct classification, the input data set
#'              should be aligned to that of the reference data set.
#'              This function aligns these data sets.
#'
#' @param timeline_data         Timeline of input to be classified.
#' @param model_start_date      Model start date
#' @param model_end_date        Model end date.
#' @param num_samples           Number of samples.
#'
#' @return A list of breaks that will be applied to the input data set.
#'
.timeline_match <- function(timeline_data,
                            model_start_date,
                            model_end_date,
                            num_samples) {
    # set caller to show in errors
    .check_set_caller(".timeline_match")

    # make sure the timeline is a valid set of dates
    timeline_data <- lubridate::as_date(timeline_data)
    # define the input start date
    input_start_date <- timeline_data[[1L]]

    # create a list  the subset dates to break the input data set
    subset_dates <- list()
    # consider two cases:
    # (1) start date of data is before start date model
    # (2) start date of data is the same or after start date of model
    if (timeline_data[[1L]] < model_start_date) {
        # what is the expected start and end dates based on the patterns?
        ref_st_mday <- as.character(lubridate::mday(model_start_date))
        ref_st_month <- as.character(lubridate::month(model_start_date))
        year_st_date <- as.character(lubridate::year(input_start_date))
        est_start_date <- lubridate::as_date(paste0(
            year_st_date, "-",
            ref_st_month, "-",
            ref_st_mday
        ))
        # find the actual starting date by searching the timeline
        idx_start_date <- which.min(abs(est_start_date - timeline_data))
    } else {
        # find the actual starting date by searching the timeline
        idx_start_date <- which.min(abs(model_start_date - timeline_data))
    }

    # take the start date of the input to be classified
    start_date <- timeline_data[idx_start_date]
    # is the start date a valid one?
    .check_that(.timeline_valid_date(start_date, timeline_data))
    # what is the expected end date of the classification?
    idx_end_date <- idx_start_date + (num_samples - 1L)
    end_date <- timeline_data[idx_end_date]
    # is the end date a valid one?
    .check_that(!(is.na(end_date)))

    # go through the timeline of the data
    # find the reference dates for the classification
    while (!is.na(end_date)) {
        # add the start and end date
        subset_dates[[length(subset_dates) + 1L]] <- c(start_date, end_date)

        # estimate the next start and end dates
        idx_start_date <- idx_end_date + 1L
        start_date <- timeline_data[idx_start_date]
        idx_end_date <- idx_start_date + num_samples - 1L
        # estimate
        end_date <- timeline_data[idx_end_date]
    }
    # is the end date a valid one?
    end_date <- subset_dates[[length(subset_dates)]][[2L]]
    .check_that(.timeline_valid_date(end_date, timeline_data))
    subset_dates
}

#' @title Find indexes in a timeline that match the reference dates
#' @name .timeline_match_indexes
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description For correct classification, the time series of the input data
#'              should be aligned to that of the reference data set
#'              (usually a set of patterns).
#'              This function aligns these data sets so that shape
#'              matching works correctly
#'
#' @param timeline      Timeline of input observations (vector).
#' @param ref_dates     List of breaks to be applied to the input data set.
#'
#' @return              A list of indexes that match the reference dates
#'                      to the timelines.
#'
.timeline_match_indexes <- function(timeline, ref_dates) {
    ref_dates |>
        purrr::map(function(date_pair) {
            start_index <- which(timeline == date_pair[[1L]])
            end_index <- which(timeline == date_pair[[2L]])

            c(start_index, end_index)
        })
}
#' @title Find the subset of a timeline that is contained
#'        in an interval defined by start_date and end_date
#' @name  .timeline_during
#' @noRd
#' @keywords internal
#'
#' @param timeline      A valid timeline
#' @param start_date    A date which encloses the start of timeline
#' @param end_date      A date which encloses the end of timeline
#'
#' @return              A timeline
#'
.timeline_during <- function(timeline,
                             start_date = NULL,
                             end_date = NULL) {
    # set caller to show in errors
    .check_set_caller(".timeline_during")
    # obtain the start and end indexes
    if (.has_not(start_date)) {
        start_date <- timeline[[1L]]
    }
    if (.has_not(end_date)) {
        end_date <- timeline[[length(timeline)]]
    }
    valid <- timeline >= lubridate::as_date(start_date) &
        timeline <= lubridate::as_date(end_date)

    # postcondition - check that there exists at least one valid date
    .check_that(any(valid))
    # return valid dates
    return(timeline[valid])
}

#' @title Find if the date information is correct
#' @name  .timeline_format
#' @keywords internal
#' @noRd
#' @description Given a information about dates, check if all dates can be
#'              interpreted by lubridate
#' @param dates  character vector representing dates
#' @return date class vector
#'
.timeline_format <- function(dates) {
    # set caller to show in errors
    .check_set_caller(".timeline_format")
    .check_that(.has(dates))

    # convert to character (strsplit does not deal with dates)
    dates <- as.character(dates)
    # check type of date interval
    converted_dates <- purrr::map_dbl(dates, function(dt) {
        if (length(strsplit(dt, "-")[[1L]]) == 1L) {
            converted_dates <- lubridate::fast_strptime(dt, c("%Y%m%d", "%Y"))
        } else if (length(strsplit(dt, "-")[[1L]]) == 2L) {
            converted_dates <- lubridate::fast_strptime(dt, "%Y-%m")
        } else if (length(strsplit(dt, "-")[[1L]]) == 3L) {
            converted_dates <- lubridate::fast_strptime(dt, "%Y-%m-%d")
        }
        converted_dates <- lubridate::as_date(converted_dates)
        # check if there are NAs values
        .check_that(!anyNA(converted_dates))
        converted_dates
    })
    # convert to a vector of dates
    converted_dates <- lubridate::as_date(converted_dates)
    # postcondition
    .check_that(length(converted_dates) == length(dates))
    converted_dates
}
#' @title Check if two timelines overlaps.
#' @name .timeline_has_overlap
#' @keywords internal
#' @noRd
#' @description This function checks if the given two timeline overlaps.
#' @param  timeline1 First timeline
#' @param  timeline2 Second timeline.
#' @return       TRUE if first and second timeline overlaps.
#'
.timeline_has_overlap <- function(timeline1, timeline2) {
    min(timeline1) <= max(timeline2) && min(timeline2) <= max(timeline1)
}
