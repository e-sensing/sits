#' Period API
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{lipecaso@@gmail.com}
#'
#' According to ISO-8601 a duration is the amount of intervening time
#' in a time interval. Here, we use a simplified representation of a duration
#' that we call \code{period}.
#'
#' It is represented by the the regular expression
#' \code{^P[0-9]+[DMY]$}. \code{P} is the period designator placed at the
#' start of the string. \code{[0-9]+} is the integer value and is followed
#' by a \code{period} unit: \code{Y} is the year designator, \code{M} is the
#' month designator, and \code{D} is the day designator.
#'
#' @param period A \code{character}.
#'
#' @examples
#' if (sits_run_examples()) {
#'     .check_period("P16D") # valid
#'     .check_period("P1M10D") # error: invalid period format
#'     .period_val("P16D") # 16
#'     .period_val("P2M") # 2
#'     .period_val("P1Y") # 1
#'     .period_unit("P16D") # day
#'     .period_unit("P2M") # month
#'     .period_unit("P1Y") # year
#' }
#'
#' @family data types
#' @keywords internal
#' @name period_api
#' @noRd
NULL

#' @describeIn period_api Return the value part of a \code{period}.
#' @returns \code{.period_val()}: numeric value of a period.
#' @noRd
.period_val <- function(period) {
    .check_period(period)
    .as_dbl(gsub("^P([0-9]+)[DMY]$", "\\1", period))
}

#' @describeIn period_api Return the unit of a \code{period}.
#'   Can be one of \code{'day'}, \code{'month'}, or \code{'year'}.
#' @returns \code{.period_unit()}: description of unit of a period.
#' @noRd
.period_unit <- function(period) {
    .check_period(period)
    unit <- c(D = "day", M = "month", Y = "year")
    unit[[gsub("^P[0-9]+([DMY])$", "\\1", period)]]
}

#' @describeIn period_api Create period windows.
#' @returns \code{.period_windows()}: Period windows.
#' @noRd
.period_windows <- function(period, step, start_date, end_date) {
    # Transform `period` and `step` strings in duration
    period_duration <- lubridate::as.duration(period)
    step_duration <- lubridate::as.duration(step)
    # Transform `start_date` and `end_date` to date
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    # Final period windows
    period_windows <- list()
    # Define first time period (used as part of the step)
    current_start <- start_date
    # Create period windows
    while(current_start < end_date) {
        # Create the window: current start date + step
        current_end <- current_start + period_duration
        # Avoid window definition beyond the end date
        if (current_end > end_date) {
            current_end <- end_date
        }
        # Save period window
        period_windows <-
            c(period_windows, list(c(
                start = as.Date(current_start),
                end = as.Date(current_end)
            )))
        # Move to the next window date: current start date + step
        current_start <- current_start + step_duration
    }
    period_windows
}
