#' Period API
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
#' .period_check("P16D") # valid
#' .period_check("P1M10D") # error: invalid period format
#' .period_val("P16D") # 16
#' .period_val("P2M") # 2
#' .period_val("P1Y") # 1
#' .period_unit("P16D") # day
#' .period_unit("P2M") # month
#' .period_unit("P1Y") # year
#' }
#'
#' @family data types
#' @keywords internal
#' @name period_api
#' @noRd
NULL

#' @describeIn period_api Check if a character string is a valid \code{period}.
#' @returns \code{.period_check()}: nothing.
#' @noRd
.period_check <- function(period) {
    if (!grepl("^P[0-9]+[DMY]$", period)) {
        stop("invalid period format")
    }
}

#' @describeIn period_api Return the value part of a \code{period}.
#' @returns \code{.period_val()}: numeric value of a period.
#' @noRd
.period_val <- function(period) {
    .period_check(period)
    .as_dbl(gsub("^P([0-9]+)[DMY]$", "\\1", period))
}

#' @describeIn period_api Return the unit of a \code{period}.
#'   Can be one of \code{'day'}, \code{'month'}, or \code{'year'}.
#' @returns \code{.period_unit()}: description of unit of a period.
#' @noRd
.period_unit <- function(period) {
    .period_check(period)
    unit <- c(D = "day", M = "month", Y = "year")
    unit[[gsub("^P[0-9]+([DMY])$", "\\1", period)]]
}
