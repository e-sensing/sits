#' @title  Comparison functions
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Functions to compare two numeric vectors using tolerance parameter.
#'
#' @param x,y  a numeric value to compare.
#' @param tolerance  a positive numeric value.
#'   Default is 0 (exact comparison).
NULL

#' @title Compare if `x` is equal to `y` considering a tolerance
#' @noRd
#' @returns A logical value
.is_eq <- function(x, y, tolerance = 0) {
    .check_set_caller(".is_eq")
    .check_that(tolerance >= 0)
    # Compute result and return
    all(abs(x - y) <= tolerance[[1]])
}
#' @title Compare if `x` is less than `y` considering a tolerance
#' @noRd
#' @returns A logical value
.is_lt <- function(x, y, tolerance = 0) {
    .check_set_caller(".is_lt")
    .check_that(tolerance >= 0)
    # Compute result and return
    all(abs(y - x) > tolerance[[1]])
}
#' @title Compare if `x` is greater than `y` considering a tolerance
#' @noRd
#' @returns A logical value
.is_gt <- function(x, y, tolerance = 0) {
    .check_set_caller(".is_gt")
    .check_that(tolerance >= 0)
    # Compute result and return
    all(abs(x - y) > tolerance[[1]])
}
