#' @title  Comparison functions
#'
#' @keywords internal
#'
#' @name comparison_functions
#'
#' @description Functions to compare two numeric vectors using tolerance
#'  parameter.
#'
#' @param x,y       a \code{numeric} value to compare.
#'
#' @param tolerance a unique positive \code{numeric} value. Default is 0.
#'
#' @return a \code{logical} value.
NULL

#' @rdname comparison_functions
.is_eq <- function(x, y, tolerance = 0) {
    .check_num(
        x = tolerance,
        min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid tolerance parameter"
    )

    return(abs(x - y) <= tolerance)
}

#' @rdname comparison_functions
.is_lt <- function(x, y, tolerance = 0) {
    .check_num(
        x = tolerance,
        min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid tolerance parameter"
    )

    return((y - x) > tolerance)
}

#' @rdname comparison_functions
.is_gt <- function(x, y, tolerance = 0) {
    .check_num(
        x = tolerance,
        min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid tolerance parameter"
    )

    return((x - y) > tolerance)
}

#' @rdname comparison_functions
.is_int <- function(x, tolerance = 0) {
    .is_eq(x, round(x), tolerance = tolerance)
}
