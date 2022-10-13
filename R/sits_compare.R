#  Comparison functions
#  Functions to compare two numeric vectors using tolerance parameter
#
.is_eq <- function(x, y, tolerance = 0) {
    .check_num(
        x = tolerance,
        min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid tolerance parameter"
    )

    return(all(abs(x - y) <= tolerance))
}
#
.is_lt <- function(x, y, tolerance = 0) {
    .check_num(
        x = tolerance,
        min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid tolerance parameter"
    )

    return(all((y - x) > tolerance))
}

#
.is_gt <- function(x, y, tolerance = 0) {
    .check_num(
        x = tolerance,
        min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid tolerance parameter"
    )

    return(all((x - y) > tolerance))
}

#
.is_int <- function(x, tolerance = 0) {
    .is_eq(x, round(x), tolerance = tolerance)
}
