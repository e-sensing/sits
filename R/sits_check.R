#' @title Check functions
#'
#' @name check_functions
#'
#' @description
#' Functions used to check parameters in a systematic way.
#'
#' @param caller        A \code{character} value with the caller name.
#' @param x             Any object that will be evaluated.
#' @param local_msg     A \code{character} with the generic error message that
#' will be shown inside parenthesis.
#' @param msg           A \code{character} with the error message that will be
#' shown as the main message to the user.
#' @param is_named      A \code{logical} indicating if the check permits unnamed
#' list.
#' @param len_min       A \code{numeric} indicating the minimum length of vector
#' or list users provides for functions. Default is \code{0}.
#' @param len_max       A \code{numeric} indicating the maximum length of vector
#' or list users provides for functions. Default is \code{2^31}.
#' @param fn_check      A \code{function} used to test each element of an
#' object.
#' @param is_integer    A \code{logical} indicating if the value should be
#' integer.
#' @param allow_na      A \code{logical} indicating if the check permits empty
#' NA values. Default is FALSE.
#' @param allow_null    A \code{logical} indicating if the check permits empty
#' NULL values. Default is FALSE.
#' @param min           A atomic \code{vector} of numeric indicating the
#' inclusive minimum value that the user can provide in function parameter.
#' Only works for numeric check. By default it is \code{-Inf}.
#' @param max           A atomic \code{vector} of numeric indicating the
#' inclusive maximum value that the user can provide in function parameter.
#' Only works for numeric check. By default it is \code{Inf}.
#' @param exclusive_min A atomic \code{vector} of numeric indicating the
#' exclusive minimum value that the user can provide in function parameter.
#' Only works for numeric check. By default it is \code{-Inf}.
#' @param exclusive_max A atomic \code{vector} of numeric indicating the
#' exclusive maximum value that the user can provide in function parameter.
#' Only works for numeric check. By default it is \code{Inf}.
#' @param allow_empty   A \code{logical} indicating if the check permits empty
#' list. Default is TRUE.
#' @param regex         A \code{character} value with regular expression to be
#' evaluated against data.
#' @param min_len       A \code{numeric} indicating the minimum length of vector
#' or list users provides for functions. Default is \code{0}.
#' @param max_len       A \code{numeric} indicating the maximum length of vector
#' or list users provides for functions. Default is \code{2^31}.
#' @param within        A \code{character} vector indicating a set of elements
#' from which \code{x} is a kind of subset. The actual behavior is pointed by
#' \code{discriminator} parameter.
#' @param discriminator A \code{character} value indicating how subset
#' verification will be done (see details).
#' @param contains      A \code{character} vector indicating a set of elements
#' to which \code{x} is a kind of superset. The actual behavior is pointed by
#' \code{discriminator} parameter.
#' @param case_sensitive  A \code{logical} indicating if the check is compared
#' with case sensitive. Default is \code{TRUE}.
#' @param can_repeat    A \code{logical} value indicating if vector \code{x}
#' can have repeated elements or not.
#' @param extensions    A \code{character} vector with all allowed file
#' extensions.
#' @param expr          A R \code{expression} to be evaluated.
#' @param show_pks_name A \code{logical} value indicating if
#'                      uninstalled packages can be shown.
#' @param tolerance     A \code{numeric} with the tolerance to be
#' accepted in range test. The default value is NULL.
#' @param ...           Additional parameters for \code{fn_check} function.
#'
#' @return
#' Unless otherwise specified, all checking functions return the same
#' argument as \code{x} if a \code{TRUE} evaluation occurs.
NULL

#' @rdname check_functions
#'
#' @details
#' Error message functions:
#' \itemize{
#' \item{\code{.check_set_caller()} should be used to set the caller name
#' that appears in error messages. Any error raised by a check function
#' will show the caller function in its error message. The caller name will
#' be determined by the last call to this function before error occurs.
#' If no call was made, the first function in the calling stack will be
#' used.
#' }
#' \item{\code{.check_identify_caller()} searches for the caller
#' name to be shown in error messages. The function searches in calling stack
#' if a call to \code{check_set_caller()} was made and returns its value. If
#' no call was found, it returns the first function in calling stack.
#' }
#' }
#'
#' @return \code{.check_set_caller()} returns \code{NULL}.
.check_set_caller <- function(caller) {
    envir <- parent.frame()
    if (length(sys.frames()) > 1) {
        envir <- sys.frame(-1)
    }
    assign(".check_caller", caller, envir = envir)

    return(invisible(NULL))
}

#' @rdname check_functions
#'
#' @return
#' \code{.check_identify_caller()} returns a \code{character} value.
.check_identify_caller <- function() {

    # check calling stack
    for (f in rev(sys.frames())) {
        if (exists(".check_caller", envir = f, inherits = FALSE)) {
            caller <- get(".check_caller", envir = f, inherits = FALSE)
            return(caller)
        }
    }

    # check parent frame
    if (exists(".check_caller", envir = parent.frame())) {
        caller <- get(".check_caller", envir = f)
        return(caller)
    }

    # no caller defined, get first function in calling stack
    caller <- sys.calls()[[1]]
    caller <- gsub(
        pattern = "^(.*)\\(.*$", replacement = "\\1",
        x = paste(caller)[[1]]
    )
    return(caller)
}

#' @rdname check_functions
#'
#' @details
#'
#' General check functions:
#' \itemize{
#' \item{
#' \code{.check_that()} function checks if the argument in
#' \code{x} is \code{logical} or not. If it is \code{logical}, it will be
#' evaluated as \code{TRUE} if all values are \code{TRUE}, \code{FALSE}
#' otherwise. If the argument is not \code{logical}, it will be evaluated
#' as \code{TRUE} if its length is greater than zero,
#' \code{FALSE} otherwise. If a \code{FALSE} evaluation occurs, an error
#' will be raised.
#' }
#' \item{
#' \code{.check_null()} throws an error if \code{x} argument
#' is \code{NULL}.
#' }
#' \item{
#' \code{.check_na()} throws an error if any element of \code{x}
#' is \code{NA}.
#' }
#' \item{
#' \code{.check_names()} throws an error if \code{x} does not have
#' names and \code{is_named} argument is \code{TRUE} (and vice-versa). This
#' function checks for empty or duplicated names if \code{is_named} is
#' \code{TRUE}.
#' }
#' \item{
#' \code{.check_length()} throws an error if length of \code{x}
#' is out of the range specified by \code{len_min} and \code{len_max}
#' (both inclusive).
#' }
#' \item{
#' \code{.check_apply()} throws an error only if \code{fn_check}
#' function throws an error when applied to each \code{x} element.
#' }
#' }
#' @keywords internal
.check_that <- function(x, ...,
                        local_msg = NULL,
                        msg = NULL) {
    value <- (is.logical(x) && all(x)) || (!is.logical(x) && length(x) > 0)

    if (!value) {

        # get caller function name
        caller <- .check_identify_caller()

        # format error message
        if (is.null(msg)) {
            msg <- sprintf("%s: %%s", caller)
        } else {
            msg <- sprintf("%s: %s (%%s)", caller, msg)
        }

        if (is.null(local_msg)) {
            expr <- deparse(substitute(expr = x, env = environment()))
            local_msg <- sprintf("%s is not TRUE", expr)
        }

        # process message
        stop(sprintf(msg, local_msg), call. = FALSE)
    }

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_null <- function(x, ...,
                        msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    .check_that(
        !is.null(x),
        local_msg = "value cannot be NULL",
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_na <- function(x, ..., allow_na = FALSE, msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    if (!allow_na) {
        .check_that(
            !any(is.na(x)),
            local_msg = "NA value is not allowed",
            msg = msg
        )
    }

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_require_packages <- function(x, ...,
                                    msg = "Please install package(s)") {
    are_packages_installed <- purrr::map_lgl(
        x, requireNamespace,
        quietly = TRUE
    )

    .check_that(
        all(are_packages_installed),
        msg = paste(msg, x[!are_packages_installed])
    )

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_names <- function(x, ...,
                         is_named = TRUE,
                         msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # cannot test zero length arguments
    if (length(x) == 0) {
        return(invisible(x))
    }

    if (is_named) {
        .check_that(
            !is.null(names(x)) && !any(is.na(names(x))),
            local_msg = "value should have names",
            msg = msg
        )

        .check_that(
            length(names(x)) == length(unique(names(x))),
            local_msg = "names should be unique",
            msg = msg
        )
    } else {
        .check_that(
            is.null(names(x)),
            local_msg = "value should be unnamed",
            msg = msg
        )
    }

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_length <- function(x, ...,
                          len_min = 0,
                          len_max = 2^31 - 1,
                          msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # pre-condition
    .check_num_type(
        len_min,
        is_integer = TRUE,
        msg = "invalid 'len_min' parameter"
    )
    .check_num_type(
        len_max,
        is_integer = TRUE,
        msg = "invalid 'len_max' parameter"
    )

    if (len_min == len_max) {
        .check_that(
            length(x) == len_min,
            local_msg = paste0("length should be ", len_min),
            msg = msg
        )
    }

    # these checks are separate because the messages are different
    .check_that(
        length(x) >= len_min,
        local_msg = paste0("length should be >= ", len_min),
        msg = msg
    )

    .check_that(
        length(x) <= len_max,
        local_msg = paste0("length should be <= ", len_max),
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_apply <- function(x, fn_check, ...) {
    if (!is.function(fn_check)) {
        stop(".check_apply: fn_check should be a function.", call. = TRUE)
    }

    # check all elements
    lapply(x, fn_check, ...)

    return(invisible(x))
}

#' @rdname check_functions
#'
#' @details
#'
#' Check type functions:
#' \itemize{
#' \item{
#' \code{.check_lgl_type()} throws an error if \code{x} type is not
#' \code{logical}.
#' }
#' \item{
#' \code{.check_num_type()} throws an error if \code{x}
#' type is not \code{numeric}. Also, an error will be throw if \code{x} values
#' are not \code{integer} and \code{is_integer} parameter is \code{TRUE}.
#' }
#' \item{
#' \code{.check_int_type()} throws an error if \code{x}
#' type is not \code{numeric} with integer values.
#' }
#' \item{
#' \code{.check_chr_type()} throws an error if \code{x}
#' type is not \code{character}.
#' }
#' \item{
#' \code{.check_lst_type()} throws an error if \code{x}
#' type is not \code{list}.
#' }
#' }
#' @keywords internal
.check_lgl_type <- function(x, ...,
                            msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    .check_that(
        is.logical(x),
        local_msg = "value is not logical",
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_num_type <- function(x, ...,
                            is_integer = FALSE,
                            msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    .check_that(
        is.numeric(x),
        local_msg = "value is not a number",
        msg = msg
    )

    # test integer
    if (is_integer) {

        # if length is zero there is nothing to check
        if (length(x) == 0) {
            return(invisible(x))
        }

        .check_that(
            is.numeric(x) && all(x == suppressWarnings(as.integer(x))),
            local_msg = "value is not integer",
            msg = msg
        )
    }

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_chr_type <- function(x, ...,
                            msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    .check_that(
        is.character(x),
        local_msg = "value is not character type",
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_lst_type <- function(x, ...,
                            msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    .check_that(
        is.list(x),
        local_msg = "value is not a list",
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
#'
#' @details
#' Combined check functions. These function combine some checks mentioned
#' above in one place. In general, these functions can check for \code{NA}
#' (if \code{allow_na=FALSE}), for value length (if either \code{len_min}
#' and \code{len_max} are defined - for \code{list} the parameters are
#' \code{min_len} and \code{max_len}, respectively), for \code{NULL} value
#' (if \code{allow_null=FALSE}), and for names (if \code{is_named} is
#' \code{TRUE} or \code{FALSE}). Depending on specific type, the functions
#' also check for:
#'
#' \itemize{
#' \item{
#' \code{.check_lgl()} checks for \code{logical} values.
#' }
#' \item{
#' \code{.check_num()} checks for \code{numeric} values and its range (if
#' either \code{min}, \code{max}, \code{exclusive_min}, or \code{exclusive_max}
#' parameters are defined). It also checks \code{integer} values
#' (if \code{is_integer=TRUE}).
#' }
#' \item{
#' \code{.check_chr()} checks for \code{character} type and empty strings (if
#' \code{allow_empty=FALSE}). It also checks strings through regular
#' expression (if \code{regex} parameter is defined).
#' }
#' \item{
#' \code{.check_lst()} checks for \code{list} type. By default, it checks if
#' the list is named. Additionally, a function can be passed to
#' \code{fn_check} parameter to check its elements. This enables to pass
#' other checking functions like \code{.check_num()} to verify the type of
#' its elements. In this case, extra parameters can be passed by \code{...}.
#' }
#' }
#' @keywords internal
.check_lgl <- function(x, ...,
                       allow_na = FALSE,
                       len_min = 0,
                       len_max = 2^31 - 1,
                       allow_null = FALSE,
                       is_named = FALSE,
                       msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_lgl_type(x, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    if (!allow_na) {
        .check_na(x, msg = msg)
    }

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_num <- function(x, ...,
                       allow_na = FALSE,
                       min = -Inf,
                       max = Inf,
                       exclusive_min = -Inf,
                       exclusive_max = Inf,
                       len_min = 0,
                       len_max = 2^31 - 1,
                       allow_null = FALSE,
                       is_integer = FALSE,
                       is_named = FALSE,
                       tolerance = 0,
                       msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_num_type(x, is_integer = is_integer, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    .check_na(x, allow_na = allow_na, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    # check range
    .check_num_min_max(
        x = x,
        min = min,
        max = max,
        exclusive_min = exclusive_min,
        exclusive_max = exclusive_max,
        tolerance = tolerance,
        msg = msg
    )

    return(invisible(x))
}

.check_num_min_max <- function(x, ...,
                               min = -Inf,
                               max = Inf,
                               exclusive_min = -Inf,
                               exclusive_max = Inf,
                               tolerance = 0,
                               msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # pre-condition
    .check_num_type(min, msg = "invalid 'min' parameter")
    .check_num_type(max, msg = "invalid 'max' parameter")
    .check_num_type(exclusive_min, msg = "invalid 'exclusive_min' parameter")
    .check_num_type(exclusive_max, msg = "invalid 'exclusive_max' parameter")
    .check_num_type(x = tolerance, msg = "invalid 'tolerance' parameter")

    # remove NAs before check to test tolerance
    result <- x
    x <- x[!is.na(x)]

    # adjust min and max to tolerance
    if (!is.null(tolerance)) {
        min <- min - tolerance
        max <- max + tolerance
        exclusive_min <- exclusive_min - tolerance
        exclusive_max <- exclusive_max + tolerance
    }

    # min and max checks
    if (min == max) {
        .check_that(
            all(x == min),
            local_msg = paste0("value should be ", min),
            msg = msg
        )
    }
    .check_that(
        all(x >= min),
        local_msg = paste0("value should be >= ", min),
        msg = msg
    )
    .check_that(
        all(x <= max),
        local_msg = paste0("value should be <= ", max),
        msg = msg
    )

    # exclusive_min and exclusive_max checks
    .check_that(
        all(x > exclusive_min),
        local_msg = paste0("value should be > ", exclusive_min),
        msg = msg
    )
    .check_that(
        all(x < exclusive_max),
        local_msg = paste0("value should be < ", exclusive_max),
        msg = msg
    )

    return(invisible(result))
}

#' @rdname check_functions
#' @keywords internal
.check_chr <- function(x, ...,
                       allow_na = FALSE,
                       allow_empty = TRUE,
                       len_min = 0,
                       len_max = 2^31 - 1,
                       allow_null = FALSE,
                       is_named = FALSE,
                       regex = NULL,
                       msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # check for null and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_chr_type(x, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    if (!allow_na) {
        .check_na(x, msg = msg)
    }

    # check empty
    if (!allow_empty) {
        .check_that(
            all(nchar(x[!is.na(x)]) > 0),
            local_msg = "empty value is not allowed",
            msg = msg
        )
    }


    # check names
    .check_names(x, is_named = is_named, msg = msg)

    # check regular expression pattern
    if (!is.null(regex)) {
        .check_that(
            all(grepl(pattern = regex, x = x)),
            local_msg = "value did not match pattern",
            msg = msg
        )
    }

    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
.check_lst <- function(x, ...,
                       min_len = 0,
                       max_len = 2^31 - 1,
                       allow_null = FALSE,
                       is_named = TRUE,
                       fn_check = NULL,
                       msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # check for null and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_lst_type(x, msg = msg)

    # check length
    .check_length(x, len_min = min_len, len_max = max_len, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    # check using function
    if (!is.null(fn_check)) {
        .check_apply(x, fn_check = fn_check, msg = msg, ...)
    }

    return(invisible(x))
}

#' @rdname check_functions
#'
#' @details
#' Subset check functions. Two functions are provided to check for
#' subset elements in \code{character} vectors. These functions are the
#' symmetrical equivalent to each other, but the error messages are different.
#' For the \code{.check_chr_within()}, the error message focus on the
#' \code{within} values. For the \code{.check_chr_contains()}, the error
#' message focus on the \code{contains} values. The verification is done
#' accordingly to the \code{discriminator} parameter, that can be:
#' \code{one_of}, \code{any_of}, \code{all_of}, \code{none_of}, or
#' \code{exactly}.
#'
#' \itemize{
#' \item{
#' \code{.check_chr_within()} throws an error if provided \code{within} vector
#' does not correspond to the \code{discriminator} with respect to \code{x}
#' parameter (e.g. "one of x within...", "all of x within...).
#' \code{one_of}: only one value (can it repeat?) of \code{x} appears
#' in \code{within} vector. \code{any_of}: at least one value (can it
#' repeat?) of \code{x} appears in \code{within} vector. \code{all_of}
#' (default): all values (can it repeat?) of \code{x} appears in \code{within}
#' vector. \code{none_of}: no value of \code{x} is in \code{within} vector.
#' \code{exactly}: value of \code{x} (can it repeat?) is equal to
#' \code{within} vector.
#' }
#' \item{
#' \code{.check_chr_contains()} throws an error if provided \code{x}
#' vector does not correspond to the \code{discriminator} with respect to
#' \code{contains} parameter (e.g. "x contains one of...",
#' "x contains all of..."). \code{one_of}: only one value (can it repeat?) of
#' \code{contains} appears in \code{x} vector. \code{any_of}: at least one
#' value (can it repeat?) of \code{contains} appears in \code{x} vector.
#' \code{all_of} (default): all values (can it repeat?) of \code{contains}
#' appears in \code{x} vector. \code{none_of}: no value of \code{contains} is
#' in \code{x} vector. \code{exactly}: value of \code{contains} is exactly
#' (can it repeat?) equal to \code{x}.
#' }
#' }
#' @keywords internal
.check_chr_within <- function(x,
                              within, ...,
                              case_sensitive = TRUE,
                              discriminator = "all_of",
                              can_repeat = TRUE,
                              msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # pre-condition
    .check_chr(within,
        len_min = 1,
        msg = "invalid 'within' parameter"
    )

    # allowed discriminators and its print values
    discriminators <- c(
        one_of = "be only one of",
        any_of = "be at least one of",
        all_of = "be",
        none_of = "be none of",
        exactly = "be exactly"
    )

    if (length(discriminator) != 1 ||
        !discriminator %in% names(discriminators)) {
        stop(paste(
            ".check_chr_within: discriminator should be one of",
            "'one_of', 'any_of', 'all_of', 'none_of', or 'exactly'."
        ),
        call. = TRUE
        )
    }

    # check type
    .check_chr_type(x, msg = msg)

    # check for repeated values
    if (!can_repeat) {
        .check_that(
            length(x) == length(unique(x)),
            local_msg = "values can not repeat",
            msg = msg
        )
    }

    result <- x

    # simplify
    x <- unique(x)
    within <- unique(within)

    # transform inputs to verify without case sensitive
    original_within <- within
    if (!case_sensitive) {
        x <- tolower(x)
        within <- tolower(within)
    }

    # prepare local message
    local_msg <- sprintf(
        "values should %s: %s",
        discriminators[[discriminator]],
        paste0("'", original_within, "'",
            collapse = ", "
        )
    )

    # check discriminator
    if (discriminator == "one_of") {
        .check_that(
            sum(x %in% within) == 1,
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "any_of") {
        .check_that(
            any(x %in% within),
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "all_of") {
        .check_that(
            all(x %in% within),
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "none_of") {
        .check_that(
            !any(x %in% within),
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "exactly") {
        .check_that(
            all(x %in% within) && all(within %in% x),
            local_msg = local_msg,
            msg = msg
        )
    }

    return(invisible(result))
}

#' @rdname check_functions
#' @keywords internal
.check_chr_contains <- function(x,
                                contains, ...,
                                case_sensitive = TRUE,
                                discriminator = "all_of",
                                can_repeat = TRUE,
                                msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # pre-condition
    .check_chr(contains,
        len_min = 1,
        msg = "invalid 'contains' parameter"
    )

    # allowed discriminators and its print values
    discriminators <- c(
        one_of = "contain only one of",
        any_of = "contain at least one of",
        all_of = "contain",
        none_of = "not contain any of",
        exactly = "be exactly"
    )

    if (length(discriminator) != 1 ||
        !discriminator %in% names(discriminators)) {
        stop(paste(
            ".check_chr_contains: discriminator should be one of",
            "'one_of', 'any_of', or 'all_of'."
        ),
        call. = TRUE
        )
    }

    # check type
    .check_chr_type(x, msg = msg)

    # check for repeated values
    if (!can_repeat) {
        .check_that(
            length(contains) == length(unique(contains)),
            local_msg = "values can not repeat",
            msg = msg
        )
    }

    result <- x

    # simplify
    x <- unique(x)
    contains <- unique(contains)

    # transform inputs to verify without case sensitive
    original_contains <- contains
    if (!case_sensitive) {
        x <- tolower(x)
        contains <- tolower(contains)
    }

    # prepare local message
    local_msg <- sprintf(
        "values should %s: %s",
        discriminators[[discriminator]],
        paste0("'", original_contains, "'", collapse = ", ")
    )

    # check discriminator
    if (discriminator == "one_of") {
        .check_that(
            sum(contains %in% x) == 1,
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "any_of") {
        .check_that(
            any(contains %in% x),
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "all_of") {
        .check_that(
            all(contains %in% x),
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "none_of") {
        .check_that(
            !any(contains %in% x),
            local_msg = local_msg,
            msg = msg
        )
    } else if (discriminator == "exactly") {
        .check_that(
            all(contains %in% x) && all(x %in% contains),
            local_msg = local_msg,
            msg = msg
        )
    }

    return(invisible(result))
}

#' @rdname check_functions
#'
#' @details
#' Special checking function:
#'
#' \itemize{
#' \item{
#' \code{.check_file()} throws an error if provided value is not a valid and
#' existing file path.
#' }
#' }
#' @keywords internal
.check_file <- function(x, ...,
                        extensions = NULL,
                        msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' parameter")
    }

    # file extension
    ext_file <- function(x) {
        gsub(
            pattern = "[^?]+\\.([^?/.]+).*$",
            replacement = "\\1",
            basename(x)
        )
    }

    # check parameter
    .check_chr(x,
        allow_empty = FALSE, len_min = 1,
        allow_null = FALSE, msg = msg
    )

    # check extension
    if (!is.null(extensions)) {
        .check_chr_within(ext_file(x),
            within = extensions,
            case_sensitive = FALSE,
            msg = "invalid file extension"
        )
    }

    existing_files <- file.exists(x)
    existing_dirs <- dir.exists(x)
    .check_that(
        all(existing_files | existing_dirs),
        local_msg = paste(
            "file does not exist:",
            paste0("'", x[!existing_files], "'",
                collapse = ", "
            )
        ),
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
#'
#' @details
#' Special checking function:
#'
#' \itemize{
#' \item{
#' \code{.check_env_var()} throws an error if provided environment variable is
#' not existing.
#' }
#' }
#' @keywords internal
.check_env_var <- function(x, ...,
                           msg = NULL) {

    # make default message
    if (purrr::is_null(msg)) {
        # get x as expression
        x_expr <- deparse(substitute(x, environment()))
        msg <- paste0("invalid '", x_expr, "' environment variable")
    }

    .check_null(x, msg = msg)

    .check_chr_type(x, msg = msg)

    if (length(x) > 0) {
        .check_apply(
            x,
            fn_check = function(x) {
                .check_that(
                    x = nzchar(Sys.getenv(x)),
                    msg = paste(
                        sprintf("%s: ", x),
                        msg
                    )
                )
            }
        )
    } else {
        .check_that(x = nzchar(Sys.getenv(x)), msg = msg)
    }

    return(invisible(x))
}

#' @rdname check_functions
#'
#' @details
#' Contextual check and error conversion functions:
#'
#' \itemize{
#' \item{
#' \code{.check_warn()} converts an error raised by an R expression in
#' \code{expr} parameter into a warning message.
#' }
#' \item{
#' \code{.check_error()} captures any error raised by an R expression in
#' \code{expr} parameter, and shows a personalized message.
#' }
#' }
#' @keywords internal
.check_warn <- function(expr) {
    result <- tryCatch(
        {
            expr
        },
        error = function(e) {
            warning(e$message, call. = FALSE)
        }
    )

    return(invisible(result))
}

#' @rdname check_functions
#' @keywords internal
.check_error <- function(expr, ...,
                         msg = NULL) {
    result <- tryCatch(
        {
            expr
        },
        error = function(e) {
            .check_that(FALSE, local_msg = e$message, msg = msg)
        }
    )

    return(invisible(result))
}
#' @rdname check_functions
#' @keywords internal
.check_documentation <- function(progress) {
    # if working on sits documentation mode, no progress bar
    if (Sys.getenv("SITS_DOCUMENTATION_MODE") == "true") {
        progress <- FALSE
    }

    return(progress)
}
