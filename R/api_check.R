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
#' @rdname check_functions
#' @noRd
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
#' @name .check_identify_caller
#' @noRd
#' @return the name of the function that is being tested.
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
#' @noRd
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
#' @noRd
#' @return The same input value if no error occurs
#'
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
#' @noRd
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
#' @noRd
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
#' @noRd
.check_names <- function(x, ...,
                         is_named = TRUE,
                         is_unique = TRUE,
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
        if (is_unique) {
            .check_that(
                length(names(x)) == length(unique(names(x))),
                local_msg = "names should be unique",
                msg = msg
            )
        }

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
#' @noRd
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
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
.check_chr <- function(x, ...,
                       allow_na = FALSE,
                       allow_empty = TRUE,
                       len_min = 0,
                       len_max = 2^31 - 1,
                       allow_null = FALSE,
                       is_named = FALSE,
                       has_unique_names = TRUE,
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
    .check_names(x,
                 is_named = is_named,
                 is_unique = has_unique_names,
                 msg = msg)

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
#' @noRd
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
#' @noRd
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
#' @noRd
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
            local_msg = "values cannot repeat",
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
        "value should %s: %s",
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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

#' @title Check is numerical parameter is valid using reasonable defaults
#' @name .check_num_parameter
#' @param  x   parameter to be checked
#' @param  min minimum value
#' @param  max maximum value
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#' @param  allow_na   allow NA?
#' @param  exclusive_min  is there an exclusive minimum?
#' @param  tolerance tolerance for equality comparison
#'
#' @return          No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_num_parameter <- function(param, min = -Inf, max = Inf,
                                 len_min = 1, len_max = 1,
                                 allow_na = FALSE,
                                 exclusive_min = -Inf, tolerance = 0) {
    .check_num(
        x = param,
        allow_na = allow_na,
        min = min,
        max = max,
        len_min = len_min,
        len_max = len_max,
        exclusive_min = exclusive_min,
        tolerance = tolerance
    )
}

.check_lgl_parameter <- function(param, len_min = 1, len_max = 1,
                                 allow_na = FALSE, allow_null = FALSE,
                                 is_named = FALSE) {
    .check_lgl(
        x = param,
        len_min = len_min,
        len_max = len_max,
        allow_na = allow_na,
        allow_null = allow_null,
        is_named = is_named
    )
}


#' @title Check is dates parameter are valid
#' @name .check_dates_parameter
#' @param  param   parameter to be checked
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#' @param  allow_null allow NULL?
#'
#' @return No return value, called for side effects.
#' @keywords internal
.check_dates_parameter <- function(param, len_min = 1, len_max = 2^31 - 1,
                                   allow_null = TRUE) {
    # Standard regexp of RFC 3339
    pattern_rfc <- "^\\d{4}-\\d{2}-\\d{2}$"
    .check_that(
        x = all(grepl(pattern_rfc, param, perl = TRUE)),
        msg = "invalid date format",
        local_msg = paste(
            "'start_date' and 'end_date' should follow",
            "year-month-day format: YYYY-MM-DD"
        )
    )
}

#' @title Check is integer parameter is valid using reasonable defaults
#' @name .check_int_parameter
#' @param  x   parameter to be checked
#' @param  min minimum value
#' @param  max maximum value
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#'
#' @return          No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_int_parameter <- function(param, min = 1, max = 2^31 - 1,
                                 len_min = 1, len_max = 1) {
    .check_num(
        x = param,
        min = min,
        max = max,
        len_min = len_min,
        len_max = len_max,
        is_integer = TRUE
    )
}
#' @title Check is integer parameter is valid using reasonable defaults
#' @name .check_chr_parameter
#' @param  x   parameter to be checked
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#' @return          No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_chr_parameter <- function(param, len_min = 1, len_max = 1) {
    .check_chr(
        param,
        len_min = len_min,
        len_max = len_max
    )
}
#' @title Check is window size is valid using reasonable defaults
#' @name .check_window_size
#' @param  window_size   size of the local window
#' @param  min minimum value
#' @param  max maximum value
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_window_size <- function(window_size, min = 1, max = 2^32 - 1) {
    .check_int_parameter(window_size, min = min, max = max)
    .check_that(
        x = window_size %% 2 != 0,
        msg = "window_size must be an odd number"
    )
}

#' @title Check is multicores parameter is valid using reasonable defaults
#' @name .check_multicores
#' @param  multicores   number of cores to be used
#'
#' @return          No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_multicores <- function(multicores) {
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )
}
#' @title Check is memsize parameter is valid using reasonable defaults
#' @name .check_memsize
#' @param  memsize  size of memory in GB
#' @return          No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_memsize <- function(memsize) {
    # precondition - memory
    .check_num(
        x = memsize,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'memsize' parameter"
    )
}
#' @title Check is output_dir parameter is valid using reasonable defaults
#' @name .check_output_dir
#' @param  output_dir  name of output directory
#' @return          No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_output_dir <- function(output_dir) {
    .check_file(
        x = .file_normalize(output_dir),
        msg = "invalid output dir"
    )
}
.check_crs <- function(crs) {
    crs <- suppressWarnings(.try(sf::st_crs(crs), .default = NA))
    .check_that(
        x = !is.na(crs),
        local_msg = "the 'crs' must be a valid character or numeric value.",
        msg = "invalid 'crs' parameter."
    )
}
#' @title Check is version parameter is valid using reasonable defaults
#' @name .check_version
#' @keywords internal
#' @noRd
#' @param  version  version parameter
#' @return          No return value, called for side effects.
.check_version <- function(version) {
    .check_chr(
        x = version,
        len_min = 1,
        msg = "invalid version"
    )
}
#' @title Check is version parameter is valid using reasonable defaults
#' @name .check_version
#' @keywords internal
#' @noRd
#' @param  progress progress parameter
#' @return          No return value, called for side effects.
.check_progress <- function(progress) {
    .check_lgl(
        x = progress,
        len_min = 1,
        len_max = 1,
        msg = "invalid progress"
    )
}
#' @title Check is expression parameter is valid using reasonable defaults
#' @name .check_expression
#' @param  list_expr expression parameter
#' @return          No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_expression <- function(list_expr) {
    .check_lst(list_expr,
               min_len = 1, max_len = 1,
               msg = "invalid expression value"
    )
}

#' @title Does the values has same number of pixels than input values?
#' @name .check_predicted
#' @param values a matrix of processed values
#' @param input_pixels number of pixels in input matrix
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_processed_values <- function(values, input_pixels) {
    .check_that(
        x = .has(nrow(values)) && nrow(values) == input_pixels,
        msg = paste("size of processed matrix is different",
                    "from number of input pixels")
    )
}
#' @title Does the input data contain a set of predicted values?
#' @name .check_predicted
#' @param data a sits tibble
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_predicted <- function(data) {
    .check_chr_contains(
        x = names(data),
        contains = "predicted",
        msg = "input data without predicted values"
    )
    .check_chr_within(
        x = .conf("ts_predicted_cols"),
        within = names(data$predicted[[1]]),
        msg = "tibble has not been classified"
    )
}
#' @title Does the input data contain a raster cube?
#' @name .check_is_raster_cube
#' @param data a sits cube to be tested
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_is_raster_cube <- function(data) {
    .check_that(
        x = inherits(data, "raster_cube"),
        local_msg = "data should be a raster cube",
        msg = "invalid cube parameter"
    )
}
#' @title Check if cube is a probs cube
#' @name .check_is_probs_cube
#' @param cube a sits cube to be tested
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube         input data cube
.check_is_probs_cube <- function(cube) {
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "cube is not probability cube"
    )
}
#' @title Check if cube is a variance cube
#' @name .check_is_variance_cube
#' @param cube a sits cube to be tested
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  cube         input data cube
.check_is_variance_cube <- function(cube) {
    .check_that(
        x = inherits(cube, "variance_cube"),
        msg = "cube is not variance cube"
    )
}
#' @title Check if cube is a uncert cube
#' @name .check_is_uncert_cube
#' @param cube a sits cube to be tested
#' @return  No return value, called for side effects
#' @keywords internal
#' @noRd
.check_is_uncert_cube <- function(cube) {
    .check_that(
        x = inherits(cube, "uncertainty_cube"),
        msg = "cube is not an uncertainty cube"
    )
}
#' @title Check if cube is a classified image
#' @name .check_cube_is_class_cube
#' @param cube a sits cube to be tested
#' @return  No return value, called for side effects
#' @keywords internal
#' @noRd
.check_cube_is_class_cube <- function(cube) {
    .check_that(
        x = inherits(cube, "class_cube"),
        msg = "cube is not classified image"
    )
}
#' @title Does the input data contain a sits tibble?
#' @name .check_is_sits_tibble
#' @param data a sits tibble
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_is_sits_tibble <- function(data) {
    .check_that(
        x = inherits(data, "sits"),
        local_msg = "data should be a sits tibble",
        msg = "invalid samples parameter"
    )
}
#' @title Does the input data contain a sits accuracy object?
#' @name .check_is_sits_accuracy
#' @param data a sits accuracy object
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_is_sits_accuracy <- function(data) {
    .check_that(
        x = inherits(data, what = "sits_accuracy"),
        local_msg = "please run sits_accuracy() first",
        msg = "input does not contain accuracy information"
    )
}
#' @title Does the input data contain a sits model?
#' @name .check_is_sits_model
#' @param model a sits model
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_is_sits_model <- function(model) {
    .check_that(
        x = inherits(model, "function"),
        local_msg = "please, run sits_train() first",
        msg = "invalid sits model"
    )
    .check_that(
        x = inherits(model, "sits_model"),
        local_msg = "please, run sits_train() first",
        msg = "invalid sits model"
    )
    .check_that(
        x = any(c("model", "torch_model",
                  # Old models
                  "result_rfor", "result_svm", "model_xgb") %in%
                    ls(environment(model))),
        local_msg = "please, run sits_train() first",
        msg = "invalid sits model"
    )
    .check_that(
        x = "samples" %in% ls(environment(model)),
        local_msg = "please, run sits_train() first",
        msg = "invalid sits model"
    )
    # Check model samples
    samples <- .ml_samples(model)
    .check_samples(samples)
}
#' @title Does the data contain the cols of sample data and is not empty?
#' @name .check_samples
#' @param data a sits tibble
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_samples <- function(data) {
    .check_that(
        x = all(.conf("df_sample_columns") %in% colnames(data)),
        msg = "invalid samples file"
    )
    .check_that(
        x = nrow(data) > 0,
        msg = "samples does not contain values"
    )
}

#' @title Does input data has time series?
#' @name .check_samples_ts
#' @param data a sits tibble
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_samples_ts <- function(data) {
    .check_samples(data)
    .check_chr_contains(
        x = colnames(data),
        contains = "time_series",
        msg = "invalid samples data"
    )
    # Get unnested time series
    ts <- .sits_ts(data)
    # check there is an Index column
    .check_that(x = "Index" %in% colnames(ts))
    # check if all samples have the same bands
    n_bands <- unique(lengths(data[["time_series"]]))
    .check_that(length(n_bands) == 1,
                local_msg = "samples with different bands",
                msg = "inconsistent samples")
}

#' @title Can the input data be used for training?
#' @name .check_samples_train
#' @param data a sits tibble
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_samples_train <- function(data) {
    .check_samples(data)
    # check that there is no NA in labels
    labels <- .sits_labels(data)
    .check_that(
        x = !("NoClass" %in% labels) && !("" %in% labels) &&
            !any(is.na(labels)),
        msg = "invalid labels in samples data"
    )
    .check_samples_ts(data)
    # Get unnested time series
    ts <- .ts(data)
    # check there are no NA in distances
    .check_that(x = !(anyNA(ts)), msg = "samples contain NA values")
    # check samples timeline
    n_times <- unique(unlist(tapply(
        .ts_sample_id(ts), .ts_sample_id(ts), length, simplify = FALSE
    ), use.names = FALSE))
    .check_that(length(n_times) == 1,
                local_msg = "samples contain timelines with different lengths",
                msg = "inconsistent samples")

}
#' @title Is the samples_validation object valid?
#' @name .check_samples_validation
#' @param samples_validation a sits tibble with validation samples
#' @param labels labels of the data cube to be validated
#' @param timeline timeline of the data cube to be validated
#' @param bands  bands of the data cube to be validated
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_samples_validation <- function(samples_validation,
                                      labels,
                                      timeline,
                                      bands) {
    # check if the validation samples are ok
    .check_samples(samples_validation)
    # check if the labels matches with train data
    .check_that(
        all(sits_labels(samples_validation) %in% labels) &&
            all(labels %in% sits_labels(samples_validation))
    )
    # check if the timeline matches with train data
    .check_that(
        length(sits_timeline(samples_validation)) == length(timeline)
    )
    # check if the bands matches with train data
    .check_that(
        all(sits_bands(samples_validation) %in% bands) &&
            all(bands %in% sits_bands(samples_validation))
    )
}
#' @title Do the samples contain a cluster column?
#' @name .check_samples_cluster
#' @param data a sits tibble with cluster col
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
#  Are the samples valid?
.check_samples_cluster <- function(data) {
    .check_samples(data)
    # is the input data the result of a cluster function?
    .check_chr_contains(
        names(data),
        contains = "cluster",
        msg = "missing cluster column"
    )
}
#' @title Are the predictors valid?
#' @name .check_predictors
#' @param pred a tibble with predictors values
#' @param samples samples from where the predictors have been calculated
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_predictors <- function(pred, samples) {
    cols <- .pred_cols # From predictors API
    .check_that(
        x = cols %in% colnames(pred),
        msg = "invalid predictors data"
    )
    .check_that(
        x = nrow(pred) > 0,
        msg = "invalid predictors data"
    )
    n_bands <- length(sits_bands(samples))
    n_times <- length(sits_timeline(samples))
    .check_that(
        x = ncol(pred) == 2 + n_bands * n_times,
        msg = "invalid predictors data"
    )
}

#' @title Does the data contain the cols of sample data and is not empty?
#' @name .check_smoothness
#' @param smoothness a vector or numeric value
#' @param nlabels    a numeric value
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_smoothness <- function(smoothness, nlabels) {
    .check_that(
        length(smoothness) == 1 || length(smoothness) == nlabels,
        msg = paste(
            "smoothness must be either one value or",
            "a vector of length", nlabels
        )
    )
}
#' @title Check classification parameters
#' @name .check_cube_model
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Verify that required parameters are correct.
#'
#' @param  cube            Tibble with information about a data cube.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @return No value called for side effects
.check_cube_model <- function(cube, ml_model) {

    # ensure metadata tibble exists
    .check_that(
        x = nrow(cube) > 0,
        msg = "invalid metadata for the cube"
    )

    # ensure the machine learning model has been built
    .check_null(
        x = ml_model,
        msg = "trained ML model not available"
    )
    # precondition - test is model is valid
    .check_is_sits_model(ml_model)
}

#' @title Check if cube has only one tile
#' @name .check_has_one_tile
#' @param  cube         input data cube
#' @return  No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_has_one_tile <- function(cube) {
    .check_that(
        x = nrow(cube) == 1,
        msg = "accepts only one tile at a time"
    )
}

#' @title Check that cube is regular
#' @name .check_is_regular
#' @keywords internal
#' @noRd
#' @param cube  datacube
#' @return  TRUE/FALSE
.check_is_regular <- function(cube) {
    .check_that(
        x = .cube_is_regular(cube),
        msg = "cube is not regular, please run sits_regularize() first"
    )
}

#' @title Check if bboxes of all tiles of the cube are the same
#' @name .check_has_unique_bbox
#' @keywords internal
#' @noRd
#' @param  cube         input data cube
#' @return TRUE/FALSE
.check_has_unique_bbox <- function(cube) {
    tolerance <- .conf(
        "sources", .cube_source(cube),
        "collections", .cube_collection(cube),
        "ext_tolerance"
    )

    # check if the resolutions are unique
    equal_bbox <- slider::slide_lgl(cube, function(tile) {
        file_info <- .fi(tile)

        test <-
            (.is_eq(max(file_info[["xmax"]]),
                    min(file_info[["xmax"]]),
                    tolerance = tolerance
            ) &&
                .is_eq(max(file_info[["xmin"]]),
                       min(file_info[["xmin"]]),
                       tolerance = tolerance
                ) &&
                .is_eq(max(file_info[["ymin"]]),
                       min(file_info[["ymin"]]),
                       tolerance = tolerance
                ) &&
                .is_eq(max(file_info[["ymax"]]),
                       min(file_info[["ymax"]]),
                       tolerance = tolerance
                ))

        return(test)
    })

    if (!all(equal_bbox)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
#' @title Check if sizes of all tiles of the cube are the same
#' @name .check_has_unique_tile_size
#' @keywords internal
#' @noRd
#' @param  cube         input data cube
#' @return TRUE/FALSE
.check_has_unique_tile_size <- function(cube) {
    # check if the sizes of all tiles are the same
    test_cube_size <- slider::slide_lgl(cube, function(tile) {
        if (length(unique(.tile_nrows(tile))) > 1 ||
            length(unique(.tile_ncols(tile))) > 1) {
            return(FALSE)
        }
        return(TRUE)
    })

    if (!all(test_cube_size)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' @title Check if file is a CSV
#' @name .check_file_csv
#' @param file filename
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
#'
.check_file_csv <- function(file) {
    .check_file(
    x = file,
    extensions = "csv"
    )
}
#' @title Check if CSV data is correct
#' @name .check_csv
#' @param csv  data frame extracted from CSV file
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
#
.check_csv <- function(csv) {
    # check if required col names are available
    .check_chr_contains(
        x = colnames(csv),
        contains = .conf("df_sample_columns"),
        discriminator = "all_of",
        msg = "invalid csv file"
    )
}
#' @title Check if data contains predicted and reference values
#' @name .check_pred_ref_match
#' @param reference  vector with reference labels
#' @param predicted  vector with predicted labels
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_pred_ref_match <- function(reference, predicted) {
    .check_that(
        x = length(reference) == length(predicted),
        msg = "predicted and reference vector do not match"
    )
}
#' @title Do the samples and tile match?
#' @name .check_samples_tile_match
#' @param samples  samples organised as a tibble
#' @param tile  one tile of a data cube
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_samples_tile_match <- function(samples, tile) {
    # do they have the same timelines?
    samples_timeline_length <- length(sits_timeline(samples))
    tiles_timeline_length <- length(sits_timeline(tile))
    .check_that(
        samples_timeline_length == tiles_timeline_length,
        msg = "number of instances of samples and cube differ"
    )
    # do they have the same bands?
    tile_bands <- sits_bands(tile)
    bands <- sits_bands(samples)
    .check_chr_within(
        x = bands,
        within = tile_bands,
        msg = "some bands in samples are not in cube"
    )
}
#' @title Does the input data contains valid reference labels?
#' @name  .check_labels
#' @param  data vector with labels
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_labels <- function(data) {
    .check_that(
        x = !("NoClass" %in% (data)),
        msg = "input data without labels"
    )
}
#' @title Check if bands are part of a data cube
#' @name .check_cube_bands
#' @param cube          Data cube
#' @param bands         Bands to be check
#' @param add_cloud     Include the cloud band?
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
#'
.check_cube_bands <- function(cube, bands, add_cloud = TRUE) {

    # all bands are upper case
    .check_chr_within(
        bands,
        within = .cube_bands(cube = cube, add_cloud = add_cloud),
        case_sensitive = FALSE,
        msg = "invalid 'bands' parameter"
    )
}
#' @title Check if  cubes have the same bbox
#' @name .check_cubes_same_bbox
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return No return value, called for side effects.
.check_cubes_same_bbox <- function(cube1, cube2) {
    tolerance <- .conf(
        "sources", .cube_source(cube1),
        "collections", .cube_collection(cube1),
        "ext_tolerance"
    )
    ok <- slider::slide2_lgl(cube1, cube2,
                           function(tile_first, tile_cube) {
            return(.bbox_equal(
                .tile_bbox(tile_first),
                .tile_bbox(tile_cube),
                tolerance = tolerance)
            )
    })
    .check_that(
        x = all(ok),
        msg = "cubes do not have the same bounding box"
    )
}

#' @title Check if cubes have the same size
#' @name .check_cubes_same_size
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return No return value, called for side effects.
.check_cubes_same_size <- function(cube1, cube2) {
    .check_that(
        x = all(.cube_ncols(cube1) == .cube_ncols(cube2)) &&
            all(.cube_nrows(cube1) == .cube_nrows(cube2)),
        msg = "cubes do not have the same size"
    )
}

#' @title Check if cubes have the same tiles
#' @name .check_cubes_same_tiles
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return No return value, called for side effects.
.check_cubes_same_tiles <- function(cube1, cube2) {
    .check_that(
        x = nrow(cube1) == nrow(cube2),
        msg = "cubes do not have the same tiles"
    )
}

#' @title Check if cubes have the same labels
#' @name .check_cubes_same_labels
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return No return value, called for side effects.
.check_cubes_same_labels <- function(cube1, cube2) {
    .check_that(
        x = all(sits_labels(cube1) ==  sits_labels(cube2)),
        msg = "cubes do not have the same labels"
    )
}
#' @title Check if cubes have the same timeline
#' @name .check_cubes_same_timeline
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return No return value, called for side effects.
.check_cubes_same_timeline <- function(cube1, cube2) {
    .check_that(
        x = all(sits_timeline(cube1) == sits_timeline(cube2)),
        msg = "cubes do not have the same timeline"
    )
}
#' @title Check if two cubes have the same organization
#' @name .check_cubes_match
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return No return value, called for side effects.
.check_cubes_match <- function(cube1, cube2) {
    # check same size
    .check_cubes_same_tiles(cube1, cube2)
    .check_cubes_same_size(cube1, cube2)
    .check_cubes_same_bbox(cube1, cube2)
    .check_cubes_same_timeline(cube1, cube2)
    .check_cubes_same_labels(cube1, cube2)

}
#' @title Check if cubes have the same organization
#' @name .check_cube_list_match
#' @keywords internal
#' @noRd
#' @param  cubes         list of input data cubes
#' @return No return value, called for side effects.
.check_cube_list_match <- function(cubes) {
    .check_lst_type(cubes,
                    msg = "cubes are not in a list")
    # check same size
    first <- cubes[[1]]
    purrr::map(cubes, function(cube) {
        .check_cubes_match(first, cube)
    })
}

#' @title Check if list of probs cubes have the same organization
#' @name .check_probs_cube_lst
#' @keywords internal
#' @noRd
#' @param  cubes         list of input data cubes
#' @return No return value, called for side effects.
.check_probs_cube_lst <- function(cubes) {
    .check_that(length(cubes) >= 2,
                local_msg = "length should be at least two",
                msg = "invalid `cubes` parameter"
    )
    .check_lst_type(cubes, msg = "cubes are not in a list")
    # is every cube a probs cube
    purrr::map(cubes, .check_is_probs_cube)
    # check same size
    first <- cubes[[1]]
    purrr::map(cubes, function(cube) {
        .check_cubes_match(first, cube)
    })
}

#' @title Check if list of uncertainty cubes have the same organization
#' @name .check_uncert_cube_lst
#' @keywords internal
#' @noRd
#' @param  uncert_cubes     list of input data cubes
#' @return No return value, called for side effects.
.check_uncert_cube_lst <- function(uncert_cubes) {
    .check_that(length(uncert_cubes) >= 2,
                local_msg = "length should be at least two",
                msg = "invalid `uncert_cubes` parameter"
    )
    .check_lst_type(uncert_cubes, msg = "cubes are not in a list")
    # is every cube a probs cube
    purrr::map(uncert_cubes, .check_is_uncert_cube)
    # check same size
    first <- uncert_cubes[[1]]
    purrr::map(uncert_cubes, function(cube) {
        .check_cubes_match(first, cube)
    })
}

#' @title Check if errox matrix and area are cosrrect
#' @name .check_error_matrix_area
#' @param  error_matrix  Error matrix for classification
#' @param  area  Area of each class
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
#'
.check_error_matrix_area <- function(error_matrix, area) {
    .check_that(
        x = all(dim(error_matrix) > 1),
        msg = "invalid dimensions in error matrix"
    )
    .check_that(
        x = length(unique(dim(error_matrix))) == 1,
        msg = "The error matrix is not square"
    )
    .check_that(
        x = all(colnames(error_matrix) == rownames(error_matrix)),
        msg = "Labels mismatch in error matrix."
    )
    .check_that(
        x = unique(dim(error_matrix)) == length(area),
        msg = "Mismatch between error matrix and area vector."
    )
    .check_that(
        x = all(names(area) %in% colnames(error_matrix)),
        msg = "Label mismatch between error matrix and area vector."
    )
}
#' @title Checks if the required packages are installed
#' @name .check_require_packages
#' @param x   the name of the required package
#' @return the name of the package
#' @keywords internal
#' @noRd
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
#' @title Checks if the character parameter is empty
#' @name .check_empty_char
#' @param x a character vector
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_empty_char <- function(x, msg, ...) {
    .check_that(all(nzchar(x)), msg = msg, ...)
}
.check_endmembers_parameter <- function(x) {
    .check_that(
        x = inherits(x, c("data.frame", "character")),
        msg = "invalid 'endmembers' parameter"
    )
}
#' @title Checks if the endmembers data is in a valid parameter
#' @name .check_endmembers_tbl
#' @param em   Reference spectra endmembers.
#' @param cube A sits cube
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_endmembers_tbl <- function(em) {
    # Pre-condition
    .check_na(
        x = em,
        msg = "Invalid 'endmembers' parameter"
    )
    # Pre-condition
    .check_chr_contains(
        x = colnames(em),
        contains = c("TYPE", "CLASS"),
        discriminator = "any_of",
        msg = "Invalid 'endmembers' parameter"
    )
    # Pre-condition
    .check_chr(
        x = .endmembers_fracs(em), allow_empty = FALSE, len_min = 1,
        msg = "The reference endmembers cannot be empty"
    )
    # Pre-condition
    .check_that(
        nrow(em) < ncol(em),
        msg = paste(
            "Endmembers must be less or equal than the",
            "number of spectral bands."
        )
    )
}
.check_endmembers_bands <- function(em, bands) {
    .check_chr_within(
        x = .band_eo(.endmembers_bands(em)),
        within = bands,
        msg = "invalid 'endmembers' columns"
    )
}
.check_res <- function(x) {
    .check_num(
        x = x,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        allow_null = TRUE,
        msg = "invalid 'res' parameter."
    )
}
#' @title Checks if working in documentation mode
#' @name .check_documentation
#' @param progress  flag set to show progress bar
#' @return TRUE/FALSE
#' @keywords internal
#' @noRd
.check_documentation <- function(progress) {
    # if working on sits documentation mode, no progress bar
    if (Sys.getenv("SITS_DOCUMENTATION_MODE") == "true" ||
        Sys.getenv("SITS_DOCUMENTATION_MODE") == "TRUE") {
        progress <- FALSE
    }

    return(progress)
}
#' @title Checks if messages should be displayed
#' @name .check_messages
#' @return TRUE/FALSE
#' @keywords internal
#' @noRd
.check_messages <- function() {
    # if working on sits documentation mode, no progress bar
    if (Sys.getenv("SITS_DOCUMENTATION_MODE") == "true" ||
        Sys.getenv("SITS_DOCUMENTATION_MODE") == "TRUE") {
        return(FALSE)
    }
    else
        return(TRUE)
}

#' @title Checks if warnings should be displayed
#' @name .check_warnings
#' @return TRUE/FALSE
#' @keywords internal
#' @noRd
.check_warnings <- function() {
    # if working on sits documentation mode, no progress bar
    if (Sys.getenv("SITS_DOCUMENTATION_MODE") == "true" ||
        Sys.getenv("SITS_DOCUMENTATION_MODE") == "TRUE") {
        return(FALSE)
    }
    else
        return(TRUE)
}
.check_stac_items <- function(items) {
    .check_that(
        rstac::items_length(items) > 0,
        local_msg = paste("please, check 'roi', 'start_date', 'end_date', and",
                          "'tile' parameters"),
        msg = "cube search criteria returned no items"
    )
}
