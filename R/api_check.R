#' @title Check functions
#'
#' @name check_functions
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @param len_min       A \code{numeric} indicating the minimum length of vector
#' or list users provides for functions. Default is \code{0}.
#' @param len_max     A \code{numeric} indicating the maximum length of vector
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
#' @param file_exists   A \code{logical} value indicating if
#'                      the file should exist
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
#' @return Called for side effects
.check_set_caller <- function(caller) {
    envir <- parent.frame()
    if (length(sys.frames()) > 1) {
        envir <- sys.frame(-1)
    }
    assign(".check_caller", caller, envir = envir)
    return(invisible(caller))
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
#' @return  Called for side effects
.check_that <- function(x, ...,
                        local_msg = NULL,
                        msg = NULL) {
    value <- (is.logical(x) && all(x)) || (!is.logical(x) && length(x) > 0)
    if (!value) {
        # get caller function name
        caller <- .check_identify_caller()
        # format error message
        if (is.null(msg))
            msg <- .conf("messages", caller)
        # include local message if available
        if (is.null(local_msg))
            msg <- paste0(caller, ": ", msg)
        else
            msg <- paste0(caller, ": ", local_msg)
        # process message
        stop(msg, call. = FALSE)
    }
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_null <- function(x, ...,
                        local_msg = NULL,
                        msg = NULL) {
    # check that value is not NULL
    .check_that(!is.null(x),
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_na <- function(x, ..., allow_na = FALSE, local_msg = NULL, msg = NULL) {
    if (!allow_na) {
        .check_that(
            !anyNA(x),
            local_msg = local_msg,
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
                         local_msg = NULL,
                         msg = NULL) {
    # cannot test zero length arguments
    if (length(x) == 0) {
        return(invisible(x))
    }
    if (is_named) {
        .check_that(
            .has(names(x)) && !anyNA(names(x)),
            local_msg = local_msg,
            msg = .conf("messages", ".check_names_is_named")
        )
        if (is_unique) {
            .check_that(
                length(names(x)) == length(unique(names(x))),
                local_msg = local_msg,
                msg = .conf("messages", ".check_names_unique" )
            )
        }
    } else {
        .check_that(
            is.null(names(x)),
            local_msg = local_msg,
            msg = .conf("messages", ".check_names_is_unnamed")
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
                          local_msg = NULL,
                          msg = NULL) {
    .check_that(
        length(x) >= len_min && length(x) <= len_max,
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_apply <- function(x, fn_check, ..., local_msg = NULL, msg = NULL) {
    .check_that(
        is.function(fn_check),
        local_msg = local_msg,
        msg = msg
    )
    # check all elements
    lapply(x, fn_check, ...)
    return(invisible(x))
}
#' @rdname check_functions
#'
#' @description
#' Internal check functions. These functions are used internally
#' to check if the parameters passed by other check functions are valid.
#'
#' \itemize{
#' \item{
#' \code{.check_lgl_type()} checks for \code{logical} values.
#' }
#' \item{
#' \code{.check_num_type()} checks for \code{numeric} values and its range (if
#' either \code{min}, \code{max}, \code{exclusive_min}, or \code{exclusive_max}
#' parameters are defined). It also checks \code{integer} values
#' (if \code{is_integer=TRUE}).
#' }
#' \item{
#' \code{.check_chr_type()} checks for \code{character} and empty strings (if
#' \code{allow_empty=FALSE}). It also checks strings through regular
#' expression (if \code{regex} parameter is defined).
#' }
#' \item{
#' \code{.check_lst_type()} checks for \code{list} type. By default, checks if
#' the list is named. Additionally, a function can be passed to
#' \code{fn_check} parameter to check its elements. This enables to pass
#' other checking functions like \code{.check_num()} to verify the type of
#' its elements. In this case, extra parameters can be passed by \code{...}.
#' }
#' }
#' @keywords internal
#' @noRd
.check_lgl_type <- function(x, ...,
                            local_msg = NULL,
                            msg = NULL) {
    .check_that(
        is.logical(x),
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_num_type <- function(x, ...,
                            is_integer = FALSE,
                            local_msg = NULL,
                            msg = NULL) {
    .check_that(
        is.numeric(x),
        local_msg = local_msg,
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
            local_msg = local_msg,
            msg = msg
        )
    }
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_chr_type <- function(x, ...,
                            local_msg = NULL,
                            msg = NULL) {
    .check_that(
        is.character(x),
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}

#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_lst_type <- function(x, ...,
                            local_msg = NULL,
                            msg = NULL) {
    .check_that(
        is.list(x),
        local_msg = local_msg,
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
#' \code{len_min} and \code{len_max}, respectively), for \code{NULL} value
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
                       local_msg = NULL,
                       msg = NULL) {

    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }
    # check NULL
    .check_null(x, local_msg = local_msg, msg = msg)
    # check type
    .check_lgl_type(x, local_msg = local_msg, msg = msg)
    # check length
    .check_length(x, len_min = len_min, len_max = len_max,
                  local_msg = local_msg, msg = msg)
    # check NA
    if (!allow_na) {
        .check_na(x, local_msg = local_msg, msg = msg)
    }
    # check names
    .check_names(x, is_named = is_named, local_msg = local_msg, msg = msg)
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
                       is_odd = FALSE,
                       tolerance = 0,
                       local_msg = NULL,
                       msg = NULL) {
    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }
    # check NULL
    .check_null(x, local_msg = local_msg, msg = msg)
    # check type
    .check_num_type(x, is_integer = is_integer,
                    local_msg = local_msg, msg = msg)
    # check length
    .check_length(x, len_min = len_min, len_max = len_max,
                  local_msg = local_msg, msg = msg)
    # check NA
    .check_na(x, allow_na = allow_na,
              local_msg = local_msg, msg = msg)
    # check names
    .check_names(x, is_named = is_named,
                 local_msg, msg = msg)
    # check range
    .check_num_min_max(
        x = x,
        min = min,
        max = max,
        exclusive_min = exclusive_min,
        exclusive_max = exclusive_max,
        tolerance = tolerance,
        local_msg = local_msg,
        msg = msg
    )
    if (is_odd)
        .check_that(x %% 2 != 0, msg = msg)

    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_num_min_max <- function(x, ...,
                               min = -Inf,
                               max = Inf,
                               exclusive_min = -Inf,
                               exclusive_max = Inf,
                               tolerance = 0,
                               local_msg = NULL,
                               msg = NULL) {

    # pre-condition
    .check_num_type(min, local_msg = local_msg, msg = msg)
    .check_num_type(max, local_msg = local_msg, msg = msg)
    .check_num_type(exclusive_min, local_msg = local_msg, msg = msg)
    .check_num_type(exclusive_max, local_msg = local_msg, msg = msg)
    .check_num_type(x = tolerance, local_msg = local_msg, msg = msg)

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
            local_msg = local_msg,
            msg = paste0("value should be ", min)
        )
    }
    .check_that(
        all(x >= min),
        local_msg = local_msg,
        msg = paste0("value should be >= ", min)
    )
    .check_that(
        all(x <= max),
        local_msg = local_msg,
        msg = paste0("value should be <= ", max)
    )
    # exclusive_min and exclusive_max checks
    .check_that(
        all(x > exclusive_min),
        local_msg = local_msg,
        msg = paste0("value should be > ", exclusive_min)
    )
    .check_that(
        all(x < exclusive_max),
        local_msg = local_msg,
        msg = paste0("value should be < ", exclusive_max)
    )
    return(invisible(result))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_chr <- function(x, ...,
                       allow_na = FALSE,
                       allow_empty = TRUE,
                       allow_duplicate = TRUE,
                       len_min = 0,
                       len_max = 2^31 - 1,
                       allow_null = FALSE,
                       is_named = FALSE,
                       has_unique_names = TRUE,
                       regex = NULL,
                       local_msg = NULL,
                       msg = NULL) {
    # check for null and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }
    # check NULL
    .check_null(x, local_msg = local_msg, msg = msg)
    # check type
    .check_chr_type(x, local_msg = local_msg, msg = msg)
    # check length
    .check_length(x, len_min = len_min, len_max = len_max,
                  local_msg = local_msg, msg = msg)
    # check NA
    if (!allow_na) {
        .check_na(x, local_msg = local_msg, msg = msg)
    }
    # check empty
    if (!allow_empty) {
        .check_that(
            all(nchar(x[!is.na(x)]) > 0),
            local_msg = local_msg,
            msg = msg
        )
    }
    # check duplicate
    if (!allow_duplicate) {
        .check_that(
            anyDuplicated(x) == 0,
            local_msg = local_msg,
            msg = msg
        )
    }
    # check names
    .check_names(x,
                 is_named = is_named,
                 is_unique = has_unique_names,
                 local_msg = local_msg,
                 msg = msg
    )
    # check regular expression pattern
    if (!is.null(regex)) {
        .check_that(
            all(grepl(pattern = regex, x = x)),
            local_msg = local_msg,
            msg = msg
        )
    }
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_lst <- function(x, ...,
                       len_min = 0,
                       len_max = 2^31 - 1,
                       allow_null = FALSE,
                       is_named = TRUE,
                       fn_check = NULL,
                       local_msg = NULL,
                       msg = NULL) {

    # check for null and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }
    # check NULL
    .check_null(x, local_msg = local_msg, msg = msg)
    # check type
    .check_lst_type(x, local_msg = local_msg, msg = msg)
    # check length
    .check_length(x, len_min = len_min, len_max = len_max,
                  local_msg = local_msg, msg = msg)
    # check names
    .check_names(x, is_named = is_named, local_msg = local_msg, msg = msg)
    # check using function
    if (!is.null(fn_check)) {
        .check_apply(x, fn_check = fn_check,
                     local_msg = local_msg, msg = msg, ...)
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
    # check parameter name
    param_x <- deparse(substitute(x, environment()))
    # make default message
    local_msg_x <- .check_var_message(param_x)

    # check within name
    param_w <- deparse(substitute(within, environment()))
    # make default message
    local_msg_w <- .check_var_message(param_w)

    # pre-condition
    .check_chr(within,
               len_min = 1,
               local_msg = local_msg_w,
               msg = msg
    )
    # check parameters
    .check_discriminator(discriminator)
    # check type
    .check_chr_type(x, local_msg = local_msg_x, msg = msg)
    # check for repeated values
    if (!can_repeat) {
        .check_that(
            length(x) == length(unique(x)),
            local_msg = local_msg_x,
            msg = msg
        )
    }
    result <- x
    # simplify
    x <- unique(x)
    within <- unique(within)
    # transform inputs to verify without case sensitive
    if (!case_sensitive) {
        x <- tolower(x)
        within <- tolower(within)
    }
    # check discriminator
    if (discriminator == "one_of") {
        .check_that(
            sum(x %in% within) == 1,
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "any_of") {
        .check_that(
            any(x %in% within),
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "all_of") {
        .check_that(
            all(x %in% within),
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "none_of") {
        .check_that(
            !any(x %in% within),
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "exactly") {
        .check_that(
            all(x %in% within) && all(within %in% x),
            local_msg = local_msg_x,
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

    # check parameter name
    var_x <- deparse(substitute(x, environment()))
    # make default message for param
    local_msg_x <- .check_var_message(var_x)
    # check type
    .check_chr_type(x, local_msg = local_msg_x)
    # check contains name
    var_cont <- deparse(substitute(contains, environment()))
    # make default message for param
    local_msg_cont <- .check_var_message(var_cont)
    # pre-condition
    .check_that(length(contains) >= 1, local_msg = local_msg_cont)
    # check discriminators
    .check_discriminator(discriminator)
    # check for repeated values
    if (!can_repeat) {
        .check_that(
            length(contains) == length(unique(contains)),
            local_msg = local_msg_cont,
            msg = msg
        )
    }
    result <- x
    # simplify
    x <- unique(x)
    contains <- unique(contains)
    # transform inputs to lower case
    if (!case_sensitive) {
        x <- tolower(x)
        contains <- tolower(contains)
    }
    # check discriminator
    if (discriminator == "one_of") {
        .check_that(
            sum(contains %in% x) == 1,
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "any_of") {
        .check_that(
            any(contains %in% x),
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "all_of") {
        .check_that(
            all(contains %in% x),
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "none_of") {
        .check_that(
            !any(contains %in% x),
            local_msg = local_msg_x,
            msg = msg
        )
    } else if (discriminator == "exactly") {
        .check_that(
            all(contains %in% x) && all(x %in% contains),
            local_msg = local_msg_x,
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
                        file_exists = TRUE,
                        local_msg = NULL,
                        msg = NULL) {
    # check parameter name
    var <- deparse(substitute(x, environment()))
    # make default message for param
    local_msg <- .check_var_message(var)

    # file extension
    ext_file <- function(x) {
        gsub(
            pattern = "[^?]+\\.([^?/.]+).*$",
            replacement = "\\1",
            basename(x)
        )
    }
    if (is.null(msg))
        # check parameter
        .check_chr(x,
                   allow_na = FALSE,
                   allow_empty = FALSE,
                   len_min = 1,
                   allow_null = FALSE,
                   local_msg = local_msg,
                   msg = msg
        )
    # check extension
    if (!is.null(extensions)) {
        extension <- ext_file(x)
        .check_that(extension %in% extensions,
                    local_msg = local_msg)
    }
    if (file_exists) {
        existing_files <- file.exists(x)
        existing_dirs <- dir.exists(x)
        .check_that(
            all(existing_files | existing_dirs),
            local_msg = local_msg,
            msg = paste(.conf("messages", ".check_file_missing"),
                paste0("'", x[!existing_files], "'",
                       collapse = ", "
                )
            )
        )
    } else {
        .check_that(
            x = suppressWarnings(file.create(x)),
            local_msg = local_msg,
            msg = .conf("messages", ".check_file_writable")
        )
    }
    return(invisible(x))
}
#' @title Check environment variable
#' @name .check_env_var
#' @description
#' \code{.check_env_var()} throws an error if provided environment variable is
#' not existing.
#' @keywords internal
#' @noRd
#' @return Called for side effects.
.check_env_var <- function(x, ...,
                           local_msg = NULL,
                           msg = NULL) {
    # check parameter name
    var <- deparse(substitute(x, environment()))
    # make default message for param
    local_msg <- .check_var_message(var)
    # check env var exists
    .check_that(nchar(Sys.getenv(x)) > 0, local_msg = local_msg)
    return(invisible(x))
}
#' @title Check warning
#' @name .check_warn
#' @description
#' Converts an error raised by an R expression in
#' \code{expr} parameter into a warning message
#' @param  expr   R expression
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_warn <- function(expr) {
    result <- tryCatch(
        {
            expr
        },
        error = function(e) {
            warning(e[["message"]], call. = FALSE)
        }
    )
    return(invisible(result))
}
#' @title Check error
#' @name .check_error
#' @description
#' captures any error raised by an R expression in
#' \code{expr} parameter, and shows a personalized message.
#'
#' @param  expr   R expression
#' @param  ...    Other parameters
#' @param  msg Error message
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_error <- function(expr, ...,
                         msg = NULL) {
    result <- tryCatch(
        {
            expr
        },
        error = function(e) {
            .check_that(FALSE, local_msg = e[["message"]], msg = msg)
        }
    )
    return(invisible(result))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_na_null_parameter <- function(x, ...) {
    # check for NA
    .check_na_parameter(x)
    # check for NULL
    .check_null_parameter(x)
    return(invisible(x))
}
#' @rdname check_functions
#' @name .check_null_parameter
#' @param  x   parameter to be checked
#' @keywords internal
#' @noRd
.check_null_parameter <- function(x, ...,
                        msg = NULL) {
    # check parameter name
    param <- deparse(substitute(x, environment()))
    local_msg <- paste("NULL value not allowed for", param)
    # check that value is not NULL
    .check_that(!is.null(x),
                local_msg = local_msg,
                msg = msg
    )
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_na_parameter <- function(x, ..., allow_na = FALSE, msg = NULL) {
    # check parameter name
    param <- deparse(substitute(x, environment()))
    local_msg <- paste("NA value not allowed for", param)
    if (!allow_na) {
        .check_that(
            !anyNA(x),
            local_msg = local_msg,
            msg = msg
        )
    }
    return(invisible(x))
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
#' @param  msg Error message
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_num_parameter <- function(x,
                                 min = -Inf,
                                 max = Inf,
                                 len_min = 1,
                                 len_max = 1,
                                 allow_na = FALSE,
                                 allow_null = FALSE,
                                 is_named = FALSE,
                                 exclusive_min = -Inf,
                                 tolerance = 0,
                                 msg = NULL) {
    # check parameter name
    param <- deparse(substitute(x, environment()))
    local_msg <- .check_param_message(param)
    .check_num(
        x,
        allow_na = allow_na,
        allow_null = allow_null,
        min = min,
        max = max,
        len_min = len_min,
        len_max = len_max,
        exclusive_min = exclusive_min,
        tolerance = tolerance,
        is_named = is_named,
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}
#' @title Check is logical parameter is valid
#' @name .check_lgl_parameter
#' @param  x   parameter to be checked
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#' @param  allow_na     allow NA?
#' @param  allow_null   allow NULL?
#' @param  is_named     is the parameter named?
#' @param  msg Error message
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_lgl_parameter <- function(x,
                                 len_min = 1, len_max = 1,
                                 allow_na = FALSE, allow_null = FALSE,
                                 is_named = FALSE,
                                 msg = NULL) {
    # check parameter name
    param <- deparse(substitute(x, environment()))
    # make default message
    local_msg <- .check_param_message(param)
    .check_lgl(
        x,
        len_min = len_min,
        len_max = len_max,
        allow_na = allow_na,
        allow_null = allow_null,
        is_named = is_named,
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}
#' @title Check is date is valid
#' @name .check_date_parameter
#' @param  x   parameter to be checked
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#' @param  allow_null allow NULL?
#' @param  msg Error message
#' @return Called for side effects.
#' @keywords internal
.check_date_parameter <- function(x,
                                  len_min = 1,
                                  len_max = 1,
                                  allow_null = FALSE,
                                  msg = NULL) {
    .check_set_caller(".check_date_parameter")
    # Standard regexp of RFC 3339
    pattern_rfc <- "^\\d{4}-\\d{2}-\\d{2}$"
    # check dates are valid
    .check_that(all(grepl(pattern_rfc, x, perl = TRUE)))
    return(invisible(x))
}
#' @title Check is integer parameter is valid using reasonable defaults
#' @name .check_int_parameter
#' @param  x   parameter to be checked
#' @param  min minimum value
#' @param  max maximum value
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#' @param  is_odd  is the value an odd one?
#' @param  allow_null  Allow NULL value?
#' @param  msg Error message
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_int_parameter <- function(x, min = -2^31 + 1, max = 2^31 - 1,
                                 len_min = 1, len_max = 2^31 - 1,
                                 is_odd = FALSE, is_named = FALSE,
                                 allow_null = FALSE, msg = NULL) {
    # check parameter name
    param <- deparse(substitute(x, environment()))
    # make default message
    local_msg <- .check_param_message(param)
    .check_num(
        x,
        allow_na = FALSE,
        allow_null = allow_null,
        min = min,
        max = max,
        len_min = len_min,
        len_max = len_max,
        is_integer = TRUE,
        is_named = is_named,
        is_odd = is_odd,
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}
#' @title Check is integer parameter is valid using reasonable defaults
#' @name .check_chr_parameter
#' @param  x   parameter to be checked
#' @param  allow_na allow NA parameter?
#' @param  allow_null allow null parameter?
#' @param  allow_empty allow empty parameter?
#' @param  allow_duplicate allow duplicate parameter?
#' @param  len_min minimum length of vector
#' @param  len_max maximum length of vector
#' @param  is_named is this a named parameter?
#' @param  regex  regular expression to be tested
#' @param  msg message error
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_chr_parameter <- function(x,
                                 len_min = 1,
                                 len_max =  2^31 - 1,
                                 is_named = FALSE,
                                 allow_na = FALSE,
                                 allow_empty = FALSE,
                                 allow_null = FALSE,
                                 allow_duplicate = TRUE,
                                 regex = NULL,
                                 msg = NULL) {
    # check parameter name
    param <- deparse(substitute(x, environment()))
    # make default message
    local_msg <- .check_param_message(param)
    .check_chr(
        x,
        len_min = len_min,
        len_max = len_max,
        is_named = is_named,
        allow_null = allow_null,
        allow_na = allow_na,
        allow_empty = allow_empty,
        regex = regex,
        local_msg = local_msg,
        msg = msg
    )
    return(invisible(x))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_lst_parameter <- function(x, ...,
                       len_min = 1,
                       len_max = 2^31 - 1,
                       allow_null = FALSE,
                       is_named = TRUE,
                       fn_check = NULL,
                       msg = NULL) {
    # check parameter name
    param <- deparse(substitute(x, environment()))
    # make default message
    local_msg <- .check_param_message(param)

    # check for null and exit if it is allowed
    if (allow_null && is.null(x)) {
        return(invisible(x))
    }
    # check NULL
    .check_null(x, local_msg = local_msg, msg = msg)
    # check type
    .check_lst_type(x, msg = msg)
    # check length
    .check_length(x, len_min = len_min, len_max = len_max,
                  local_msg = local_msg, msg = msg)
    # check names
    .check_names(x, is_named = is_named,
                 local_msg = local_msg, msg = msg)
    # check using function
    if (!is.null(fn_check)) {
        .check_apply(x, fn_check = fn_check,
                     local_msg = local_msg,   msg = msg, ...)
    }
    return(invisible(x))
}
#' @title Check is period parameter is valid
#' @name .check_period
#' @describeIn Check if a character string is a valid \code{period}.
#' @returns called for side effects
#' @noRd
.check_period <- function(period) {
    .check_set_caller(".check_period")
    .check_that(grepl("^P[0-9]+[DMY]$", period))
}
#' @title Check is dates are valid
#' @name .check_dates_timeline
#' @describeIn Check if dates are part of the timeline of an object
#' @param dates    Vector of dates
#' @param tile     Tile
#' @returns called for side effects
#' @noRd
.check_dates_timeline <- function(dates, tile) {
    .check_set_caller(".check_dates_timeline")
    # is this a valid date?
    dates <- as.Date(dates)
    .check_that(all(dates %in% .tile_timeline(tile)))
    return(invisible(dates))
}
#' @title Check is crs parameter is valid
#' @name .check_crs
#' @param  crs Coordinate reference system index.
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_crs <- function(crs) {
    .check_set_caller(".check_crs")
    crs <- suppressWarnings(.try(sf::st_crs(crs), .default = NA))
    .check_that(!is.na(crs))
    return(invisible(crs))
}
#' @title Check is output_dir parameter is valid using reasonable defaults
#' @name .check_output_dir
#' @param  output_dir  name of output directory
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_output_dir <- function(output_dir) {
    .check_set_caller(".check_output_dir")
    .check_chr(
        x = output_dir,
        allow_na = FALSE,
        allow_null = FALSE,
        allow_empty = FALSE,
        len_min = 1,
        len_max = 1
    )
    output_dir <- .file_path_expand(output_dir)
    .check_file(output_dir)
    return(invisible(output_dir))
}
#' @title Check is version parameter is valid using reasonable defaults
#' @name .check_version
#' @keywords internal
#' @noRd
#' @param  version  character vector
#' @return version adjusted to remove underscores
.check_version <- function(version) {
    .check_set_caller(".check_version")
    .check_chr(
        x = version,
        allow_na = FALSE,
        allow_null = FALSE,
        allow_empty = FALSE,
        len_min = 1,
        len_max = 1
    )
    # avoids use of underscores
    version <- tolower(gsub("_", "-", version))
    return(version)
}
#' @title Check is version parameter is valid using reasonable defaults
#' @name .check_progress
#' @keywords internal
#' @noRd
#' @param  progress TRUE/FALSE
#' @return Called for side effects.
.check_progress <- function(progress) {
    .check_set_caller(".check_progress")
    .check_lgl(
        x = progress,
        len_min = 1,
        len_max = 1,
        allow_na = FALSE,
        allow_null = FALSE
    )
    return(invisible(progress))
}
#' @title Check is function parameters is valid using reasonable defaults
#' @name .check_function
#' @keywords internal
#' @noRd
#' @param fn a function parameter
#' @return Called for side effects.
.check_function <- function(fn) {
    if (.has(fn))
        .check_that(x = is.function(fn))
    return(invisible(fn))
}
#' @title Check is expression parameter is valid using reasonable defaults
#' @name .check_expression
#' @param  list_expr expression parameter
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_expression <- function(list_expr) {
    .check_lst(list_expr,
               len_min = 1, len_max = 1,
               msg = .conf("messages", ".check_expression")
    )
    return(invisible(list_expr))
}
#' @title Does the result have the same number of pixels as the input values?
#' @name .check_processed_values
#' @param values a matrix of processed values
#' @param input_pixels number of pixels in input matrix
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_processed_values <- function(values, input_pixels) {
    .check_set_caller(".check_processed_values")
    .check_that(
        !(is.null(nrow(values))) && nrow(values) == input_pixels
    )
    return(invisible(values))
}
#' @title Does the result have the same number of labels as the input values?
#' @name .check_processed_labels
#' @param values a matrix of processed values
#' @param n_labels number of labels in input matrix
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_processed_labels <- function(values, n_labels) {
    .check_set_caller(".check_processed_labels")
    .check_that(ncol(values) == n_labels)
}
#' @title Prepare default message for invalid parameter
#' @name .check_param_message
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param param  parameter name
#' @param msg    message to be issued
#' @return A valid message
#' @keywords internal
#' @noRd
.check_param_message <- function(param) {
    # make default message
    msg <- paste0("invalid ", param, " parameter")
    return(msg)
}
#' @title Prepare default message for variable
#' @name .check_var_message
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param var  parameter name
#' @param msg    message to be issued
#' @return A valid message
#' @keywords internal
#' @noRd
.check_var_message <- function(var) {
    # make default message
    msg <- paste0("invalid ", var, " variable")
    return(msg)
}
#' @title Does the input data contain a set of predicted values?
#' @name .check_predicted
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_predicted <- function(data) {
    .check_set_caller(".check_predicted")
    .check_chr_contains(
        x = names(data),
        contains = "predicted"
    )
    .check_chr_within(
        x = .conf("ts_predicted_cols"),
        within = names(data[["predicted"]][[1]])
    )
    return(invisible(data))
}
#' @title Does the input data contain a raster cube?
#' @name .check_is_raster_cube
#' @param cube a sits cube to be tested
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_is_raster_cube <- function(cube) {
    # set caller to show in errors
    .check_set_caller(".check_is_raster_cube")
    .check_that(inherits(cube, "raster_cube"))
    return(invisible(cube))
}
#' @title Does the input data contain a vector cube?
#' @name .check_is_vector_cube
#' @param cube a sits cube to be tested
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_is_vector_cube <- function(cube) {
    # set caller to show in errors
    .check_set_caller(".check_is_vector_cube")
    .check_that(inherits(cube, "vector_cube"))
    return(invisible(cube))
}

#' @title Check if cube is a probs cube
#' @name .check_is_probs_cube
#' @param cube a sits cube to be tested
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_is_probs_cube <- function(cube) {
    # set caller to show in errors
    .check_set_caller(".check_is_probs_cube")
    .check_that(inherits(cube, "probs_cube"))
    return(invisible(cube))
}
#' @title Check if cube is a variance cube
#' @name .check_is_variance_cube
#' @param cube a sits cube to be tested
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#'
.check_is_variance_cube <- function(cube) {
    # set caller to show in errors
    .check_set_caller(".check_is_variance_cube")
    .check_that(inherits(cube, "variance_cube"))
    return(invisible(cube))
}
#' @title Check if cube is a uncert cube
#' @name .check_is_uncert_cube
#' @param cube a sits cube to be tested
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_is_uncert_cube <- function(cube) {
    .check_set_caller(".check_is_uncert_cube")
    .check_that(inherits(cube, "uncertainty_cube"))
    return(invisible(cube))
}
#' @title Check if cube is a classified image
#' @name .check_is_class_cube
#' @param cube a sits cube to be tested
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_is_class_cube <- function(cube) {
    .check_set_caller(".check_is_class_cube")
    .check_that(inherits(cube, "class_cube"))
    return(invisible(cube))
}
#' @title Check if cube is a results cube
#' @name .check_is_results_cube
#' @param bands bands of the cube
#' @param labels labels of the cube
#' @return  TRUE/FALSE
#' @keywords internal
#' @noRd
.check_is_results_cube <- function(bands, labels) {
    .check_set_caller(".check_is_results_cube")
    if (!(is.null(bands)) &&
        all(bands %in% .conf("sits_results_bands"))) {
        results_cube <- TRUE
    } else {
        results_cube <- FALSE
    }
    # results cube should have only one band
    if (results_cube) {
        .check_that(length(bands) == 1)

        # is label parameter was provided in labelled cubes?
        if (bands %in% c("probs", "bayes")) {
            .check_chr(
                labels,
                len_min = 1,
                allow_duplicate = FALSE,
                is_named = TRUE,
                msg = .conf("messages", ".check_is_results_cube_probs")
            )
        }
        # labels should be named in class cubes?
        if (bands == "class") {
            .check_length(
                labels,
                len_min = 2,
                is_named = TRUE,
                msg = .conf("messages", ".check_is_results_cube_class")
            )
        }
    }
    return(results_cube)
}
#' @title Check that cube is regular
#' @name .check_cube_is_regular
#' @keywords internal
#' @noRd
#' @param cube  datacube
#' @return Called for side effects.
.check_cube_is_regular <- function(cube) {
    .check_set_caller(".check_cube_is_regular")
    .check_that(.cube_is_regular(cube))
    return(invisible(TRUE))
}
#' @title Does the input data contain a sits accuracy object?
#' @name .check_is_sits_accuracy
#' @param data a sits accuracy object
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_is_sits_accuracy <- function(data) {
    .check_set_caller(".check_is_sits_accuracy")
    .check_that(inherits(data, what = "sits_accuracy"))
    return(invisible(data))
}
#' @title Does the input data contain a sits model?
#' @name .check_is_sits_model
#' @param model a sits model
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_is_sits_model <- function(model) {
    .check_set_caller(".check_is_sits_model")
    .check_that(inherits(model, "sits_model"))
    # Check model samples
    samples <- .ml_samples(model)
    .check_samples(samples)
    return(invisible(model))
}
#' @title Does the data contain the cols of sample data and is not empty?
#' @name .check_samples
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples <- function(data) {
    # set caller to show in errors
    .check_set_caller(".check_samples")
    .check_na_null_parameter(data)
    UseMethod(".check_samples", data)
}
#' @title Does the data contain the cols of time series?
#' @name .check_samples.sits
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#' @export
.check_samples.sits <- function(data) {
    .check_that(all(.conf("df_sample_columns") %in% colnames(data)))
    .check_that(nrow(data) > 0)
    return(invisible(data))
}
#' @title Does the tibble contain the cols of time series?
#' @name .check_samples.tbl_df
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#' @export
.check_samples.tbl_df <- function(data) {
    data <- tibble::as_tibble(data)
    .check_that(all(.conf("df_sample_columns") %in% colnames(data)))
    .check_that(nrow(data) > 0)
    class(data) <- c("sits", class(data))
    return(invisible(data))
}
#' @title Does the input contain the cols of time series?
#' @name .check_samples.default
#' @param data input data
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#' @export
.check_samples.default <- function(data) {
    if (is.list(data)) {
        class(data) <- c("list", class(data))
        data <- tibble::as_tibble(data)
        data <- .check_samples(data)
    } else {
        stop(.conf("messages", ".check_samples_default"))
    }
    return(invisible(data))
}
#' @rdname check_functions
#' @keywords internal
#' @noRd
.check_raster_cube_files <- function(x, ...) {
    .check_set_caller(".check_raster_cube_files")
    # check for data access
    robj <- tryCatch(
        .raster_open_rast(.tile_path(x)),
        error = function(e) {
            return(NULL)
        })
    # return error if data is not accessible
    .check_that(!(is.null(robj)))
    return(invisible(x))
}
#' @title Does input data has time series?
#' @name .check_samples_ts
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_ts <- function(data) {
    .check_set_caller(".check_samples_ts")
    data <- .check_samples(data)
    .check_that("time_series" %in% colnames(data))
    # check there is an Index column
    .check_samples_ts_index(data)
    # check if all samples have the same bands
    .check_samples_ts_bands(data)

    return(invisible(data))
}
#' @title Is there an index column in the time series?
#' @name .check_samples_ts_index
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_ts_index <- function(data) {
    .check_set_caller(".check_samples_ts_index")
    # Get unnested time series
    ts <- .samples_ts(data)
    # check there is an Index column
    .check_that(x = "Index" %in% colnames(ts))

    return(invisible(data))
}
#' @title Are the bands in the time series the same?
#' @name .check_samples_ts_bands
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_ts_bands <- function(data) {
    .check_set_caller(".check_samples_ts_bands")
    # check if all samples have the same bands
    n_bands <- unique(lengths(data[["time_series"]]))
    .check_that(length(n_bands) == 1)

    return(invisible(data))
}
#' @title Can the input data be used for training?
#' @name .check_samples_train
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_train <- function(data) {
    .check_set_caller(".check_samples_train")
    data <- .check_samples_ts(data)
    # check that there is no NA in labels
    labels <- .samples_labels(data)
    .check_that(!("NoClass" %in% labels) && !("" %in% labels) &&
            !anyNA(labels))
    # Get unnested time series
    ts <- .ts(data)
    # check there are no NA in distances
    .check_that(!(anyNA(ts)))
    # check samples timeline
    .check_samples_timeline(data)
    return(invisible(data))
}
#' @title Is the samples_validation object valid?
#' @name .check_samples_validation
#' @param samples_validation a sits tibble with validation samples
#' @param labels labels of the data cube to be validated
#' @param timeline timeline of the data cube to be validated
#' @param bands  bands of the data cube to be validated
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_validation <- function(samples_validation,
                                      labels,
                                      timeline,
                                      bands) {
    .check_set_caller(".check_samples_validation")
    # check if the validation samples are ok
    samples_validation <- .check_samples(samples_validation)
    # check if the labels matches with train data
    .check_that(
        all(.samples_labels(samples_validation) %in% labels) &&
            all(labels %in% .samples_labels(samples_validation))
    )
    # check if the timeline matches with train data
    .check_that(
        length(.samples_timeline(samples_validation)) == length(timeline)
    )
    # check if the bands matches with train data
    .check_that(
        all(.samples_bands(samples_validation) %in% bands) &&
            all(bands %in% .samples_bands(samples_validation))
    )
    return(invisible(samples_validation))
}
#' @title Do the samples contain a cluster column?
#' @name .check_samples_cluster
#' @param data a sits tibble with cluster col
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#  Are the samples valid?
.check_samples_cluster <- function(data) {
    .check_set_caller(".check_samples_cluster")
    data <- .check_samples(data)
    # is the input data the result of a cluster function?
    .check_that("cluster" %in% names(data))
    return(invisible(data))
}
#' @title Do the samples contain a valid timeline?
#' @name .check_samples_timeline
#' @param data a sits tibble
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_timeline <- function(data) {
    .check_set_caller(".check_samples_timeline")
    ts <- .ts(data)
    n_times <- unique(unlist(tapply(
        .ts_sample_id(ts), .ts_sample_id(ts), length,
        simplify = FALSE
    ), use.names = FALSE))
    .check_that(length(n_times) == 1)
    return(invisible(data))
}
#' @title Is the object a valid point?
#' @name .check_point
#' @param x 2D point
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_point <- function(x) {
    .check_set_caller(".check_point")
    .check_that(.is_point(x))
}
#' @title Are the predictors valid?
#' @name .check_predictors
#' @param pred a tibble with predictors values
#' @param samples samples from where the predictors have been calculated
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_predictors <- function(pred, samples) {
    .check_set_caller(".check_predictors")
    cols <- .pred_cols # From predictors API
    .check_that(cols %in% colnames(pred))
    .check_that(nrow(pred) > 0)
    n_bands <- length(.samples_bands.sits(samples))
    n_times <- length(.samples_timeline(samples))
    if(inherits(samples, "sits_base"))
        n_bands_base <- length(.samples_base_bands(samples))
    else
        n_bands_base <- 0
    .check_that(ncol(pred) == 2 + n_bands * n_times + n_bands_base)
    return(invisible(pred))
}
#' @title Does the data contain the cols of sample data and is not empty?
#' @name .check_smoothness
#' @param smoothness a vector or numeric value
#' @param nlabels    a numeric value
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_smoothness <- function(smoothness, nlabels) {
    .check_set_caller(".check_smoothness")
    .check_that(length(smoothness) == 1 || length(smoothness) == nlabels)
    return(invisible(smoothness))
}
#' @title Check if data contains predicted and reference values
#' @name .check_pred_ref_match
#' @param reference  vector with reference labels
#' @param predicted  vector with predicted labels
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_pred_ref_match <- function(reference, predicted) {
    .check_set_caller(".check_pred_ref_match")
    .check_that(length(reference) == length(predicted))
    return(invisible(reference))
}
#' @title Do the samples and tile match timelines?
#' @name .check_samples_tile_match_timeline
#' @param samples  samples organised as a tibble
#' @param tile  one tile of a data cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_tile_match_timeline <- function(samples, tile) {
    .check_set_caller(".check_samples_tile_match_timeline")
    # do they have the same timelines?
    samples_timeline_length <- length(.samples_timeline(samples))
    tiles_timeline_length <- length(.tile_timeline(tile))
    .check_that(samples_timeline_length == tiles_timeline_length)
    return(invisible(samples))
}
#' @title Do the samples and tile match bands?
#' @name .check_samples_tile_match_bands
#' @param samples  samples organised as a tibble
#' @param tile  one tile of a data cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_samples_tile_match_bands <- function(samples, tile) {
    .check_set_caller(".check_samples_tile_match_bands")
    # do they have the same bands?
    tile_bands <- .tile_bands(tile)
    bands <- .samples_bands(samples)
    .check_that(all(bands %in% tile_bands))
    return(invisible(samples))
}
#' @title Does the input data contains valid reference labels?
#' @name  .check_labels
#' @param  data vector with labels
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_labels <- function(data) {
    .check_set_caller(".check_labels")
    .check_that(!("NoClass" %in% data))
    return(invisible(data))
}
#' @title Does the class cube contain enough labels?
#' @name  .check_labels_class_cube
#' @param  cube class cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_labels_class_cube <- function(cube) {
    .check_set_caller(".check_labels_class_cube")
    # select the files for the classified cube
    files <- unlist(.cube_paths(cube))
    # open the first file
    classes_list <- purrr::map(files, function(file) {
        r <- .raster_open_rast(file)
        # get the frequency table
        freq <- .raster_freq(r)
        # get the classes as numerical values
        classes_tile <- as.character(freq[["value"]])
        names(classes_tile) <- file
        return(classes_tile)
    })
    classes_num <- unique(unlist(classes_list))
    classes_num <- classes_num[!is.na(classes_num)]
    labels_num <- names(unlist(.cube_labels(cube, dissolve = FALSE)))
    # do the labels and raster numbers match?
    .check_that(all(classes_num %in% labels_num))
    return(invisible(cube))
}
#' @title Does the probs cube contains required labels?
#' @name  .check_labels_probs_cube
#' @param  cube class cube
#' @param  labels Labels to be used
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_labels_probs_cube <- function(cube, labels) {
    .check_set_caller(".check_labels_probs_cube")

    # check that the labels are part of the cube
    .check_that(all(labels %in% .cube_labels(cube)))
    return(invisible(cube))
}
#' @title Check if an object is a bbox
#' @noRd
#' @return Called for side effects.
.check_bbox <- function(x) {
    .check_set_caller(".check_bbox")
    .check_that(setequal(names(x), c(.bbox_cols, "crs")))
    return(invisible(x))
}
#' @title Check if roi is specified correcty
#' @name .check_roi
#' @param roi           Region of interest
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_roi <- function(roi = NULL) {
    # set caller to show in errors
    .check_set_caller(".check_roi")
    if (!.has(roi))
        return(invisible(NULL))
    # check vector is named
    .check_names(roi)
    # check that names are correct
    roi_names <- names(roi)
    names_ll <- c("lon_min", "lon_max", "lat_min", "lat_max")
    names_x  <- c("xmin", "xmax", "ymin", "ymax")
    .check_that(all(names_ll %in% roi_names) ||
                all(names_x  %in% roi_names)
    )
    return(invisible(roi))
}
#' @title Check if roi or tiles are provided
#' @name .check_roi_tiles
#' @param roi           Region of interest
#' @param tiles         Tiles to be included in cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_roi_tiles <- function(roi, tiles) {
    # set caller to show in errors
    .check_set_caller(".check_roi_tiles")
    # Ensures that only a spatial filter is informed
    .check_that(xor(is.null(roi), is.null(tiles)))
    return(invisible(roi))
}
#' @title Check if grid system is supported
#' @name .check_grid_system
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param grid_system   Requested grid system
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_grid_system <- function(grid_system) {
    .check_chr_contains(
        x = names(.conf("grid_systems")),
        contains = grid_system,
        case_sensitive = TRUE,
        discriminator = "one_of",
        can_repeat = FALSE,
        msg = .conf("messages", ".check_grid_system")
    )
    return(invisible(grid_system))
}

#' @title Check if bands are part of a data cube
#' @name .check_cube_bands
#' @param cube          Data cube
#' @param bands         Bands to be check
#' @param add_cloud     Include the cloud band?
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_cube_bands <- function(cube, bands, add_cloud = TRUE) {
    # set caller to show in errors
    .check_set_caller(".check_cube_bands")
    # all bands are upper case
    bands <- toupper(bands)
    cube_bands <- toupper(.cube_bands(cube = cube, add_cloud = add_cloud))
    .check_that(all(bands %in% cube_bands))
    return(invisible(cube))
}
#' @title Check if tiles are part of a data cube
#' @name .check_cube_tiles
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube          Data cube
#' @param tiles         Tile to be check
#' @param add_cloud     Include the cloud band?
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_cube_tiles <- function(cube, tiles) {
    # set caller to show in errors
    .check_set_caller(".check_cube_tiles")
    .check_that(all(tiles %in% .cube_tiles(cube)))
    return(invisible(cube))
}
#' @title Check if all rows in a cube has the same bands
#' @name .check_cube_row_same_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube          Data cube
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_cube_row_same_bands <- function(cube) {
    bands <- purrr::map(.compact(slider::slide(cube, .tile_bands)), length)
    bands <- .dissolve(bands)

    .check_that(length(unique(bands)) == 1)
}
#' @title Check if  cubes have the same bbox
#' @name .check_cubes_same_bbox
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return Called for side effects.
.check_cubes_same_bbox <- function(cube1, cube2) {
    .check_set_caller(".check_cubes_same_bbox")
    tolerance <- .conf(
        "sources", .cube_source(cube1),
        "collections", .cube_collection(cube1),
        "ext_tolerance"
    )
    ok <- slider::slide2_lgl(
        cube1, cube2,
        function(tile_first, tile_cube) {
            return(.bbox_equal(
                .tile_bbox(tile_first),
                .tile_bbox(tile_cube),
                tolerance = tolerance
            ))
        }
    )
    .check_that(all(ok))
    return(invisible(cube1))
}
#' @title Check if cubes have the same size
#' @name .check_cubes_same_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return Called for side effects..
.check_cubes_same_size <- function(cube1, cube2) {
    .check_set_caller(".check_cubes_same_size")
    .check_that(
        all(.cube_ncols(cube1) == .cube_ncols(cube2)) &&
        all(.cube_nrows(cube1) == .cube_nrows(cube2))
    )
    return(invisible(cube1))
}

#' @title Check if cubes have the same tiles
#' @name .check_cubes_same_tiles
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return Called for side effects.
.check_cubes_same_tiles <- function(cube1, cube2) {
    .check_set_caller(".check_cubes_same_tiles")
    .check_that(nrow(cube1) == nrow(cube2))
    return(invisible(cube1))
}
#' @title Check if cubes have the same labels
#' @name .check_cubes_same_labels
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return Called for side effects.
.check_cubes_same_labels <- function(cube1, cube2) {
    .check_set_caller(".check_cubes_same_labels")
    .check_that(
        all(.cube_labels(cube1) %in% .cube_labels(cube2)) &&
            all(.cube_labels(cube2) %in% .cube_labels(cube1))
    )
    return(invisible(cube1))
}
#' @title Check if cubes have the same timeline
#' @name .check_cubes_same_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return Called for side effects.
.check_cubes_same_timeline <- function(cube1, cube2) {
    .check_set_caller(".check_cubes_same_timeline")
    .check_that(all(.cube_timeline(cube1)[[1]] == .cube_timeline(cube2)[[1]]))
    return(invisible(cube1))
}
#' @title Check if two cubes have the same organization
#' @name .check_cubes_match
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  cube1     input data cube
#' @param  cube2     input data cube
#' @return Called for side effects.
.check_cubes_match <- function(cube1, cube2) {
    .check_set_caller(".check_cubes_match")
    # check same size
    .check_cubes_same_tiles(cube1, cube2)
    .check_cubes_same_size(cube1, cube2)
    .check_cubes_same_bbox(cube1, cube2)
    .check_cubes_same_timeline(cube1, cube2)
    .check_cubes_same_labels(cube1, cube2)
    return(invisible(cube1))
}
#' @title Check if list of probs cubes have the same organization
#' @name .check_probs_cube_lst
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  cubes         list of input data cubes
#' @return Called for side effects
.check_probs_cube_lst <- function(cubes) {
    .check_set_caller(".check_probs_cube_lst")
    .check_that(is.list(cubes))
    .check_that(length(cubes) >= 2)
    # is every cube a probs cube?
    purrr::map(cubes, .check_is_probs_cube)
    # check same size
    first <- cubes[[1]]
    for (i in c(2:length(cubes))) {
        .check_cubes_match(first, cubes[[i]])
    }
    return(invisible(cubes))
}
#' @title Check if list of uncertainty cubes have the same organization
#' @name .check_uncert_cube_lst
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param  uncert_cubes     list of input data cubes
#' @return Called for side effects
.check_uncert_cube_lst <- function(uncert_cubes) {
    .check_set_caller(".check_uncert_cube_lst")
    .check_that(length(uncert_cubes) >= 2)
    .check_that(is.list(uncert_cubes))
    # is every cube a probs cube
    purrr::map(uncert_cubes, .check_is_uncert_cube)
    # check same size
    first <- uncert_cubes[[1]]
    for (i in c(2:length(uncert_cubes))) {
        .check_cubes_same_size(first, uncert_cubes[[i]])
    }
    return(invisible(uncert_cubes))
}
#' @title Check if errox matrix and area are cosrrect
#' @name .check_error_matrix_area
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  error_matrix  Error matrix for classification
#' @param  area  Area of each class
#' @return Called for side effects.
#' @keywords internal
#' @noRd
#'
.check_error_matrix_area <- function(error_matrix, area) {
    .check_set_caller(".check_error_matrix_area")
    .check_that(
        x = all(dim(error_matrix) > 1),
        msg = .conf("messages", ".check_error_matrix_area_dim")
    )
    .check_that(
        x = length(unique(dim(error_matrix))) == 1,
        msg = .conf("messages", ".check_error_matrix_square")
    )
    .check_that(
        x = all(colnames(error_matrix) == rownames(error_matrix)),
        msg = .conf("messages", ".check_error_matrix_names")
    )
    .check_that(
        x = unique(dim(error_matrix)) == length(area),
        msg = .conf("messages", ".check_error_matrix_area")
    )
    .check_that(
        x = all(names(area) %in% colnames(error_matrix)),
        msg = .conf("messages", ".check_error_matrix_labels")
    )
    return(invisible(error_matrix))
}
#' @title Checks if the required packages are installed
#' @name .check_require_packages
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param x   the name of the required package
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_require_packages <- function(x, ...,
                                    msg = "Please install package(s)") {
    .check_set_caller(".check_require_packages")
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
#' @title Checks if the tibble/data.frame is empty
#' @name .check_empty_data_frame
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param x a data frame
#' @return Called for side effects.
#' @keywords internal
#' @noRd
.check_empty_data_frame <- function(x, msg = NULL, ...) {
    .check_set_caller(".check_empty_data_frame")
    .check_that(nrow(x) > 0)
    return(invisible(x))
}
#' @title Checks if the endmembers parameter is valid
#' @name .check_endmembers_parameter
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @param em  Endmembers description (data.frame)
#' @return Called for side effects.
.check_endmembers_parameter <- function(em) {
    .check_set_caller(".check_endmembers_parameter")
    .check_that(inherits(em, c("data.frame", "character")))
    return(invisible(em))
}
#' @title Checks if the endmembers data is in a valid parameter
#' @name .check_endmembers_tbl
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @param em   Reference spectra endmembers.
#' @param cube A sits cube
#' @return Called for side effects.
.check_endmembers_tbl <- function(em) {
    .check_set_caller(".check_endmembers_tbl")
    # Pre-condition
    .check_that(!anyNA(em))
    # Pre-condition
    .check_chr_contains(
        x = colnames(em),
        contains = c("TYPE", "CLASS"),
        discriminator = "any_of",
        msg = .conf("messsages", ".check_endmembers_parameter")
    )
    .check_endmembers_fracs(em)
    return(invisible(em))
}
#' @title Checks if the endmembers data is in a valid parameter
#' @name .check_endmembers_fracs
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @param em   Reference spectra endmembers.
#' @param cube A sits cube
#' @return Called for side effects.
.check_endmembers_fracs <- function(em) {
    .check_set_caller(".check_endmembers_fracs")
    # Pre-condition
    .check_that(all(length(.endmembers_fracs(em)) >= 1))
    return(invisible(em))
}
#' @title Checks if the bands required by endmembers exist
#' @name .check_endmembers_bands
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @param em    Reference spectra endmembers.
#' @param bands Bands to compute endmembers.
#' @return Called for side effects.
.check_endmembers_bands <- function(em, bands) {
    .check_set_caller(".check_endmembers_bands")
    .check_that(all(.band_eo(.endmembers_bands(em)) %in% bands))
    return(invisible(em))
}
#' @title Checks if working in documentation mode
#' @name .check_documentation
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @return TRUE/FALSE
#' @keywords internal
#' @noRd
.check_messages <- function() {
    # if working on sits documentation mode, no progress bar
    if (Sys.getenv("SITS_DOCUMENTATION_MODE") == "true" ||
        Sys.getenv("SITS_DOCUMENTATION_MODE") == "TRUE") {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' @title Checks if STAC items are correct
#' @name .check_stac_items
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @param items STAC items
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_stac_items <- function(items) {
    # set caller to show in errors
    .check_set_caller(".check_stac_items")
    .check_null_parameter(items)
    .check_that(rstac::items_length(items) > 0)
    return(invisible(items))
}
#' @title Checks recovery
#' @name .check_recovery
#' @param data     existing data
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_recovery <- function(data) {
    if (.check_messages()) {
        message(.conf("messages", ".check_recovery"))
    }
    return(invisible(data))
}
#' @title Checks discriminators
#' @name .check_discriminator
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param discriminator     discriminator for within and contains
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_discriminator <- function(discriminator) {
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
        stop(".check_chr_within: discriminator should be one of",
                "'one_of', 'any_of', 'all_of', 'none_of', or 'exactly'.",
            call. = TRUE
        )
    }
    return(invisible(discriminator))
}
#' @title Checks view bands are defined
#' @name .check_bw_rgb_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube      cube to choose band
#' @param band      B/W band for view
#' @param red       Red band for view
#' @param green     Green band for view
#' @param blue      Blue band for view
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_bw_rgb_bands <- function(cube, band, red, green, blue) {
    .check_set_caller(".check_bw_rgb_bands")
    if (!.has(band) || !(.has(red) && .has(green) && .has(blue)))
        band <- .band_best_guess(cube)
    return(band)
}
#' @title Check available bands
#' @name .check_available_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube      Data cube
#' @param band      B/W band for view
#' @param red       Red band for view
#' @param green     Green band for view
#' @param blue      Blue band for view
#' @return band for B/W and "RGB" for color images
#' @keywords internal
#' @noRd
.check_available_bands <- function(cube, band, red, green, blue) {
    .check_set_caller(".check_available_bands")
    if (.has(band)) {
        # check band is available
        .check_that(band %in% .cube_bands(cube))
        return(band)
    } else if (.has(red) && .has(green) && .has(blue)) {
        bands <- c(red, green, blue)
        # check bands are available
        .check_that(all(bands %in% .cube_bands(cube)))
        return("RGB")
    }
}

#' @title Check if the provided object is a vector
#' @name .check_vector_object
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @param v_obj  a sf, sfc or sfg object
#' @return No return value, called for side effects.
#' @keywords internal
#' @noRd
.check_vector_object <- function(v_obj) {
    .check_set_caller(".check_vector_object")
    .check_chr_contains(
        x = class(v_obj),
        contains = c("sf", "sfc", "sfg"),
        discriminator = "one_of",
        msg = .conf("messages", ".check_vector_object")
    )
    return(invisible(NULL))
}
#' @title Checks local items
#' @name .check_local_items
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @param items      Items with information on local cube
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_local_items <- function(items) {
    .check_set_caller(".check_local_items")
    # pre-condition
    .check_tiles(unique(items[["tile"]]))
    .check_crs(unique(items[["crs"]]))
    return(invisible(items))
}
#' @title Checks tiles
#' @name .check_tiles
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param tiles     vector with tile names
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_tiles <- function(tiles) {
    .check_set_caller(".check_tiles")
    # pre-condition
    .check_that(length(tiles) >= 1)
    return(invisible(tiles))
}
#' @title Checks palette
#' @name .check_palette
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param palette      Character vector with palette name
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_palette <- function(palette) {
    # verifies if cols4all package is installed
    .check_require_packages("cols4all")
    # set caller to show in errors
    .check_set_caller(".check_palette")
    # check if palette name is in RColorBrewer
    brewer_pals <- rownames(RColorBrewer::brewer.pal.info)
    # if not a Brewer palette, check that it is a cols4all palette
    if (!palette %in% brewer_pals)
        .check_chr_contains(x = cols4all::c4a_palettes(),
                            contains = palette,
                            discriminator = "any_of")
    return(invisible(NULL))
}
#' @title Check legend defined as tibble
#' @name .check_legend
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param legend      Legend (as tibble)
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_legend <- function(legend) {
    .check_set_caller(".check_legend")
    .check_chr_contains(
        x = colnames(legend),
        contains = c("name", "color"),
        discriminator = "all_of",
        msg = .conf("messages", ".check_legend")
    )
    return(invisible(NULL))
}
#' @title Checks legend_position
#' @name .check_legend_position
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param legend_position      Character vector with legend position
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_legend_position <- function(legend_position) {
    .check_set_caller(".check_legend_position")
    .check_chr_contains(
        x = legend_position,
        contains = c("outside", "inside"),
        discriminator = "one_of",
        msg = .conf("messages", ".check_legend_position")
    )
    return(invisible(NULL))
}
#' @title Checks if band is in list of bands
#' @name .check_band_in_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param band    Name of band
#' @param bands   List of bands
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_band_in_bands <- function(band, bands) {
    .check_set_caller("check_band_in_bands")
    .check_chr_contains(
        x = bands,
        contains = band,
        discriminator = "one_of",
        msg = .conf("messages", ".check_band_in_bands")
    )
    return(invisible(NULL))
}
#' @title Checks shapefile attribute
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @name .check_shp_attribute
#' @param sf_shape      sf object read from a shapefile
#' @param shp_attr      name of attribute param in shapefile
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_shp_attribute <- function(sf_shape, shp_attr) {
    # set caller to show in errors
    .check_set_caller(".check_shp_attribute")
    # get the data frame associated to the shapefile
    shp_df <- sf::st_drop_geometry(sf_shape)
    if (.has(shp_attr))
        .check_that(length(as.character(shp_df[1, (shp_attr)])) > 0)
    return(invisible(sf_shape))
}
#' @title Checks validation file
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @name .check_validation_file
#' @param validation     Path to a CSV file
#' @param shp_attr      name of attribute param in shapefile
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_validation_file <- function(validation) {
    # set caller to show in errors
    .check_set_caller(".check_validation_file")
    if (is.character(validation))
        .check_that(tolower(.file_ext(validation)) == "csv")
    return(invisible(validation))
}
#' @title Checks filter function
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description
#' Checks if the paramter is a function
#' @param filter_fn     Filter function
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_filter_fn <- function(filter_fn = NULL) {
    .check_set_caller(".check_filter_fn")
    if (.has(filter_fn))
        .check_that(is.function(filter_fn))
    return(invisible(NULL))
}
#' @title Checks distance method
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description
#' Checks if the parameter is a valid distance method for a dendrogram
#' @param dist_method    Distance method
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_dist_method <- function(dist_method) {
    .check_set_caller(".check_dist_method")
    .check_that(dist_method %in% .conf("dendro_dist_method"))
    return(invisible(NULL))
}
#' @title Checks linkage method
#' @name .check_linkage_method
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description
#' Checks if the parameter is a valid linkage method for a dendrogram
#' @param linkage    Linkage method
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_linkage_method <- function(linkage) {
    .check_set_caller(".check_linkage_method")
    .check_that(linkage %in% .conf("dendro_linkage"))
    return(invisible(NULL))
}
#' @title Check netrc file
#' @name .check_netrc_gdal
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @description
#' Check if netrc file exists and if its content is correct
#' @param attributes    Attributes required from the netrc file
#' @return Called for side effects
#' @keywords internal
#' @noRd
.check_netrc_gdal <- function(attributes) {
    .check_set_caller(".check_netrc_gdal")
    # define if the current GDAL version is reading netrc from env variable
    is_gdal_reading_netrc <- .gdal_version() >= "3.7.0"
    # define from where `netrc` file must be loaded
    # case 1 - gdal environment variable (requires GDAL >= 3.7.0)
    netrc_from_var <- ifelse(
        is_gdal_reading_netrc,
        Sys.getenv("GDAL_HTTP_NETRC_FILE", unset = NA),
        NA
    )
    # case 2 - netrc file stored in user home directory
    netrc_from_home <- ifelse(
        .Platform[["OS.type"]] == "windows",
        .conf("gdal_netrc_file_path_win"),
        .conf("gdal_netrc_file_path")
    )
    # define which netrc file will be used
    netrc_file <- ifelse(
        is.na(netrc_from_var),
        netrc_from_home,
        netrc_from_var
    )
    # if the env variable is used, then, warning users not to set the GDAL
    # variable using `Sys.setenv`
    if (!is.na(netrc_from_var)) {
        warning(.conf("messages", ".check_netrc_gdal_var"))
    }
    # check if file exist
    .check_that(file.exists(netrc_file))
    # load netrc content
    netrc_content <- readLines(netrc_file)
    # check netrc file content
    .check_that(
        any(
            purrr::map_lgl(netrc_content, function(x) {
                stringr::str_detect(x, attributes)
            })
        )
    )
    return(invisible(NULL))
}
#' @title Check torch hyperparameters
#' @name .check_opt_hparams
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param opt_hparams            Hyperparameters.
#' @param optim_params_function  Function used for optimization.
#' @return Called for side effects
#' @keywords internal
#' @noRd
#
.check_opt_hparams <- function(opt_hparams, optim_params_function) {
    .check_lst_parameter(opt_hparams,
                         msg = .conf("messages", ".check_opt_hparams")
    )
    .check_chr_within(
        x = names(opt_hparams),
        within = names(optim_params_function),
        msg = .conf("messages", ".check_opt_hparams")
    )
    return(invisible(NULL))
}
#' @title Check that cube period is unique
#' @name .check_unique_period
#' @param cube              Data cube.
#' @return Called for side effects
#' @keywords internal
#' @noRd
#
.check_unique_period <- function(cube) {
    .check_that(
        x = length(.cube_period(cube)) == 1,
        msg = .conf("messages", ".check_unique_period")
    )
}
#' @title Checks if warnings should be displayed
#' @name .check_warnings
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @return TRUE/FALSE
#' @keywords internal
#' @noRd
.check_warnings <- function() {
    # if working on sits documentation mode, no progress bar
    if (Sys.getenv("SITS_DOCUMENTATION_MODE") == "true" ||
        Sys.getenv("SITS_DOCUMENTATION_MODE") == "TRUE") {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
#' @title Warning when converting a bbox into a sf object
#' @name .check_warnings_bbox_as_sf
#' @noRd
#' @returns Called for side effects
.check_warnings_bbox_as_sf <- function() {
    if (.check_warnings())
        warning(.conf("messages", ".bbox_as_sf"), call. = FALSE)
    return(invisible(NULL))
}
#' @title Warning when labels have no colors preset
#' @name .check_warnings_colors_get
#' @noRd
#' @returns Called for side effects
.check_warnings_colors_get <- function(missing, palette){
    if (.check_warnings()) {
        warning(.conf("messages", ".colors_get_missing"), toString(missing))
        warning(.conf("messages", ".colors_get_missing_palette"), palette)
        # grDevices does not work with one color missing
    }
    return(invisible(NULL))
}
#' @title Warning when cube has no CLOUD band
#' @name .check_warnings_regularize_cloud
#' @noRd
#' @returns Called for side effects
.check_warnings_regularize_cloud <- function(cube){
    if (!all(.cube_contains_cloud(cube))) {
        if (.check_warnings())
            warning(.conf("messages", "sits_regularize_cloud"),
                    call. = FALSE,
                    immediate. = TRUE
            )
    }
    return(invisible(NULL))
}
#' @title Warning when cube has multiple values of CRS
#' @name .check_warnings_regularize_crs
#' @noRd
#' @returns Called for side effects
.check_warnings_regularize_crs <- function(){
    if (.check_warnings())
        warning(.conf("messages", "sits_regularize_crs"),
                call. = FALSE,
                immediate. = TRUE
        )
    return(invisible(NULL))
}
#' @title Warning when cube is being regularized directly from STAC files
#' @name .check_warnings_regularize_local
#' @noRd
#' @returns Called for side effects
.check_warnings_regularize_local <- function(cube){
    if (!.cube_is_local(cube) && .check_warnings()) {
        warning(.conf("messages", "sits_regularize_local"),
                call. = FALSE, immediate. = TRUE
        )
    }
    return(invisible(NULL))
}
#' @title Warning when cube has more than one timeline
#' @name .check_warnings_timeline_cube
#' @noRd
#' @returns Called for side effects
.check_warnings_timeline_cube <- function(){
    if (.check_warnings())
        warning(.conf("messages", "sits_timeline_raster_cube"),
                call. = FALSE
        )
    return(invisible(NULL))
}
