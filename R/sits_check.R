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
#' minimum value that the user can provide in function parameter. Only works for
#' numeric check. By default is \code{-Inf}.
#' @param max           A atomic \code{vector} of numeric indicating the
#' maximum value that the user can provide in function parameter. Only works for
#' numeric check. By default is \code{Inf}.
#' @param allow_zero    A \code{logical} indicating if the check permits zero
#' values. Default is TRUE.
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
#' @param expr          A R \code{expression} to be evaluated.
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
    if (length(sys.frames()) > 1)
        envir <- sys.frame(-1)
    assign(".check_caller", caller, envir = envir)

    return(invisible(NULL))
}

#' @rdname check_functions
#'
#' @return \code{.check_identify_caller()} returns a \code{character} value.
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
    caller <- gsub(pattern = "^(.*)\\(.*$", replacement = "\\1",
                   x = paste(caller)[[1]])
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
.check_that <- function(x, ...,
                        local_msg = NULL,
                        msg = NULL) {

    value <- (is.logical(x) && all(x)) || (!is.logical(x) && length(x) > 0)

    if (!value) {

        # get caller function name
        caller <- .check_identify_caller()

        # format error message
        if (is.null(msg))
            msg <- sprintf("%s: %%s", caller)
        else
            msg <- sprintf("%s: %s (%%s)", caller, msg)

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
.check_null <- function(x, ...,
                        msg = NULL) {

    .check_that(
        !is.null(x),
        local_msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
.check_na <- function(x, ...,
                      msg = NULL) {

    .check_that(
        !any(is.na(x)),
        local_msg = "NA value is not allowed",
        msg = msg
    )

    return(invisible(x))
}


#' @rdname check_functions
.check_names <- function(x, ...,
                         is_named = TRUE,
                         msg = NULL) {

    # cannot test zero length arguments
    if (length(x) == 0)
        return(invisible(x))

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
    } else
        .check_that(
            is.null(names(x)),
            local_msg = "value should be unnamed",
            msg = msg
        )

    return(invisible(x))
}

#' @rdname check_functions
.check_length <- function(x, ...,
                          len_min = NULL,
                          len_max = NULL,
                          msg = NULL) {

    # pre-condition
    if (!is.null(len_min) && !is.numeric(len_min))
        stop(".check_length: len_min parameter should be numeric.")

    if (!is.null(len_max) && !is.numeric(len_max))
        stop(".check_length: len_max parameter should be numeric.")

    # set error message
    if (!is.null(len_min) && !is.null(len_max) && len_min == len_max)
        local_msg <- sprintf("length should be == %s", len_min)
    else if (is.null(len_min) && is.null(len_max))
        local_msg <- "invalid length" # never throws an error in this case!
    else if (is.null(len_max))
        local_msg <- sprintf("length should be >= %s", len_min)
    else if (is.null(len_min))
        local_msg <- sprintf("length should be <= %s", len_max)
    else
        local_msg <- sprintf("length should be between %s and %s",
                             len_min, len_max)

    if (is.null(len_min))
        len_min <- 0

    if (is.null(len_max))
        len_max <- 2^31

    .check_that(
        len_min <= length(x) && length(x) <= len_max,
        local_msg = local_msg,
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
.check_apply <- function(x, fn_check, ...) {

    if (!is.function(fn_check))
        stop(".check_apply: fn_check should be a function.", call. = TRUE)

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
#'
.check_lgl_type <- function(x, ...,
                            msg = NULL) {

    .check_that(
        is.logical(x),
        local_msg = "value is not logical",
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
.check_num_type <- function(x, ...,
                            is_integer = FALSE,
                            msg = NULL) {

    .check_that(
        is.numeric(x),
        local_msg = "value is not a number",
        msg = msg
    )

    # test integer
    if (is_integer) {

        # if length is zero there is nothing to check
        if (length(x) == 0)
            return(invisible(x))

        .check_that(
            is.numeric(x) && all(x == as.integer(x)),
            local_msg = "value is not integer",
            msg = msg
        )
    }

    return(invisible(x))
}

#' @rdname check_functions
.check_chr_type <- function(x, ...,
                            msg = NULL) {

    .check_that(
        is.character(x),
        local_msg = "value is not character type",
        msg = msg
    )

    return(invisible(x))
}

#' @rdname check_functions
.check_lst_type <- function(x, ...,
                            msg = NULL) {

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
#' either \code{min} or \code{max} parameters are defined). It also checks
#' for non-zero and \code{integer} values (if \code{allow_zero=FALSE} and
#' \code{is_integer=TRUE}, respectively).
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
.check_lgl <- function(x, ...,
                       allow_na = FALSE,
                       len_min = NULL,
                       len_max = NULL,
                       allow_null = FALSE,
                       is_named = FALSE,
                       msg = NULL) {

    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x))
        return(invisible(x))

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_lgl_type(x, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    if (!allow_na)
        .check_na(x, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    return(invisible(x))
}

#' @rdname check_functions
.check_num <- function(x, ...,
                       allow_na = FALSE,
                       min = -Inf,
                       max = Inf,
                       allow_zero = TRUE,
                       len_min = NULL,
                       len_max = NULL,
                       allow_null = FALSE,
                       is_integer = FALSE,
                       is_named = FALSE,
                       msg = NULL) {

    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x))
        return(invisible(x))

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_num_type(x, is_integer = is_integer, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    if (!allow_na)
        .check_na(x, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    # check range
    # pre-condition
    if (!is.null(min) && !is.numeric(min))
        stop(".check_num: min parameter should be numeric.")

    if (!is.null(max) && !is.numeric(max))
        stop(".check_num: max parameter should be numeric.")

    # remove NAs before check
    res <- x
    x <- x[!is.na(x)]
    .check_that(
        all(min <= x) && all(x <= max),
        local_msg = "value is out of range",
        msg = msg
    )

    # allow zero
    if (!allow_zero)
        .check_that(
            all(x != 0),
            local_msg = "value cannot be zero",
            msg = msg
        )

    return(invisible(res))
}

#' @rdname check_functions
.check_chr <- function(x, ...,
                       allow_na = FALSE,
                       allow_empty = TRUE,
                       len_min = NULL,
                       len_max = NULL,
                       allow_null = FALSE,
                       is_named = FALSE,
                       regex = NULL,
                       msg = NULL) {

    # check for null and exit if it is allowed
    if (allow_null && is.null(x))
        return(invisible(x))

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_chr_type(x, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    if (!allow_na)
        .check_na(x, msg = msg)

    # check empty
    if (!allow_empty)
        .check_that(
            all(nchar(x[!is.na(x)]) > 0),
            local_msg = "empty value is not allowed",
            msg = msg
        )


    # check names
    .check_names(x, is_named = is_named, msg = msg)

    # check regular expression pattern
    if (!is.null(regex))
        .check_that(
            all(grepl(pattern = regex, x = x)),
            local_msg = "value did not match pattern",
            msg = msg
        )

    return(invisible(x))
}

#' @rdname check_functions
.check_lst <- function(x, ...,
                       min_len = NULL,
                       max_len = NULL,
                       allow_null = FALSE,
                       is_named = TRUE,
                       fn_check = NULL,
                       msg = NULL) {

    # check for null and exit if it is allowed
    if (allow_null && is.null(x))
        return(invisible(x))

    # check NULL
    .check_null(x, msg = msg)

    # check type
    .check_lst_type(x, msg = msg)

    # check length
    .check_length(x, len_min = min_len, len_max = max_len, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    # check using function
    if (!is.null(fn_check))
        .check_apply(x, fn_check = fn_check, msg = msg, ...)

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
#' \code{one_of}, \code{any_of}, \code{all_of}, or \code{none_of}.
#'
#' \itemize{
#' \item{
#' \code{.check_chr_within()} throws an error if provided \code{within} vector
#' does not correspond to the \code{discriminator} with respect to \code{x}
#' parameter (e.g. "one of x within...", "all of x within...).
#' \code{one_of}: only one value (can repeat) of \code{x} appears
#' in \code{within} vector. \code{any_of}: at least one value (can
#' repeat) of \code{x} appears in \code{within} vector. \code{all_of}
#' (default): all values (can repeat) of \code{x} appears in \code{within}
#' vector. \code{none_of}: no value of \code{x} is in \code{within} vector.
#' }
#' \item{
#' \code{.check_chr_contains()} throws an error if provided \code{x}
#' vector does not correspond to the \code{discriminator} with respect to
#' \code{contains} parameter (e.g. "x contains one of...",
#' "x contains all of..."). \code{one_of}: only one value (can repeat) of
#' \code{contains} appears in \code{x} vector. \code{any_of}: at least one
#' value (can repeat) of \code{contains} appears in \code{x} vector.
#' \code{all_of} (default): all values (can repeat) of \code{contains}
#' appears in \code{x} vector. \code{none_of}: no value of \code{contains} is
#' in \code{x} vector.
#' }
#' }
#'
.check_chr_within <- function(x,
                              within, ...,
                              case_sensitive = TRUE,
                              discriminator = "all_of",
                              msg = NULL) {

    # pre-condition
    .check_chr(within, len_min = 1,
               msg = "invalid 'within' parameter")

    # allowed discriminators and its print values
    discriminators <- c("one_of" = "one of", "any_of" = "at least one of",
                        "all_of" = "all of", "none_of" = "none of")

    if (length(discriminator) != 1 ||
        !discriminator %in% names(discriminators))
        stop(paste(".check_chr_within: discriminator should be one of",
                   "'one_of', 'any_of', 'all_of', or 'none_of'."),
             call. = TRUE)

    # check type
    .check_chr_type(x, msg = msg)

    res <- x

    # simplify
    x <- unique(x)
    within <- unique(within)

    # transform inputs to verify without case sensitive
    if (!case_sensitive) {
        x <- tolower(x)
        within <- tolower(within)
    }

    # prepare local message
    if (length(within) > 1)
        local_msg <- sprintf("value should be %s: %s",
                             discriminators[[discriminator]],
                             paste0("'", within, "'", collapse = ", "))
    else
        local_msg <- sprintf("value should be %s",
                             paste0("'", within, "'"))

    # check discriminator
    if (discriminator == "one_of")
        .check_that(
            sum(x %in% within) == 1,
            local_msg = local_msg,
            msg = msg
        )
    else if (discriminator == "any_of")
        .check_that(
            any(x %in% within),
            local_msg = local_msg,
            msg = msg
        )
    else if (discriminator == "all_of")
        .check_that(
            all(x %in% within),
            local_msg = local_msg,
            msg = msg
        )
    else if (discriminator == "none_of")
        .check_that(
            !any(x %in% within),
            local_msg = local_msg,
            msg = msg
        )

    return(invisible(res))
}

#' @rdname check_functions
.check_chr_contains <- function(x,
                                contains, ...,
                                case_sensitive = TRUE,
                                discriminator = "all_of",
                                msg = NULL) {

    # pre-condition
    .check_chr(contains, len_min = 1,
               msg = "invalid 'contains' parameter")

    # allowed discriminators and its print values
    discriminators <- c("one_of" = "only one of", "any_of" = "at least one of",
                        "all_of" = "all of")

    if (length(discriminator) != 1 ||
        !discriminator %in% names(discriminators))
        stop(paste(".check_chr_contains: discriminator should be one of",
                   "'one_of', 'any_of', or 'all_of'."),
             call. = TRUE)

    # check type
    .check_chr_type(x, msg = msg)

    res <- x

    # simplify
    x <- unique(x)
    contains <- unique(contains)

    # transform inputs to verify without case sensitive
    if (!case_sensitive) {
        x <- tolower(x)
        contains <- tolower(contains)
    }

    # prepare local message
    if (length(contains) > 1)
        local_msg <- sprintf("value should contain %s: %s",
                             discriminators[[discriminator]],
                             paste0("'", contains, "'", collapse = ", "))
    else
        local_msg <- sprintf("value should contain %s",
                             paste0("'", contains, "'"))

    # check discriminator
    if (discriminator == "one_of")
        .check_that(
            sum(contains %in% x) == 1,
            local_msg = local_msg,
            msg = msg
        )
    else if (discriminator == "any_of")
        .check_that(
            any(contains %in% x),
            local_msg = local_msg,
            msg = msg
        )
    else if (discriminator == "all_of")
        .check_that(
            all(contains %in% x),
            local_msg = local_msg,
            msg = msg
        )
    else if (discriminator == "none_of")
        .check_that(
            !any(contains %in% x),
            local_msg = local_msg,
            msg = msg
        )

    return(invisible(res))
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
.check_file <- function(x, ...,
                        msg = NULL) {

    # check type
    .check_chr(x, allow_empty = FALSE, len_min = 1,
               allow_null = FALSE, msg = msg)

    existing_files <- file.exists(x)
    .check_that(
        all(existing_files),
        local_msg = sprintf("file %s does not exist",
                            paste0("'", x[!existing_files], "'",
                                   collapse = ", ")),
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
.check_env_var <- function(x, ...,
                           msg = NULL) {

    .check_null(x, msg = msg)

    .check_chr_type(x, msg = msg)

    if (length(x) > 0)
        .check_apply(
            x,
            fn_check = function(x) .check_that(x = nzchar(Sys.getenv(x)),
                                               msg = paste(sprintf("%s: ", x),
                                                           msg))
        )
    else
        .check_that(x = nzchar(Sys.getenv(x)), msg = msg)

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
.check_warn <- function(expr) {

    res <- tryCatch({
        expr
    }, error = function(e) {
        warning(e$message, call. = FALSE)
    })

    return(invisible(res))
}

#' @rdname check_functions
.check_error <- function(expr, ...,
                         msg = NULL) {

    res <- tryCatch({
        expr
    }, error = function(e) {
        .check_that(FALSE, local_msg = e$message, msg = msg)
    })

    return(invisible(res))
}
