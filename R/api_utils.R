#' @title Data type functions
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These are a short named version of data type functions.
#'
#' @param x Input value.
#' @param ... Additional parameters.
#'
#' @examples
#' if (sits_run_examples()) {
#' .as_int(1.234)
#' .as_dbl(42L)
#' x <- 1.234
#' .as_int(x) == x # x is not integer
#' x <- 42.0
#' .as_int(x) == x # x is an integer
#' .as_chr(x)
#' .as_date(list("2020-01-01", "2022-12-01"))
#' .has(list()) # FALSE
#' .has(NULL) # FALSE
#' .has(c()) # FALSE
#' .has(FALSE) # TRUE
#' .set_class(list(), "new_class")
#' .compact(c(1, 2, 3)) # 1 2 3
#' .compact(c(1, 1, 1)) # 1
#' }
#'
NULL

#' @title Convert an input to \code{integer}.
#' @noRd
#' @returns An integer or `NULL` if value is empty.
.as_int <- function(x) {
    .default(as.integer(x))
}

#' @title Convert an input to \code{character}.
#'   Returns \code{character} or \code{NULL} if value is empty.
#' @noRd
.as_chr <- function(x) {
    .default(as.character(x))
}

#' @title Convert an input to \code{numeric}.
#'   Returns \code{numeric} or \code{NULL} if value is empty.
#' @noRd
.as_dbl <- function(x) {
    .default(as.numeric(x))
}

#' @title Convert an input to \code{Date}.
#'   Returns \code{Date} or \code{NULL} if value is empty.
#' @noRd
.as_date <- function(x) {
    .default(lubridate::as_date(x))
}

#' @title Check if an input has a value or not. Any zero length
#'   value of any type is evaluated as \code{FALSE}. This function is broader
#'   than \code{is.null()} that only accounts for \code{NULL} value.
#'   Returns \code{logical}.
#' @noRd
.has <- function(x) {
    length(x) > 0
}

#' @title Check if an input has names or not. If there is
#'   any element without a name the function evaluates as \code{FALSE}.
#'   Returns \code{logical}.
#' @noRd
.has_name <- function(x) {
    if (.has(names(x))) return(names(x) != "")
    rep(FALSE, length(x))
}

#' @title Set \code{class} of object \code{x}.
#'   Returns updated \code{x} object.
#' @noRd
.set_class <- function(x, ...) {
    class(x) <- unique(c(...))
    x
}

#' @title Evaluates unique values of \code{x}. If there is
#'   only one unique value, return it. Otherwise return all \code{x}.
#'   Returns same value as \code{x} or the unique value in \code{x} (if
#'   this is the case).
#' @noRd
.compact <- function(x) {
    value <- unique(x)
    if (length(value) != 1) {
        return(x)
    }
    value
}

#' @title Handling error
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' This is a fancy implementation of \code{tryCatch()}. It
#' has a shorter name and provide a easy functionality of rolling back
#' (run an expression in case of error, but not avoiding it),
#' of default value (run expression in case of error bypassing it).
#' Customized error messages can be passed to \code{msg_error} param.
#'
#' The order of execution is the following:
#' (1) try evaluate \code{expr};
#' (2) if everything goes well, run step 6 and return the last expression
#'   evaluated in \code{expr} (end);
#' (3) if an error occurs in step 1, evaluate \code{.rollback} expression
#'   (if informed);
#' (4) if \code{.default} is not informed, run step 6 and throws
#'   the error (end);
#' (5) if \code{.default} is informed, evaluate it, run step 6, and
#'   return the last expression in \code{.default} (end);
#' (6) evaluate \code{.finally} (if informed).
#'
#' @param expr Expression to be evaluated.
#' @param ... Additional parameter to be passed to \code{tryCatch()}.
#' @param .rollback Expression to run in case of error.
#' @param .default Expression to evaluate and return in case of error
#'   (setting this parameter avoids error raising).
#' @param .msg_error An optional customized error message.
#' @param .finally An optional expression to run before exit function
#'   (with error or not).
#'
#' @examples
#' if (sits_run_examples()) {
#' .try({
#'   file <- tempfile("test.txt")
#'   cat(letters, file = file)
#'   cat(letters[["a"]], file = file, append = TRUE) # error!
#' },
#' .rollback = {
#'   unlink(file) # delete file before error is thrown
#' })
#'
#' value <- .try({
#'   addr <- url("http://example.com/")
#'   open(addr)
#'   readLines(addr)
#'   "You have access to the internet!" # don't use return()!
#' },
#' .default = {
#'   "You do not have access to the internet!" # bypass any error!
#' },
#' .finally = {
#'   close(addr) # close connection before exit (with error or not)
#' })
#' print(value)
#' }
#'
#' @returns Last expression evaluated in \code{expr}, if no error occurs.
#'   If an error occurs, the function returns the last expression
#'   evaluated in \code{.default} parameter. If \code{.default} parameter
#'   is not informed, the function will raise the error.
#'
.try <- function(expr,
                 ...,
                 .rollback = NULL,
                 .default = NULL,
                 .msg_error = NULL,
                 .finally = NULL) {
    has_default <- !missing(.default)
    if (!missing(.finally)) on.exit(.finally)
    tryCatch(
        expr,
        ...,
        error = function(e) {
            if (!is.null(.rollback)) {
                .rollback
            }
            if (has_default) {
                return(.default)
            }
            stop(if (!is.null(.msg_error)) {
                .msg_error
            } else {
                e$message
            })
        }
    )
}

.rbind <- function(x) {
    do.call(rbind, args = x)
}

.discard <- function(data, cols) {
    cols <- which(names(data) %in% cols)
    if (.has(cols)) {
        data <- data[-cols]
    }
    # Return data
    data
}

.by <- function(data, col, fn, ...) {
    if (!col %in% names(data)) {
        stop("invalid 'col' parameter: '", col, "' not found in data columns")
    }
    unname(c(by(data, data[[col]], fn, ...)))
}

.by_dfr <- function(data, col, fn, ...) {
    .rbind(.by(data, col, fn, ...))
}

.between <- function(x, min, max) {
    min <= x & x <= max
}

.partitions <- function(x, n) {
    n <- max(1, min(length(x), n))
    .as_int(round(seq.int(from = 1, to = n, length.out = length(x))))
}

.collapse <- function(...) {
    paste0(..., collapse = ", ")
}

.default <- function(x, default = NULL) {
    if (.has(x)) return(x)
    default
}

.common_size <- function(...) {
    tibble::tibble(...)
}
