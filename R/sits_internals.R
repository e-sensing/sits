#---- data type ----

#' Data type functions
#'
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
#' @returns See description of each function.
#'
#' @family data types
#' @keywords internal
#' @name data_type
NULL

#' @describeIn data_type Convert an input to \code{integer}.
#'   Returns \code{integer} or \code{NULL} if value is empty.
.as_int <- function(x) {
    if (.has(x)) as.integer(x) else NULL
}
#' @describeIn data_type Convert an input to \code{character}.
#'   Returns \code{character} or \code{NULL} if value is empty.
.as_chr <- function(x) {
    if (.has(x)) as.character(x) else NULL
}

#' @describeIn data_type Convert an input to \code{numeric}.
#'   Returns \code{numeric} or \code{NULL} if value is empty.
.as_dbl <- function(x) {
    if (.has(x)) as.numeric(x) else NULL
}

#' @describeIn data_type Convert an input to a date type. This is
#'   the same function as \code{lubridate::as_date()}.
#'   Returns \code{date} or \code{NULL} if value is empty.
.as_date <- function(x) {
    if (.has(x)) lubridate::as_date(unlist(x, recursive = FALSE)) else NULL
}

#' @describeIn data_type Check if an input has a value or not. Any zero length
#'   value of any type is evaluated as \code{FALSE}. This function is broader
#'   than \code{is.null()} that only accounts for \code{NULL} value.
#'   Returns \code{logical}.
.has <- function(x) {
    length(x) > 0
}

#' @describeIn data_type Check if an input has names or not. If there is
#'   any element without a name the function evaluates as \code{FALSE}.
#'   Returns \code{logical}.
.has_name <- function(x) {
    if (.has(names(x))) return(names(x) != "")
    rep(FALSE, length(x))
}

#' @describeIn data_type Set \code{class} of object \code{x}.
#'   Returns updated \code{x} object.
.set_class <- function(x, ...) {
    class(x) <- unique(c(...))
    x
}

#' @describeIn data_type Evaluates unique values of \code{x}. If there is
#'   only one unique value, return it. Otherwise return all \code{x}.
#'   Returns same value as \code{x} or the unique value in \code{x} (if
#'   this is the case).
.compact <- function(x) {
    value <- unique(x)
    if (length(value) != 1) {
        return(x)
    }
    value
}


#' @title Handling error
#' @name .try
#' @keywords internal
#' @noRd
#' @description
#' This is a fancy implementation of \code{tryCatch()}. It
#' has a shorter name and provide a easy functionality of rolling back
#' (run an expression in case of error, but not avoiding it),
#' of default value (run expression in case of error bypassing it).
#' Customized error messages can be passed to \code{msg_error} param.
#'
#' The order of execution is the following:
#' \enumerate{
#' \item try evaluate \code{expr};
#' \item if everything goes well, run step 6 and return the last expression
#'   evaluated in \code{expr} (end);
#' \item if an error occurs in step 1, evaluate \code{.rollback} expression
#'   (if informed);
#' \item if \code{.default} is not informed, run step 6 and throws
#'   the error (end);
#' \item if \code{.default} is informed, evaluate it, run step 6, and
#'   return the last expression in \code{.default} (end);
#' \item evaluate \code{.finally} (if informed).
#' }
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
#' @seealso \code{\link[base]{tryCatch}}
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

.by <- function(data, col, fn, ...) {
    unname(c(by(data, data[[col]], fn, ...)))
}

.by_dfr <- function(data, col, fn, ...) {
    .rbind(.by(data, col, fn, ...))
}

.by_lgl <- function(data, col, fn, ...) {
    vapply(.by(data, col, fn, ...), c, logical(1))
}

.between <- function(x, min, max) {
    min <= x & x <= max
}

.partitions <- function(x, n) {
    n <- max(1, min(length(x), n))
    .as_int(round(seq.int(from = 1, to = n, length.out = length(x))))
}




#  Band configuration accessors
#
#  These functions are read-only accessors of band_conf objects. A
#  band_conf is an entry of band definition in config. It can be associated
#  to an eo_cube or derived_cube
#'

#'

#' @name .data_type
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return data type associated to the configuration
.data_type <- function(conf) {
    .as_chr(conf[["data_type"]][[1]])
}

#' @name .miss_value
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return  missing value associated to the band
.miss_value <- function(conf) {
    .as_dbl(conf[["missing_value"]][[1]])
}
#' @name .min_value
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return  minimum value associated to the band
.min_value <- function(conf) {
    .as_dbl(conf[["minimum_value"]][[1]])
}

#' @name .max_value
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return  maximum value associated to the band
.max_value <- function(conf) {
    .as_dbl(conf[["maximum_value"]][[1]])
}

#' @name .scale
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return  scale factor associated to the band
.scale <- function(conf) {
    .as_dbl(conf[["scale_factor"]][[1]])
}

#' @name .offset
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return  offset value associated to the band
.offset <- function(conf) {
    .as_dbl(conf[["offset_value"]][[1]])
}
#' @name .cloud_interp_values
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return  cloud interpolation values associated to the band
.cloud_interp_values <- function(conf) {
    .as_int(conf[["interp_values"]])
}

#' @name .cloud_bit_mask
#' @noRd
#' @param conf A band definition value from config. Can be retrieved by
#'   .conf_eo_band() or .conf_derived_band().
#' @return  cloud bit maks values associated to the band.
.cloud_bit_mask <- function(conf) {
    .as_int(conf[["bit_mask"]][[1]])
}
