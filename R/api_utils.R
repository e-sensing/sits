#' @title Data type functions
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' These are a short named version of data type functions.
#'
#' @param x           Input value.
#' @param column_name Character with column name
#' @param ... Additional parameters.
#'
#' @examples
#' if (sits_run_examples()) {
#'     .as_int(1.234)
#'     .as_dbl(42L)
#'     x <- 1.234
#'     .as_int(x) == x # x is not integer
#'     x <- 42.0
#'     .as_int(x) == x # x is an integer
#'     .as_chr(x)
#'     .as_date(list("2020-01-01", "2022-12-01"))
#'     .has(list()) # FALSE
#'     .has(NULL) # FALSE
#'     .has(c()) # FALSE
#'     .has(FALSE) # TRUE
#'     .set_class(list(), "new_class")
#'     .compact(c(1, 2, 3)) # 1 2 3
#'     .compact(c(1, 1, 1)) # 1
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

#' @title Check if variable has been defined. Any zero length
#'   value of any type is evaluated as \code{FALSE}. This function is broader
#'   than \code{is.null()} that only accounts for \code{NULL} value.
#'   Returns \code{logical}.
#' @noRd
.has <- function(x) {
    length(x) > 0L
}
#' @title Check if variable has not been defined. Any zero length
#'   value of any type is evaluated as \code{FALSE}. This function is broader
#'   than \code{is.null()} that only accounts for \code{NULL} value.
#'   Returns \code{logical}.
#' @noRd
.has_not <- function(x) {
    !.has(x)
}

.has_cloud <- function(bands) {
    .source_cloud() %in% bands
}

#' @title Check if an input has names or not. If there is
#'   any element without a name the function evaluates as \code{FALSE}.
#'   Returns \code{logical}.
#' @noRd
.has_name <- function(x) {
    if (.has(names(x))) {
        return(names(x) != "")
    }
    rep(FALSE, length(x))
}

#' @title Check if an input has column name or not. If there is
#'   the function evaluates as \code{TRUE}.
#'   Returns \code{logical}.
#' @noRd
.has_column <- function(x, column_name) {
    any(.has_name(x)) && column_name %in% names(x)
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
    if (length(value) != 1L) {
        return(x)
    }
    value
}

.dissolve <- function(x) {
    unique(unlist(x, recursive = FALSE, use.names = FALSE))
}

#' @title Handling error
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' This is a implementation of \code{tryCatch()}. It
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
#'     .try(
#'         {
#'             file <- tempfile("test.txt")
#'             cat(letters, file = file)
#'             cat(letters[["a"]], file = file, append = TRUE) # error!
#'         },
#'         .rollback = {
#'             unlink(file) # delete file before error is thrown
#'         }
#'     )
#'
#'     value <- .try(
#'         {
#'             addr <- url("http://example.com/")
#'             open(addr)
#'             readLines(addr)
#'             "You have access to the internet!" # don't use return()!
#'         },
#'         .default = {
#'             "You do not have access to the internet!" # bypass any error!
#'         },
#'         .finally = {
#'             close(addr) # close connection before exit (with error or not)
#'         }
#'     )
#'     print(value)
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
                e[["message"]]
            })
        }
    )
}
#' @title Discards cols in data
#' @noRd
#' @param data  Data.frame or matrix
#' @param cols  Column names to be discarded
#' @returns Data without cols
.discard <- function(data, cols) {
    cols <- which(names(data) %in% cols)
    if (.has(cols)) {
        data <- data[-cols]
    }
    # Return data
    data
}
#' @title Apply function in data column
#' @noRd
#' @param data  Data.frame or matrix
#' @param col   Column names to be used for function
#' @param fn    Function to be applied
#' @param ...   Generic entries
#' @returns Data with function applied
.by <- function(data, col, fn, ...) {
    # precondition
    .check_set_caller(".by")
    .check_chr_within(col,
        within = names(data),
        discriminator = "any_of"
    )

    unname(c(by(data, data[[col]], fn, ...)))
}
#' @title Check value is between max and min
#' @noRd
#' @param x     Value
#' @param min   Minimum reference value
#' @param max   Maximum reference value
#' @returns TRUE/FALSE
.between <- function(x, min, max) {
    min <= x & x <= max
}
#' @title Calculate partitions in vector
#' @noRd
#' @param x     Data vector
#' @param n     Number of partitions
#' @returns Vector with indexes for partitions
.partitions <- function(x, n) {
    n <- max(1L, min(length(x), n))
    .as_int(round(seq.int(from = 1L, to = n, length.out = length(x))))
}
#' @title Collapse
#' @noRd
#' @param ...   Generic entries (character vectors)
#' @returns Single character vectors
.collapse <- function(...) {
    toString(...)
}
#' @title Return default value
#' @noRd
#' @param x     R object
#' @param default     Default value
#' @returns Default value if x is NULL
.default <- function(x, default = NULL) {
    if (!all(is.na(x)) && .has(x)) {
        return(x)
    }
    default
}

#' @title Return prepared value if X is not NULL
#' @noRd
#' @param x         R object
#' @param prepare   Prepared value
#' @param default   Default value
#' @returns Prepared value if x is not NULL
.prepare_null <- function(x, prepare, default) {
    if (!all(is.na(x)) && .has(x)) {
        return(prepare)
    }
    default
}

#' @title Return prepared value if X is TRUE
#' @noRd
#' @param x         R object
#' @param prepare   Prepared value
#' @param default   Default value
#' @returns Prepared value if x is TRUE
.prepare_lgl <- function(x, prepare, default) {
    if (.has(x) && x) {
        return(prepare)
    }
    default
}

#' @title Create a tibble from a vector
#' @noRd
#' @param ...   Generic entries
#' @returns Default value if x is NULL
.common_size <- function(...) {
    tibble::tibble(...)
}
#' @title Get i-th element of data.frame x
#' @noRd
#' @param x     Data.frame
#' @param i     Row index
.slice_dfr <- function(x, i) {
    UseMethod(".slice_dfr", i)
}
#' @export
.slice_dfr.numeric <- function(x, i) {
    # set caller to show in errors
    .check_set_caller(".slice_dfr_numeric")
    .check_that(all(i <= nrow(x)))
    x[i, ]
}
#' @title       Function that returns a data frame
#' @description Generates a row-wise tibble from the function applied
#'   to each element of list
#' @noRd
#' @param x     A list of elements to apply to the function
#' @param fn    A function that receives an element and return a tibble
#' @param ...   Additional parameters to the function
#' @returns A tibble
.map_dfr <- function(x, fn, ...) {
    purrr::list_rbind(lapply(x, fn, ...))
}
#' @title       Function that returns a data frame
#' @description Generates a column-wise tibble from the function applied
#'   to each element of list
#' @noRd
#' @param x     A list of elements to apply to the function
#' @param fn    A function that receives an element and return a tibble
#' @param ...   Additional parameters to the function
#' @returns A tibble
.map_dfc <- function(x, fn, ...) {
    purrr::list_cbind(lapply(x, fn, ...))
}
#' @title       Function that returns a random subdirectory of tempdir()
#' @description Generates a random subdir
#' @noRd
#' @keywords internal
#' @returns  Name of a valid subdir of tempdir()
#'
.rand_sub_tempdir <- function() {
    new_dir <- FALSE
    while (!new_dir) {
        new_temp_dir <- file.path(tempdir(), sample.int(10000L, size = 1L))
        if (!dir.exists(new_temp_dir)) {
            dir.create(new_temp_dir)
            new_dir <- TRUE
        }
    }
    new_temp_dir
}
