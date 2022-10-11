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

#----  Utility functions ----

#' Handling error
#'
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
#' @family utility functions
#' @keywords internal
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

#' Spatial intersects
#'
#' This function is based on \code{sf::st_intersects()}. It projects \code{y}
#' to the CRS of \code{x} before compute intersection. For each geometry of
#' \code{x}, the function returns the \code{TRUE} if it intersects with any
#' geometry of \code{y}, otherwise it returns \code{FALSE}.
#'
#' @param x,y \code{sf} geometries.
#'
#' @returns A \code{logical} vector indicating which geometries of \code{x}
#' intersects \code{y} geometries.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- .bbox_as_sf(c(xmin=1, xmax=2, ymin=3, ymax=4, crs=4326))
#' y <- .roi_as_sf(c(lon_min=1.5, lon_max=3, lat_min=3.5, lat_max=5))
#' .intersects(x, y) # TRUE
#' }
#'
#' @family utility functions
#' @family region objects API
#' @keywords internal
.intersects <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(sf::st_intersects(x, y, sparse = FALSE), 1, any)
}

.between <- function(x, min, max) {
    min <= x & x <= max
}

.partitions <- function(x, n) {
    n <- max(1, min(length(x), n))
    .as_int(round(seq.int(from = 1, to = n, length.out = length(x))))
}

#---- Generic accessors ----

#' Bbox accessors
#'
#' These functions are accessors of \code{bbox} fields of a \code{vector}.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the \code{vector} is not \code{atomic}, it is removed from the object.
#'
#' \code{.xmin()}, \code{.xmax()}, \code{.ymin()}, \code{.ymax()},
#' and \code{.crs()} get/set, respectively, \code{"xmin"}, \code{"xmax"},
#' \code{"ymin"}, \code{"ymax"}, and \code{"crs"} fields.
#'
#' @param x Object to get/set field value.
#' @param value Value to set on object field.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- c(xmax = "123")
#' .xmax(x) # 123 as number
#' x <- list(xmin = 1, xmax = 2, ymin = 3, ymax = 4)
#' .crs(x) <- 4326
#' x # with 'crs' field
#' .as_crs(3857) # EPSG:3857
#' }
#'
#' @returns Getters return respective field value or \code{NULL}, if it doesn't
#'   exist. Setters return the updated \code{x} object.
#'
#' @family accessors
#' @keywords internal
#' @name bbox_accessors
NULL

#' @describeIn bbox_accessors Get \code{'xmin'} field.
.xmin <- function(x) {
    .as_dbl(.compact(x[["xmin"]]))
}

#' @describeIn bbox_accessors Set \code{'xmin'} field as numeric.
`.xmin<-` <- function(x, value) {
    x[["xmin"]] <- .as_dbl(value)
    x
}

#' @describeIn bbox_accessors Get \code{'xmax'} field.
.xmax <- function(x) {
    .as_dbl(.compact(x[["xmax"]]))
}

#' @describeIn bbox_accessors Set \code{'xmax'} field as numeric.
`.xmax<-` <- function(x, value) {
    x[["xmax"]] <- .as_dbl(value)
    x
}

#' @describeIn bbox_accessors Get \code{'ymin'} field.
.ymin <- function(x) {
    .as_dbl(.compact(x[["ymin"]]))
}

#' @describeIn bbox_accessors Set \code{'ymin'} field as numeric.
`.ymin<-` <- function(x, value) {
    x[["ymin"]] <- .as_dbl(value)
    x
}

#' @describeIn bbox_accessors Get \code{'ymax'} field.
.ymax <- function(x) {
    .as_dbl(.compact(x[["ymax"]]))
}

#' @describeIn bbox_accessors Set \code{'ymax'} field as numeric.
`.ymax<-` <- function(x, value) {
    x[["ymax"]] <- .as_dbl(value)
    x
}

#' @describeIn bbox_accessors Convert a CRS numeric value to \code{character},
#'   appending it after \code{'EPSG:'}.
.as_crs <- function(x) {
    if (.has(x)) {
        if (is.character(x))
            .compact(x)
        else if (is.numeric(x))
            paste0("EPSG:", .compact(x))
        else if (is.na(x))
            NA_character_
        else
            stop("invalid crs value")
    }
}

#' @describeIn bbox_accessors Get \code{'crs'} field.
.crs <- function(x) {
    .as_crs(x[["crs"]])
}

#' @describeIn bbox_accessors Set \code{'crs'} field as \code{character} string.
`.crs<-` <- function(x, value) {
    x[["crs"]] <- .as_crs(value)
    x
}

#' Block accessors
#'
#' These functions are accessors of \code{block} fields in a \code{vector}.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the \code{vector} is not \code{atomic}, it is removed from the object.
#'
#' \code{.col()}, \code{.row()}, \code{.ncols()}, and \code{.nrows()} get/set,
#' respectively, \code{"col"}, \code{"row"}, \code{"ncols"}, and
#' \code{"nrows"} fields.
#'
#' @param x Object to get field value.
#' @param value Value to set on object field.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- c(row = 3.45)
#' .row(x) # 3 as integer
#' x <- list(col = 1, row = 2, ncols = 3)
#' .nrows(x) # NULL
#' .nrows(x) <- 4
#' x
#' }
#'
#' @returns Getters return respective field value or \code{NULL}, if it doesn't
#'   exist. Setters return the updated \code{x} object.
#'
#' @family accessors
#' @keywords internal
#' @name block_accessors
NULL

#' @describeIn block_accessors Get \code{'col'} field.
.col <- function(x) {
    .as_int(.compact(x[["col"]]))
}

#' @describeIn block_accessors Set \code{'col'} field as integer.
`.col<-` <- function(x, value) {
    x[["col"]] <- .as_int(value)
    x
}

#' @describeIn block_accessors Get \code{'row'} field.
.row <- function(x) {
    .as_int(.compact(x[["row"]]))
}

#' @describeIn block_accessors Set \code{'row'} field as integer.
`.row<-` <- function(x, value) {
    x[["row"]] <- .as_int(value)
    x
}

#' @describeIn block_accessors Get \code{'ncols'} field.
.ncols <- function(x) {
    .as_int(.compact(x[["ncols"]]))
}

#' @describeIn block_accessors Set \code{'ncols'} field as integer.
`.ncols<-` <- function(x, value) {
    x[["ncols"]] <- .as_int(value)
    x
}

#' @describeIn block_accessors Get \code{'nrows'} field.
.nrows <- function(x) {
    .as_int(.compact(x[["nrows"]]))
}

#' @describeIn block_accessors Set \code{'nrows'} field as integer.
`.nrows<-` <- function(x, value) {
    x[["nrows"]] <- .as_int(value)
    x
}

#' Chunk accessors
#'
#' These functions are read-only accessors of \code{chunk} fields in a
#' \code{vector}.
#'
#' \code{.xres()} and \code{.yres()} computes, respectively, \code{"xres"} and
#' \code{"yres"} values from chunk fields. The values are computed as
#' \itemize{
#' \item \eqn{xres=(xmax - xmin)/ncols}
#' \item \eqn{yres=(ymax - ymin)/nrows}
#' }
#'
#' @param x Object to get field value.
#' @param value Value to set on object field.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- c(nrows = 100, ymin = 1, ymax = 10)
#' .yres(x) # 0.09
#' }
#'
#' @returns Spatial resolution.
#'
#' @family accessors
#' @keywords internal
#' @name chunk_accessors
NULL

#' @describeIn chunk_accessors Computes \code{x} resolution of a \code{chunk}.
.xres <- function(x) {
    (.xmax(x) - .xmin(x)) / .ncols(x)
}

#' @describeIn chunk_accessors Computes \code{y} resolution of a \code{chunk}.
.yres <- function(x) {
    (.ymax(x) - .ymin(x)) / .nrows(x)
}

#' Band configuration accessors
#'
#' These functions are read-only accessors of \code{band_conf} objects. A
#' \code{band_conf} is an entry of band definition in config. It can be
#' from an \code{eo_cube} or \code{derived_cube}.
#'
#' @param conf A band definition value from config. Can be retrieved by
#'   \code{.conf_eo_band()} or \code{.conf_derived_band()}.
#'
#' @examples
#' if (sits_run_examples()) {
#' # Get configuration band
#' x <- .conf_eo_band(
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   band = "NIR"
#' )
#'
#' .
#' }
#'
#' @returns Respective configuration value.
#'
#' @family accessors
#' @keywords internal
#' @name band_accessors
NULL

#' @describeIn band_accessors Get \code{data_type} entry.
.data_type <- function(conf) {
    .as_chr(conf[["data_type"]][[1]])
}

#' @describeIn band_accessors Get \code{missing_value} entry.
.miss_value <- function(conf) {
    .as_dbl(conf[["missing_value"]][[1]])
}

#' @describeIn band_accessors Get \code{minimum_value} entry.
.min_value <- function(conf) {
    .as_dbl(conf[["minimum_value"]][[1]])
}

#' @describeIn band_accessors Get \code{maximum_value} entry.
.max_value <- function(conf) {
    .as_dbl(conf[["maximum_value"]][[1]])
}

#' @describeIn band_accessors Get \code{scale_factor} entry.
.scale <- function(conf) {
    .as_dbl(conf[["scale_factor"]][[1]])
}

#' @describeIn band_accessors Get \code{offset_value} entry.
.offset <- function(conf) {
    .as_dbl(conf[["offset_value"]][[1]])
}

#' @describeIn band_accessors Get \code{interp_values} entry.
.cloud_interp_values <- function(conf) {
    .as_int(conf[["interp_values"]])
}

#' @describeIn band_accessors Get \code{bit_mask} entry.
.cloud_bit_mask <- function(conf) {
    .as_int(conf[["bit_mask"]][[1]])
}
