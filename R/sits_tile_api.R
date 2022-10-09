#---- data type ----

#' Data type functions
#'
#' These are a short named version of data type functions.
#'
#' @param x Input value.
#' @param ... Additional parameters.
#'
#' @examples
#' \dontrun{
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
    .default(as.integer(x))
}
#' @describeIn data_type Convert an input to \code{character}.
#'   Returns \code{character} or \code{NULL} if value is empty.
.as_chr <- function(x) {
    .default(as.character(x))
}

#' @describeIn data_type Convert an input to \code{numeric}.
#'   Returns \code{numeric} or \code{NULL} if value is empty.
.as_dbl <- function(x) {
    .default(as.numeric(x))
}

#' @describeIn data_type Convert an input to a date type. This is
#'   the same function as \code{lubridate::as_date()}.
#'   Returns \code{date} or \code{NULL} if value is empty.
.as_date <- function(x) {
    .default(lubridate::as_date(unlist(x, recursive = FALSE)))
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
#' \dontrun{
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
    if (.has(.finally)) on.exit(.finally, add = TRUE)
    tryCatch(
        expr,
        ...,
        error = function(e) {
            if (.has(.rollback)) {
                .rollback
            }
            if (has_default) {
                return(.default)
            }
            stop(if (.has(.msg_error)) {
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
#' \dontrun{
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

.collapse <- function(...) {
    paste0(..., collapse = ", ")
}

.default <- function(x, default = NULL) {
    if (.has(x)) return(x)
    default
}

.common_size <- function(...) {
    c(data.frame(...))
}

#' period API
#'
#' According to ISO-8601 a duration is the amount of intervening time
#' in a time interval. Here, we use a simplified representation of a duration
#' that we call \code{period}.
#'
#' \code{period} is represented by the the regular expression
#' \code{^P[0-9]+[DMY]$}. \code{P} is the period designator placed at the
#' start of the string. \code{[0-9]+} is the integer value and is followed
#' by a \code{period} unit: \code{Y} is the year designator, \code{M} is the
#' month designator, and \code{D} is the day designator.
#'
#' @param period A \code{character}.
#'
#' @examples
#' \dontrun{
#' .period_check("P16D") # valid
#' .period_check("P1M10D") # error: invalid period format
#' .period_val("P16D") # 16
#' .period_val("P2M") # 2
#' .period_val("P1Y") # 1
#' .period_unit("P16D") # day
#' .period_unit("P2M") # month
#' .period_unit("P1Y") # year
#' }
#'
#' @family data types
#' @keywords internal
#' @name period_api
NULL

#' @describeIn period_api Check if a character string is a valid
#' \code{period}.
#'
#' @returns \code{.period_check()}: nothing.
.period_check <- function(period) {
    if (!grepl("^P[0-9]+[DMY]$", period)) {
        stop("invalid period format")
    }
}

#' @describeIn period_api Return the value part of a
#' \code{period}.
#'
#' @returns \code{.period_val()}: numeric value of a period.
.period_val <- function(period) {
    .period_check(period)
    .as_dbl(gsub("^P([0-9]+)[DMY]$", "\\1", period))
}

#' @describeIn period_api Return the unit of a \code{period}.
#' Can be one of \code{'day'}, \code{'month'}, or \code{'year'}.
#'
#' @returns \code{.period_unit()}: description of unit of a period.
.period_unit <- function(period) {
    .period_check(period)
    unit <- c(D = "day", M = "month", Y = "year")
    unit[[gsub("^P[0-9]+([DMY])$", "\\1", period)]]
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
#' @param default_crs Default crs value if object doesn't have one.
#'
#' @examples
#' \dontrun{
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
.crs <- function(x, default_crs = NULL) {
    if ("crs" %in% names(x)) {
        .as_crs(x[["crs"]])
    } else {
        default_crs
    }
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
#' \dontrun{
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
#' \dontrun{
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
#' \dontrun{
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

#' Point accessors
#'
#' These functions are accessors of \code{point} fields of a \code{vector}.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the \code{vector} is not \code{atomic}, it is removed from the object.
#'
#' \code{.lon()} and \code{.lat()} get/set, respectively, \code{"longitude"}
#' and \code{"latitude"} fields.
#'
#' @param x Object to get/set field value.
#' @param value Value to set on object field.
#'
#' @examples
#' \dontrun{
#' x <- c(longitude = "123")
#' .lon(x) # 123 as number
#' x <- list(longitude = 1:10)
#' .lat(x) <- 11:20
#' x # with 'longitude' and 'latitude' fields
#' }
#'
#' @returns Getters return respective field value or \code{NULL}, if it doesn't
#'   exist. Setters return the updated \code{x} object.
#'
#' @family accessors
#' @keywords internal
#' @name point_accessors
NULL

#' @describeIn point_accessors Get \code{'longitude'} field.
.lon <- function(x) {
    .as_dbl(.compact(x[["longitude"]]))
}

#' @describeIn point_accessors Set \code{'longitude'} field as numeric.
`.lon<-` <- function(x, value) {
    x[["longitude"]] <- .as_dbl(value)
    x
}

#' @describeIn point_accessors Get \code{'latitude'} field.
.lat <- function(x) {
    .as_dbl(.compact(x[["latitude"]]))
}

#' @describeIn point_accessors Set \code{'latitude'} field as numeric.
`.lat<-` <- function(x, value) {
    x[["latitude"]] <- .as_dbl(value)
    x
}

#---- Block API: ----

#' Block API
#'
#' A block represents a region of a matrix. A \code{block} is any
#' \code{list} or \code{tibble} containing \code{col}, \code{row},
#' \code{ncols}, and \code{nrows} fields.
#'
#' \code{col} and \code{row} fields are optional. If not present, they
#' are assumed to be \code{1}.
#'
#' @param x Any object to extract a \code{block}.
#' @param block Any object with \code{ncols}, \code{nrows} fields.
#' @param overlap Pixels to increase/decrease block \code{ncols} and
#' \code{nrows}.
#'
#' @examples
#' \dontrun{
#' x <- list(a = 0, z = 0)
#' .block(x) # NULL
#' x <- list(a = 0, row = 2, ncols = 3, nrows = 4, z = 0)
#' .block(x) # col defaults to 1
#' .block_size(x, overlap = 0)
#' .block_size(x, overlap = 2)
#' }
#'
#' @seealso \link{block_accessors}
#' @family region objects API
#' @keywords internal
#' @name block_api
NULL

# block fields
.block_cols <- c("ncols", "nrows")

#' @describeIn block_api Does vector \code{x} has \code{block} fields?
#'
#' @returns \code{.has_block()}: \code{logical}.
.has_block <- function(x) {
    all(.block_cols %in% names(x))
}

#' @describeIn block_api Extract a \code{block} from any given
#' \code{vector}.
#'
#' @returns \code{.block()}: \code{block}.
.block <- function(x) {
    if (!.has_block(x)) {
        return(NULL)
    }
    col <- .default(x = .col(x), default = 1)
    row <- .default(x = .row(x), default = 1)
    # Return a block
    .common_size(col = col, row = row, ncols = .ncols(x), nrows = .nrows(x))
}

#' @describeIn block_api Compute the number of pixels for a
#' \code{block} considering an additional overlapping parameter.
#'
#' @returns \code{.block_size()}: \code{integer}.
.block_size <- function(block, overlap = 0) {
    (.nrows(block) + 2 * overlap) * (.ncols(block) + 2 * overlap)
}

#---- Bbox API: ----

#' Bbox API
#'
#' A bounding box represents a rectangular geographical region in a certain
#' projection. A \code{bbox} is any \code{list} or \code{tibble} containing
#' \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}, and \code{crs} fields.
#' A \code{bbox} may contains multiple entries.
#'
#' @param x Any object to extract a \code{bbox}.
#' @param ... Additional parameters.
#' @param default_crs If no CRS is present in \code{x}, which CRS should be
#' used? If \code{NULL} default CRS will be \code{'EPSG:4326'}.
#' @param bbox A \code{bbox}.
#' @param as_crs A CRS to project \code{bbox}. Useful if bbox has multiples
#' CRS.
#'
#' @examples
#' \dontrun{
#' x <- list(a = 0, z = 0)
#' .bbox(x) # NULL
#' x <- list(
#'   a = 0, xmin = 1:3, xmax = 2:4, ymin = 3:5, ymax = 4:6,
#'   crs = 4326, z = 0
#' )
#' .bbox(x)
#' .bbox_as_sf(x) # 3 features
#' .bbox_as_sf(x, as_crs = "EPSG:3857")
#' }
#'
#' @seealso \link{bbox_accessors}
#' @family region objects API
#' @keywords internal
#' @name bbox_api
NULL

# bbox fields
.bbox_cols <- c("xmin", "xmax", "ymin", "ymax")

#' @describeIn bbox_api Does vector \code{x} has \code{bbox} fields?
#'
#' @returns \code{.has_bbox()}: \code{logical}.
.has_bbox <- function(x) {
    all(.bbox_cols %in% names(x))
}

#' @describeIn bbox_api Extract a \code{bbox} from any given
#' \code{vector}.
#'
#' @returns \code{.bbox()}: \code{bbox}.
.bbox <- function(x, ..., default_crs = NULL) {
    if (!.has_bbox(x)) {
        return(NULL)
    }
    xmin <- .xmin(x)
    xmax <- .xmax(x)
    ymin <- .ymin(x)
    ymax <- .ymax(x)
    crs <- .crs(x, default_crs = .default(
        x = default_crs, default = {
        warning("object has no crs, assuming 'EPSG:4326'", call. = FALSE)
        "EPSG:4326"
    }))
    bbox <- .common_size(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, crs = crs
    )
    xmin <- pmin(.xmin(bbox), .xmax(bbox))
    xmax <- pmax(.xmin(bbox), .xmax(bbox))
    ymin <- pmin(.ymin(bbox), .ymax(bbox))
    ymax <- pmax(.ymin(bbox), .ymax(bbox))
    .xmin(bbox) <- xmin
    .xmax(bbox) <- xmax
    .ymin(bbox) <- ymin
    .ymax(bbox) <- ymax
    # Return bbox
    x
}

#' @describeIn bbox_api Convert a \code{bbox} into a
#' \code{sf} polygon object.
#'
#' @returns \code{.bbox_as_sf()}: \code{sf}.
.bbox_as_sf <- function(bbox, ..., default_crs = NULL, as_crs = NULL) {
    bbox <- .bbox(bbox, default_crs = default_crs)
    if (!.has(bbox)) {
        stop("object does not have a valid bbox")
    }
    # Check if there are multiple CRS in bbox
    if (length(.crs(bbox)) > 1 && !.has(as_crs)) {
        warning(
            "object has multiples crs values, reprojecting to ",
            "EPSG:4326\n", "(use 'as_crs' to reproject to a ",
            "different crs value)"
        )
        as_crs <- "EPSG:4326"
    }
    # Convert to sf object and return it
    purrr::pmap_dfr(bbox, function(xmin, xmax, ymin, ymax, crs) {
        geom <- sf::st_sf(
            geometry = sf::st_sfc(sf::st_polygon(list(rbind(
                c(xmin, ymax), c(xmax, ymax), c(xmax, ymin), c(xmin, ymin),
                c(xmin, ymax)
            )))), crs = crs
        )
        # Project CRS
        if (.has(as_crs)) {
            geom <- sf::st_transform(geom, crs = as_crs)
        }
        # Return geom
        geom
    })
}

#---- ROI API: ----

#' ROI API
#'
#' A Region of Interest (ROI) represents an geographic area. There are
#' three types of ROI objects, namely \code{sf} (from package \code{sf}),
#' \code{bbox} (from \code{.bbox()}), and \code{lonlat}.
#' A \code{lonlat} object is any \code{vector} containing \code{lon_min},
#' \code{lon_max}, \code{lat_min}, and \code{lat_max} fields. Its CRS is
#' defined to be \code{'EPSG:4326'}.
#'
#' @param roi A \code{roi}.
#' @param ... Parameters to be evaluated accordingly to \code{roi} type.
#' @param default_crs If no CRS is present in a \code{bbox} object passed
#' to \code{crs}, which CRS should be used? If \code{NULL} default CRS will
#' be \code{'EPSG:4326'}.
#'
#' @examples
#' \dontrun{
#' x <- list(lon_min = 1, lon_max = 2, lat_min = 3, lat_max = 4)
#' .roi_type(x) # lonlat
#' .roi_as_sf(x)
#' x <- list(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 3857)
#' .roi_type(x) # bbox
#' .roi_as_sf(x)
#' .roi_switch(
#'   x,
#'   lonlat = "It's in WGS 84",
#'   bbox = paste("It's in", .crs(x))
#' )
#' }
#'
#' @family region objects API
#' @keywords internal
#' @name roi_api
NULL

# roi 'lonlat' fields
.roi_lonlat_cols <- c("lon_min", "lon_max", "lat_min", "lat_max")

#' @describeIn roi_api Tells which type of ROI is in \code{roi}
#' parameter (One of \code{'sf'}, \code{'bbox'}, or \code{'lonlat'}).
#'
#' @returns \code{.roi_type()}: \code{character}.
.roi_type <- function(roi) {
    if (inherits(roi, c("sf", "sfc"))) {
        "sf"
    } else if (all(.bbox_cols %in% names(roi))) {
        "bbox"
    } else if (all(.roi_lonlat_cols %in% names(roi))) {
        "lonlat"
    } else {
        stop("invalid 'roi' parameter")
    }
}

#' @describeIn roi_api Chooses one of the arguments passed in
#' \code{...} according to which type of \code{roi} parameter.
#'
#' @returns \code{.roi_switch()}: one of the arguments in \code{...}.
.roi_switch <- function(roi, ...) {
    switch(.roi_type(roi),
           ...
    )
}

#' @describeIn roi_api Converts \code{roi} to an \code{sf} object.
#'
#' @returns \code{.roi_as_sf()}: \code{sf}.
.roi_as_sf <- function(roi, default_crs = NULL) {
    .roi_switch(
        roi = roi, sf = roi,
        bbox = .bbox_as_sf(roi, default_crs = default_crs),
        lonlat = .bbox_as_sf(list(
            xmin = roi[["lon_min"]], xmax = roi[["lon_max"]],
            ymin = roi[["lat_min"]], ymax = roi[["lat_max"]],
            crs = "EPSG:4326"
        ))
    )
}

#---- Chunks API: ----

#' Chunks API
#'
#' A chunk represents a rectangular region definition of a matrix and
#' its corresponding geographical area. So, a chunk object contains a
#' \code{block} and a \code{bbox} information. chunks can be used to access
#' specific raster image regions and optimize memory usage.
#'
#' Generally, chunks are created from an actual image that is divided
#' into small blocks. The chunks also provide overlapping support, that is,
#' chunks that intersects its neighbors by some amount of pixels.
#'
#' @param block A \code{block} to represent the common chunk size.
#' @param overlap An integer informing overlapping size in pixels.
#' @param image_size A \code{block} informing original image's matrix size.
#' @param image_bbox A \code{bbox} informing original image bbox.
#' @param chunks A \code{chunk}.
#'
#' @examples
#' \dontrun{
#' chunks <- .chunks_create(
#'   block = c(ncols = 512, nrows = 512),
#'   overlap = 2,
#'   image_size = c(ncols = 4000, nrows = 4000),
#'   image_bbox = c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4326)
#' )
#' # remove overlaps from chunks
#' cropped <- .chunks_no_overlap(chunks)
#' # removing overlaps from a non overlapped chunks produces identical bbox
#' identical(.bbox(cropped), .bbox(.chunks_no_overlap(cropped)))
#' # blocks from 'cropped' can be used to remove any overlap from rasters
#' # produced from 'chunks'.
#' .chunks_filter_spatial(
#'   chunks = chunks,
#'   roi = c(lon_min = 1.3, lon_max = 1.7, lat_min = 3.3, lat_max = 3.7)
#' )
#' }
#'
#' @seealso \link{chunk_accessors}
#' @family region objects API
#' @keywords internal
#' @name chunks_api
NULL

#' @describeIn chunks_api Creates a tibble of chunks with the same size as
#'   \code{block} and additional \code{overlap}.
#'
#' @returns \code{.chunks_create()}: \code{chunks} tibble.
.chunks_create <- function(block, overlap, image_size, image_bbox) {
    # Generate all starting block points (col, row)
    chunks <- purrr::cross_df(list(
        col = seq(1, .ncols(image_size), .ncols(block)),
        row = seq(1, .nrows(image_size), .nrows(block))
    ))
    # Adjust col and row to do overlap
    chunks[["col"]] <- .as_int(pmax(1, .col(chunks) - overlap))
    chunks[["row"]] <- .as_int(pmax(1, .row(chunks) - overlap))
    # Adjust ncols and nrows to do overlap
    chunks[["ncols"]] <-
        .as_int(pmin(.ncols(image_size), .col(chunks) + .ncols(block) +
                         overlap - 1) - .col(chunks) + 1)
    chunks[["nrows"]] <-
        .as_int(pmin(.nrows(image_size), .row(chunks) + .nrows(block) +
                         overlap - 1) - .row(chunks) + 1)
    # Chunk of entire image
    entire_image <- c(image_size, image_bbox)
    # Prepare a raster as template to crop bbox
    t_obj <- .chunks_as_raster(chunk = entire_image, nlayers = 1)
    # Generate chunks' bbox
    chunks <- slider::slide_dfr(chunks, function(chunk) {
        # Crop block from template
        r_obj <- .raster_crop_metadata(r_obj = t_obj, block = .block(chunk))
        # Add bbox information
        .xmin(chunk) <- .raster_xmin(r_obj = r_obj)
        .xmax(chunk) <- .raster_xmax(r_obj = r_obj)
        .ymin(chunk) <- .raster_ymin(r_obj = r_obj)
        .ymax(chunk) <- .raster_ymax(r_obj = r_obj)
        .crs(chunk) <- .raster_crs(r_obj = r_obj)
        chunk
    })
    # Overlapping support
    chunks[["overlap"]] <- .as_int(overlap)
    # Chunk size without overlap
    chunks[["crop_ncols"]] <- .as_int(pmin(
        .ncols(image_size) - .col(chunks) + 1, .ncols(block)
    ))
    chunks[["crop_nrows"]] <- .as_int(pmin(
        .nrows(image_size) - .row(chunks) + 1, .nrows(block)
    ))
    # Return chunks
    chunks
}

#' @describeIn chunks_api Creates an empty \code{raster} object based on the
#'   first chunk passed in \code{chunk} parameter.
#'
#' @returns \code{raster} object.
.chunks_as_raster <- function(chunk, nlayers) {
    .raster_new_rast(
        nrows = .nrows(chunk)[[1]],
        ncols = .ncols(chunk)[[1]],
        xmin = .xmin(chunk)[[1]],
        xmax = .xmax(chunk)[[1]],
        ymin = .ymin(chunk)[[1]],
        ymax = .ymax(chunk)[[1]],
        nlayers = nlayers,
        crs = .crs(chunk)[[1]]
    )
}

#' @describeIn chunks_api Creates a \code{chunk} that can be used to
#'   remove overlaps.
#'
#' @returns \code{.chunks_no_overlap()}: \code{chunks} tibble.
.chunks_no_overlap <- function(chunks) {
    # Generate blocks
    cropped <- tibble::tibble(
        col = .as_int(pmin(chunks[["overlap"]] + 1, .col(chunks))),
        row = .as_int(pmin(chunks[["overlap"]] + 1, .row(chunks)))
    )
    # Adjust blocks size
    .ncols(cropped) <- pmin(
        .ncols(chunks) - .col(cropped) + 1, .as_int(chunks[["crop_ncols"]])
    )
    .nrows(cropped) <- pmin(
        .nrows(chunks) - .row(cropped) + 1, .as_int(chunks[["crop_nrows"]])
    )
    # Generate bbox for each chunk
    cropped <- slider::slide2_dfr(chunks, cropped, function(chunk, crop) {
        # Prepare a raster as template to crop bbox
        t_obj <- .chunks_as_raster(chunk = chunk, nlayers = 1)
        # Crop block from template
        r_obj <- .raster_crop_metadata(r_obj = t_obj, block = .block(crop))
        # Add bbox information
        .xmin(crop) <- .raster_xmin(r_obj = r_obj)
        .xmax(crop) <- .raster_xmax(r_obj = r_obj)
        .ymin(crop) <- .raster_ymin(r_obj = r_obj)
        .ymax(crop) <- .raster_ymax(r_obj = r_obj)
        .crs(crop) <- .raster_crs(r_obj = r_obj)
        crop
    })
    # Finish cropped chunks
    cropped[["overlap"]] <- 0
    cropped[["crop_ncols"]] <- chunks[["crop_ncols"]]
    cropped[["crop_nrows"]] <- chunks[["crop_nrows"]]
    # Return cropped chunks
    cropped
}

#' @describeIn chunks_api Filter \code{chunks} that intersects a given
#'   \code{roi}.
#'
#' @returns \code{.chunks_filter_spatial()}: \code{chunks} tibble.
.chunks_filter_spatial <- function(chunks, roi) {
    chunks[.intersects(.bbox_as_sf(chunks), .roi_as_sf(roi)), ]
}

#---- Band API: ----

#' Band API
#'
#' We use term \code{band} to refer either to spectral bands as index generated
#' from images of actual sensor bands. To organize internal metadata and work
#' properly there are some restrictions in band names. These functions aims
#' to impose band name restrictions.
#'
#' @param band Band name.
#'
#' @examples
#' \dontrun{
#' .band_cloud() # 'CLOUD'
#' # eo bands name are uppercase
#' .band_eo("nDvI") # 'NDVI'
#' # derived bands name are lowercase
#' .band_derived("PrObS") # 'probs'
#' # bands name cannot have '_' (underscore)
#' .band_eo("NDVI_2") # 'NDVI-2'
#' }
#'
#' @seealso \link{band_accessors}
#' @name band_api
NULL

#' @describeIn band_api Returns the name of cloud band.
.band_cloud <- function() {
    "CLOUD"
}

#' @describeIn band_api Returns a well formatted band name for \code{eo_cube}.
.band_eo <- function(band) {
    gsub("_", "-", toupper(band))
}

#' @describeIn band_api Returns a well formatted band name for
#'   \code{derived_cube}.
.band_derived <- function(band) {
    gsub("_", "-", tolower(band))
}

#---- Config: ----

#' Basic access config functions
#'
#' These are basic functions to access config options.
#'
#' @param ... Set of \code{character} values representing a key to access
#'   some hierarchical config entry.
#' @param throw_error Should an error be thrown if test fails?
#'
#' @examples
#' \dontrun{
#' .conf_exists("run_tests") # TRUE
#' .conf("run_tests")
#' .conf_exists("not_existing_config_entry") # FALSE
#' }
#'
#' @returns Configuration value.
#'
#' @family config functions
#' @keywords internal
#' @name config_api
NULL

#' @describeIn config_api Tests if a key provided as \code{character} values
#'   in \code{...} parameter exists in the config. If \code{throws_error} is
#'   \code{TRUE} and the test failed, an error is raised.
.conf_exists <- function(..., throw_error = FALSE) {
    key <- c(...)
    exists <- .has(.try(sits_env[["config"]][[key]], .default = NULL))
    if (!exists && throw_error) {
        stop("key '", paste(key, collapse = "->"), "' not found in config")
    }
    # Return test
    exists
}

#' @describeIn config_api Get a config value located in a key provided as
#'   \code{character} values in \code{...} parameter. If a key does not
#'   exists, throws an error. Use \code{.conf_exists()} to test for a key
#'   existence.
.conf <- function(...) {
    key <- c(...)
    # Check for key existence and throws an error if it not exists
    .conf_exists(key, throw_error = TRUE)
    sits_env[["config"]][[c(key)]]
}

#' Config functions for \code{eo_cube}
#'
#' These are syntactic sugar functions to easily access config options for
#' bands of \code{eo_cube} cubes. \code{eo_cubes} are a S3 class representation
#' for an Earth Observation cube. It is the primary data used to obtain a
#' classification map.
#'
#' The config entries of a \code{eo_cube} are located in
#' \code{sources -> <SOURCE> -> collections -> <COLLECTION>} key.
#' Values for \code{source}, \code{collection}, and \code{band} are uppercase.
#'
#' @param source Source name.
#' @param collection Collection name.
#' @param band Band name.
#'
#' @examples
#' \dontrun{
#' # tests if 'BDC -> MOD13Q1-6 -> NDVI' key exists in config
#' .conf_eo_band_exists(
#'   source = "BDC", collection = "MOD13Q1-6", band = "NDVI"
#' )
#' # get configuration for band NDVI of 'BDC -> MOD13Q1-6' collection
#' x <- .conf_eo_band(
#'   source = "BDC", collection = "MOD13Q1-6", band = "NDVI"
#' )
#' }
#'
#' @returns Configuration value.
#'
#' @seealso Band accessors: \link{band_accessors}
#' @family config functions
#' @keywords internal
#' @name eo_cube_config
NULL

#' @describeIn eo_cube_config Tests if a \code{band} entry exists in config
#'   for some \code{source} and \code{collection}. If neither \code{source}
#'   nor \code{collection} entry are found in config, an error is thrown.
#'   Use \code{.conf_exists()} to test for \code{source} and \code{collection}
#'   existence.
.conf_eo_band_exists <- function(source, collection, band) {
    # source, collection, and band are uppercase
    source <- toupper(source)
    collection <- toupper(collection)
    band <- .band_eo(band)
    # Check for source and collection and throws an error if it not exists
    .conf_exists(
        "sources", source, "collections", collection,
        throw_error = TRUE
    )
    # Test for band and return
    .conf_exists("sources", source, "collections", collection, "bands", band)
}

#' @describeIn eo_cube_config Get a config value of for a \code{band} located
#'   in a \code{source} and \code{collection}. If the \code{band} is not
#'   found, a default value will be returned from config. If neither
#'   \code{source} nor \code{collection} entry are found in config, an
#'   error is thrown. Use \code{.conf_exists()} to test for \code{source} and
#'   \code{collection} existence.
.conf_eo_band <- function(source, collection, band) {
    # Format band name
    band <- .band_eo(band)
    # Return a default value if band does not exists in config
    if (!.conf_eo_band_exists(source, collection, band)) {
        return(.conf("default_values", "eo_cube"))
    }
    # Get band config value and return it
    .conf("sources", source, "collections", collection, "bands", band)
}

#' Config functions for \code{derived_cube}
#'
#' These are syntactic sugar functions to easily access config options for
#' bands of \code{derived_cube} cubes. \code{derived_cubes} are a S3 class
#' representation of a cube generated by the classification workflow starting
#' from an Earth Observation data cube.
#'
#' There are several classes of \code{derived_cube}:
#' \itemize{
#' \item \code{probs_cube}: multilayer probability cube produced by a
#'   classification with the probabilities attributed to each class by a
#'   model. The possible band names are \code{'probs'}, \code{'bayes'}, and
#'   \code{'bilat'}, acronyms for 'probability', 'Bayesian smoothing', and
#'   'Bilateral smoothing'.
#' \item \code{class_cube}: labeled cube (classified map) produced by choosing
#'   a label for each pixel. Its unique band name is \code{'class'}.
#' \item \code{uncertainty_cube}: a cube produced to measure the uncertainty of
#'   a classification for each pixel. The possible band names are
#'   \code{'least'}, \code{'entropy'}, and \code{'margin'}, acronyms for
#'   the method used to produce the cube.
#'   \code{'bilat'}, acronyms for 'probability', 'Bayesian smoothing', and
#'   'Bilateral smoothing'.
#' }
#'
#' Values for \code{derived_class} and \code{band} are lowercase. This was
#' done to avoid conflicts with \code{eo_cube} band naming (uppercase).
#' The config entries of a \code{derived_cube} are located in
#' \code{derived_cube -> <derived_class>} key.
#'
#' @param derived_class Class name of the \code{derived_cube}.
#' @param band Band name.
#'
#' @examples
#' \dontrun{
#' # get S3 class value that a derived_cube of class 'probs' must have
#' .conf_derived_s3class("probs_cube")
#' .conf_derived_band("probs_cube", "probs")
#' .conf_derived_band("class_cube", "class")
#' .conf_derived_band("probs_cube", "NBR") # error
#' }
#'
#' @returns Configuration value.
#'
#' @seealso Band accessors: \link{band_accessors}
#' @family config functions
#' @keywords internal
#' @name derived_cube_config
NULL

#' @describeIn derived_cube_config Get the S3 class values to instantiate a
#'   new \code{derived_cube}.
.conf_derived_s3class <- function(derived_class) {
    # derived_class is lowercase
    derived_class <- tolower(derived_class)
    .conf("derived_cube", derived_class, "s3_class")
}

#' @describeIn derived_cube_config Get the S3 class values to instantiate a
#'   new \code{derived_cube}.
.conf_derived_band <- function(derived_class, band) {
    # Format band
    band <- .band_derived(band)
    # Derived_class is lowercase
    derived_class <- tolower(derived_class)
    .conf("derived_cube", derived_class, "bands", band)
}


# .conf_exists("sources", "BDC", "collections", "MOD13Q1-6")
# .conf_exists("sources", "BDC", "collections", "MOD13Q1-7")
# .conf_eo_band("BDC", "MOD13Q1-6", "NIR", "missing_value")
# .conf_eo_band("BDC", "MOD13Q1-6", "NBR", "missing_value")
# .conf_derived_cube("probs_cube", "bands")
# .conf_derived_cube("probs_cube", "NBR")
# .conf_derived_cube_band("probs_cube", "probs", "missing_value")
# .conf_derived_cube_band("probs_cube", "bayes", "missing_value")

# ---- Point API ----

#' Point API
#'
#' A point represents an dimensionless geographical location in
#' 'EPSG:4326' projection. A \code{point} is any \code{list} or \code{tibble}
#' containing \code{longitude} and \code{latitude} fields. A \code{point} may
#' contains multiple entries.
#'
#' @param x Any object to extract a \code{point}.
#' @param ... Additional parameters.
#' @param point A \code{point}.
#' @param as_crs A CRS to project \code{point}.
#'
#' @examples
#' \dontrun{
#' x <- list(a = 0, z = 0)
#' .point(x) # NULL
#' x <- list(a = 0, longitude = 1:3, b = 2:4, latitude = 2, z = 0)
#' .point(x)
#' .point_as_sf(x) # 3 features
#' .point_as_sf(x, as_crs = "EPSG:3857") # reprojected features
#' }
#'
#' @seealso \link{point_accessors}
#' @family region objects API
#' @keywords internal
#' @name point_api
NULL

# point fields
.point_cols <- c("longitude", "latitude")

#' @describeIn point_api Does vector \code{x} has \code{point} fields?
#'
#' @returns \code{.has_point()}: \code{logical}.
.has_point <- function(x) {
    all(.point_cols %in% names(x))
}

#' @describeIn point_api Extract a \code{point} from any given
#' \code{vector}.
#'
#' @returns \code{.point()}: \code{point}.
.point <- function(x, ..., crs = NULL) {
    if (!.has_point(x)) {
        return(NULL)
    }
    if (!.has(crs)) crs <- "EPSG:4326"
    list(longitude = .lon(x), latitude = .lat(x), crs = crs)
}

#' @describeIn point_api Convert a \code{point} into a
#' \code{sf} point object.
#'
#' @returns \code{.point_as_sf()}: \code{sf}.
.point_as_sf <- function(point, ..., crs = NULL, as_crs = NULL) {
    point <- .point(point, crs = crs)
    if (!.has(point)) {
        stop("object does not have a valid point")
    }
    # Convert to sf object and return it
    purrr::pmap_dfr(point, function(longitude, latitude, crs) {
        geom <- sf::st_sf(
            geometry = sf::st_sfc(sf::st_point(c(longitude, latitude))),
            crs = crs
        )
        # Project CRS
        if (.has(as_crs)) {
            geom <- sf::st_transform(geom, crs = as_crs)
        }
        # Return geom
        geom
    })
}


#---- fi API: ----

#' File info API
#'
#' A \code{file_info} represents a set of raster references. It is a
#' \code{tibble} containing metadata describing images (e.g. bbox, band,
#' size, spatial resolution, date) and a path to access the resource. There
#' are two types of \code{file_info}: a \code{eo_cube} file info and a
#' \code{derived_cube} file_info. The main differences are in the metadata
#' stored by each type.
#'
#' @param fi A \code{file_info}.
#' @param ... Parameters to be evaluated accordingly to \code{file_info} type.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @family file info API
#' @keywords internal
#' @name fi_api
NULL

# cols of eo_cube file info
# columns not included and to be removed from file_info in the future:
#  - xres, yres;
# columns not in the file_info but to be included in the future:
#  - crs;
.fi_eo_cols <- c("fid", "band", "date", "ncols", "nrows", "xmin", "xmax",
                 "ymin", "ymax", "path")

# rows of derived_cube file info
# columns not included and to be removed from file_info in the future:
#  - xres, yres;
# columns not in the file_info but to be included in the future:
#  - crs;
.fi_derived_cols <- c("band", "start_date", "end_date", "ncols", "nrows",
                      "xmin", "xmax", "ymin", "ymax", "path")

.fi_type <- function(fi) {
    if (all(.fi_eo_cols %in% names(fi))) {
        "eo_cube"
    } else if (all(.fi_derived_cols %in% names(fi))) {
        "derived_cube"
    } else {
        stop("invalid file info")
    }
}

.fi_switch <- function(fi, ...) {
    switch(.fi_type(fi),
           ...,
           stop("invalid file_info type")
    )
}

.fi <- function(x) {
    x[["file_info"]][[1]]
}

.fi_eo <- function(fid, band, date, ncols, nrows, xres, yres, xmin, xmax,
                   ymin, ymax, path) {
    # Create a new eo file_info
    tibble::tibble(
        fid = fid, band = .band_eo(band), date = date, ncols = ncols,
        nrows = nrows, xres = xres, yres = yres, xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax, path = path
    )
}

.fi_eo_from_files <- function(files, fid, bands, date) {
    .check_that(length(files) == length(bands))
    files <- path.expand(files)
    r_obj <- .raster_open_rast(files)
    .fi_eo(
        fid = fid[[1]],
        band = bands,
        date = date[[1]],
        ncols = .raster_ncols(r_obj),
        nrows = .raster_nrows(r_obj),
        xres = .raster_xres(r_obj),
        yres = .raster_yres(r_obj),
        xmin = .raster_xmin(r_obj),
        xmax = .raster_xmax(r_obj),
        ymin = .raster_ymin(r_obj),
        ymax = .raster_ymax(r_obj),
        path = files
    )
}

.fi_derived <- function(band, start_date, end_date, ncols, nrows, xres, yres,
                        xmin, xmax, ymin, ymax, crs, path) {
    # Create a new derived file_info
    tibble::tibble(
        band = .band_derived(band), start_date = start_date,
        end_date = end_date, ncols = ncols, nrows = nrows,
        xres = xres, yres = yres, xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax, crs = crs, path = path
    )
}

.fi_derived_from_file <- function(file, band, start_date, end_date) {
    file <- path.expand(file)
    r_obj <- .raster_open_rast(file)
    .fi_derived(
        band = band, start_date = start_date, end_date = end_date,
        ncols = .raster_ncols(r_obj), nrows = .raster_nrows(r_obj),
        xres = .raster_xres(r_obj), yres = .raster_yres(r_obj),
        xmin = .raster_xmin(r_obj), xmax = .raster_xmax(r_obj),
        ymin = .raster_ymin(r_obj), ymax = .raster_ymax(r_obj),
        crs = .raster_crs(r_obj), path = file
    )
}

.fi_fid <- function(fi) {
    .as_chr(fi[["fid"]])
}

.fi_fid_filter <- function(fi, fid) {
    .fi_switch(
        fi = fi,
        eo_cube = fi[.fi_bands(fi) %in% .as_chr(fid), ]
    )
}

.fi_bands <- function(fi) {
    .as_chr(fi[["band"]])
}

.fi_filter_bands <- function(fi, bands) {
    bands_in_fi <- bands %in% .fi_bands(fi)
    if (!all(bands_in_fi)) {
        missing_bands <- paste0("'", bands[!bands_in_fi], "'", collapse = ",")
        stop("band(s) ", missing_bands, " not found")
    }
    fi[.fi_bands(fi) %in% bands, ]
}

.fi_min_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = min(.as_date(fi[["date"]])),
        derived_cube = min(.as_date(fi[["start_date"]]))
    )
}

.fi_max_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = max(.as_date(fi[["date"]])),
        derived_cube = max(.as_date(fi[["end_date"]]))
    )
}

.fi_dates <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = .as_date(fi[["date"]])
    )
}

.fi_date <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = .as_date(fi[["date"]][[1]])
    )
}

.fi_timeline <- function(fi) {
    .fi_switch(
        fi = fi,
        eo_cube = .as_date(fi[["date"]]),
        derived_cube = .as_date(c(fi[["start_date"]], fi[["end_date"]]))
    )
}

.fi_paths <- function(fi) {
    .as_chr(fi[["path"]])
}

.fi_path <- function(fi) {
    .as_chr(fi[["path"]][[1]])
}

.fi_as_sf <- function(fi) {
    .bbox_as_sf(fi)
}

.fi_during <- function(fi, start_date, end_date) {
    .between(.fi_timeline(fi), start_date, end_date)
}

.fi_filter_temporal <- function(fi, start_date, end_date) {
    if (!.has(start_date)) {
        start_date <- .fi_min_date(fi)
    }
    if (!.has(end_date)) {
        end_date <- .fi_max_date(fi)
    }
    fi[.fi_during(fi, start_date, end_date), ]
}

.fi_intersects <- function(fi, roi) {
    .intersects(.fi_as_sf(fi), .roi_as_sf(roi))
}

.fi_filter_spatial <- function(fi, roi) {
    fi[.fi_intersects(fi, roi), ]
}

.fi_read_block <- function(fi, band, block) {
    band <- band[[1]]
    # Stops if no band is found
    fi <- .fi_filter_bands(fi = fi, bands = band)
    files <- .fi_paths(fi)

    #
    # Log here
    #
    .sits_debug_log(
        event = "start_block_data_read",
        key = "band",
        value = band
    )


    # Read values from all files in file_info
    values <- .raster_read_rast(files = files, block = block)


    #
    # Log here
    #
    .sits_debug_log(
        event = "end_block_data_read",
        key = "band",
        value = band
    )

    # Return values
    values
}

#---- Tile API: ----

#' Tile API
#'
#' A \code{cube} consists of multiple tiles stacked together as rows of a
#' \code{tibble}. Get first tile of a cube. This function should be called
#' by all tile API function to ensure that only one tile will be processed.
#'
#' @param cube A \code{cube} or a \code{tile}.
#'
#' @return The first row of a cube.
.tile <- function(cube) {
    UseMethod(".tile", cube)
}

#' @export
.tile.raster_cube <- function(cube) {
    cube[1, ]
}

#' Tile field accessors
#'
#' These functions are accessors of \code{tile} objects. A
#' \code{tile} is a only-one-row \code{tibble} \code{cube} that stores
#' metadata of a cube.
#'
#' @param tile A \code{tile} or \code{cube} (only first row will be considered).
#' @param value A value to set in a \code{tile} field.
#'
#' @returns Respective \code{tile} field value.
#'
#' @family accessors
#' @keywords internal
#' @name tile_accessors
NULL

#' @describeIn tile_accessors Get \code{'source'} field of a \code{tile}.
.tile_source <- function(tile) {
    UseMethod(".tile_source", tile)
}

#' @export
.tile_source.raster_cube <- function(tile) {
    .as_chr(tile[["source"]][[1]])
}

#' @describeIn tile_accessors Get \code{'collection'} field of a \code{tile}.
.tile_collection <- function(tile) {
    UseMethod(".tile_collection", tile)
}

#' @export
.tile_collection.raster_cube <- function(tile) {
    .as_chr(tile[["collection"]][[1]])
}

#' @describeIn tile_accessors Get \code{'tile'} field of a \code{tile}.
.tile_name <- function(tile) {
    UseMethod(".tile_name", tile)
}

#' @export
.tile_name.raster_cube <- function(tile) {
    .as_chr(tile[["tile"]][[1]])
}

#' @describeIn tile_accessors Get number of image columns from \code{ncols}
#'   field (if it exists) or from first \code{ncols} in file_info.
.tile_ncols <- function(tile) {
    UseMethod(".tile_ncols", tile)
}

#' @export
.tile_ncols.raster_cube <- function(tile) {
    if ("ncols" %in% tile) {
        return(.ncols(tile)[[1]])
    }
    .ncols(.fi(tile))[[1]]
}

#' @describeIn tile_accessors Get number of image columns from \code{nrows}
#'   field (if it exists) or from first \code{nrows} in file_info.
.tile_nrows <- function(tile) {
    UseMethod(".tile_nrows", tile)
}

#' @export
.tile_nrows.raster_cube <- function(tile) {
    if ("nrows" %in% tile) {
        return(.nrows(tile)[[1]])
    }
    .nrows(.fi(tile))[[1]]
}

#' @describeIn tile_accessors Get the size of tile from \code{ncols} and
#'   \code{nrows} fields (if they exist) or from first entry in file_info.
.tile_size <- function(tile) {
    UseMethod(".tile_size", tile)
}

#' @export
.tile_size.raster_cube <- function(tile) {
    list(ncols = .tile_ncols(tile), nrows = .tile_nrows(tile))
}

#' @describeIn tile_accessors Get \code{'labels'} field of a \code{tile}.
.tile_labels <- function(tile) {
    UseMethod(".tile_labels", tile)
}

#' @export
.tile_labels.raster_cube <- function(tile) {
    .as_chr(tile[["labels"]][[1]])
}

#' @describeIn tile_accessors Set \code{'labels'} field of a \code{tile}.
`.tile_labels<-` <- function(tile, value) {
    UseMethod(".tile_labels<-", tile)
}

#' @export
`.tile_labels<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["labels"]] <- list(.as_chr(value))
    tile
}

#' @describeIn tile_accessors Get \code{'file_info'} field of a \code{tile}.
.tile_file_info <- function(tile) {
    UseMethod(".tile_file_info", tile)
}

#' @export
.tile_file_info.raster_cube <- function(tile) {
    .fi(tile) # Get the first file_info
}

#' @describeIn tile_accessors Set \code{'file_info'} field of a \code{tile}.
`.tile_file_info<-` <- function(tile, value) {
    UseMethod(".tile_file_info<-", tile)
}

#' @export
`.tile_file_info<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["file_info"]] <- list(value)
    tile
}

#---- | .tile_as_sf() ----
#' Tile API
#'
#' Convert tile \code{bbox} to a sf polygon object.
#'
#' @param tile A tile.
#'
#' @return file_info
.tile_as_sf <- function(tile) {
    UseMethod(".tile_as_sf", tile)
}

#' @export
.tile_as_sf.raster_cube <- function(tile) {
    .bbox_as_sf(.tile(tile))
}

#---- | .tile_start_date() ----
#' Tile API
#'
#' Get first date from file_info.
#'
#' @param tile A tile.
#'
#' @return date
.tile_start_date <- function(tile) {
    UseMethod(".tile_start_date", tile)
}

#' @export
.tile_start_date.raster_cube <- function(tile) {
    .fi_min_date(.fi(tile))
}

#---- | .tile_end_date() ----
#' Tile API
#'
#' Get end date from file_info.
#'
#' @param tile A tile.
#'
#' @return date
.tile_end_date <- function(tile) {
    UseMethod(".tile_end_date", tile)
}

#' @export
.tile_end_date.raster_cube <- function(tile) {
    .fi_max_date(.fi(tile))
}

#---- | .tile_timeline() ----
#' Tile API
#'
#' Get unique timeline from file_info.
#'
#' @param tile A tile.
#'
#' @return date
.tile_timeline <- function(tile) {
    UseMethod(".tile_timeline", tile)
}

#' @export
.tile_timeline.raster_cube <- function(tile) {
    unique(.fi_timeline(.fi(tile)))
}

#---- | .tile_bands() ----
#' Tile API
#'
#' Get sorted unique bands from file_info.
#'
#' @param tile A tile.
#'
#' @return character
.tile_bands <- function(tile) {
    UseMethod(".tile_bands", tile)
}

#' @export
.tile_bands.raster_cube <- function(tile) {
    unique(.fi_bands(.fi(tile)))
}

#---- | .tile_band_conf() ----
#' Tile API
#'
#' Get a band definition from config.
#'
#' @param tile A tile.
#' @param band Band character vector.
#'
#' @return band_conf or band_cloud_conf
.tile_band_conf <- function(tile, band) {
    UseMethod(".tile_band_conf", tile)
}

#' @export
.tile_band_conf.eo_cube <- function(tile, band) {
    .conf_eo_band(
        source = .tile_source(tile), collection = .tile_collection(tile),
        band = band[[1]]
    )
}

#' @export
.tile_band_conf.derived_cube <- function(tile, band) {
    .conf_derived_band(
        derived_class = .tile_derived_class(tile), band = band[[1]]
    )
}

#---- | .tile_filter_bands() ----
#' Tile API
#'
#' Filter file_info entries of a given \code{band}.
#'
#' @param tile A tile.
#' @param bands Band names to be filtered.
#'
#' @return tile
.tile_filter_bands <- function(tile, bands) {
    UseMethod(".tile_filter_bands", tile)
}

#' @export
.tile_filter_bands.eo_cube <- function(tile, bands) {
    tile <- .tile(tile)
    .tile_file_info(tile) <-
        .fi_filter_bands(fi = .fi(tile), bands = .band_eo(bands))
    tile
}

#' @export
.tile_filter_bands.derived_cube <- function(tile, bands) {
    tile <- .tile(tile)
    .tile_file_info(tile) <-
        .fi_filter_bands(fi = .fi(tile), bands = .band_derived(bands))
    tile
}

#---- | .tile_intersects() ----
#' Tile API
#'
#' Does tile \code{bbox} intersect \code{roi} parameter?
#'
#' @param tile A tile.
#' @param roi A region of interest (ROI).
#'
#' @return logical
.tile_intersects <- function(tile, roi) {
    UseMethod(".tile_intersects", tile)
}

#' @export
.tile_intersects.raster_cube <- function(tile, roi) {
    .intersects(x = .tile_as_sf(tile), y = .roi_as_sf(roi))
}

#---- | .tile_filter_spatial() ----
#' Tile API
#'
#' Filter file_info entries that intersect \code{roi} parameter.
#'
#' @param tile A tile.
#' @param roi A region of interest (ROI).
#'
#' @return tile
.tile_filter_spatial <- function(tile, roi) {
    UseMethod(".tile_filter_spatial", tile)
}

#' @export
.tile_filter_spatial.raster_cube <- function(tile, roi) {
    tile <- .tile(tile)
    .tile_file_info(tile) <- .fi_filter_spatial(fi = .fi(tile), roi = roi)
    tile
}

#---- | .tile_during() ----
#' Tile API
#'
#' Is any date of tile's timeline between 'start_date'
#' and 'end_date'?
#'
#' @param tile A tile.
#' @param start_date,end_date Date of start and end.
#'
#' @return logical
.tile_during <- function(tile, start_date, end_date) {
    UseMethod(".tile_during", tile)
}

#' @export
.tile_during.raster_cube <- function(tile, start_date, end_date) {
    any(.fi_during(
        fi = .fi(tile), start_date = start_date, end_date = end_date
    ))
}

#---- | .tile_filter_temporal() ----
#' Tile API
#'
#' Filter file_info entries by 'start_date' and 'end_date.'
#'
#' @param tile A tile.
#' @param start_date,end_date Date of start and end.
#'
#' @return tile
.tile_filter_temporal <- function(tile, start_date, end_date) {
    UseMethod(".tile_filter_temporal", tile)
}

#' @export
.tile_filter_temporal.raster_cube <- function(tile, start_date, end_date) {
    tile <- .tile(tile)
    .tile_file_info(tile) <- .fi_filter_temporal(
        fi = .fi(tile), start_date = start_date, end_date = end_date
    )
    tile
}

#---- | .tile_derived_class() ----
#' Tile API
#'
#' Get derived class of a tile.
#'
#' @param tile A tile.
#'
#' @return character
.tile_derived_class <- function(tile) {
    UseMethod(".tile_derived_class", tile)
}

#' @export
.tile_derived_class.derived_cube <- function(tile) {
    class(tile)[[1]]
}

#---- | .tile_read_block() ----
#' Tile API
#'
#' Read and preprocess a \code{block} of \code{band} values from
#' file_info rasters.
#'
#' \code{eo_cube} preprocess is slightly different from
#' \code{derived_cube}: values outside the range of minimum and maximum for
#' a band are replaced by \code{NA} in \code{eo_cube}. In \code{derived_cube},
#' values outside allowed range are clamped and replaced by minimum or maximum
#' values.
#'
#' @param tile A tile.
#' @param band Band character vector.
#' @param block A block list with (col, row, ncols, nrows).
#'
#' @return numeric
.tile_read_block <- function(tile, band, block) {
    UseMethod(".tile_read_block", tile)
}

#' @export
.tile_read_block.eo_cube <- function(tile, band, block) {
    fi <- .fi(tile)
    # Stops if band is not found
    values <- .fi_read_block(fi = fi, band = .band_eo(band), block = block)


    #
    # Log here
    #
    .sits_debug_log(
        event = "start_block_data_process",
        key = "band",
        value = band
    )


    # Correct missing, minimum, and maximum values and
    # apply scale and offset.
    band_conf <- .tile_band_conf(tile = tile, band = band)
    miss_value <- .miss_value(band_conf)
    if (.has(miss_value)) {
        values[values == miss_value] <- NA
    }
    min_value <- .min_value(band_conf)
    if (.has(min_value)) {
        values[values < min_value] <- NA
    }
    max_value <- .max_value(band_conf)
    if (.has(max_value)) {
        values[values > max_value] <- NA
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0) {
        values <- values + offset
    }


    #
    # Log here
    #
    .sits_debug_log(
        event = "end_block_data_process",
        key = "band",
        value = band
    )


    # Return values
    values
}

#' @export
.tile_read_block.derived_cube <- function(tile, band, block) {
    fi <- .fi(tile)
    # Stops if band is not found
    values <- .fi_read_block(fi = fi, band = .band_derived(band), block = block)
    # Correct missing, minimum, and maximum values and
    # apply scale and offset.
    band_conf <- .tile_band_conf(tile = tile, band = band)
    miss_value <- .miss_value(band_conf)
    if (.has(miss_value)) {
        values[values == miss_value] <- NA
    }
    min_value <- .min_value(band_conf)
    if (.has(min_value)) {
        values[values < min_value] <- min_value
    }
    max_value <- .max_value(band_conf)
    if (.has(max_value)) {
        values[values > max_value] <- max_value
    }
    scale <- .scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0) {
        values <- values + offset
    }
    # Return values
    values
}

#---- | .tile_cloud_read_block() ----
#' Tile API
#'
#' Read and preprocess a \code{block} of cloud values from
#' file_info rasters.
#'
#' @param tile A tile.
#' @param block A block list with (col, row, ncols, nrows).
#'
#' @return numeric
.tile_cloud_read_block <- function(tile, block) {
    UseMethod(".tile_cloud_read_block", tile)
}

#' @export
.tile_cloud_read_block.eo_cube <- function(tile, block) {
    if (!.band_cloud() %in% .tile_bands(tile)) {
        return(NULL)
    }
    values <- .tile_read_block(
        tile = tile, band = .band_cloud(), block = block
    )


    #
    # Log here
    #
    .sits_debug_log(
        event = "start_block_data_process",
        key = "cloud_mask",
        value = "cloud_mask"
    )


    # Get cloud parameters
    cloud_conf <- .tile_band_conf(tile = tile, band = .band_cloud())
    interp_values <- .cloud_interp_values(cloud_conf)
    is_bit_mask <- .cloud_bit_mask(cloud_conf)
    # Prepare cloud_mask
    # Identify values to be removed
    if (!is_bit_mask) {
        values <- values %in% interp_values
    } else {
        values <- matrix(bitwAnd(values, sum(2^interp_values)) > 0,
                         nrow = length(values)
        )
    }


    #
    # Log here
    #
    .sits_debug_log(
        event = "end_block_data_process",
        key = "cloud_bit_mask",
        value = is_bit_mask
    )

    # Return values
    values
}

#---- | .tile_chunks_create() ----

.tile_chunks_create <- function(tile, overlap) {
    # Get block size
    block <-
        .raster_file_blocksize(.raster_open_rast(.fi_path(.fi(tile))))
    # Compute chunks
    .chunks_create(
        block = block,
        overlap = overlap,
        image_size = .tile_size(tile),
        image_bbox = .bbox(tile)
    )
}

#---- Tile constructors: ----

# ---- | tile eo api ----
.tile_eo_from_files <- function(files, fid, bands, date, base_tile,
                                update_bbox) {
    if (update_bbox) {
        # Open raster
        r_obj <- .raster_open_rast(files)
        # Update spatial bbox
        .xmin(base_tile) <- .raster_xmin(r_obj)
        .xmax(base_tile) <- .raster_xmax(r_obj)
        .ymin(base_tile) <- .raster_ymin(r_obj)
        .ymax(base_tile) <- .raster_ymax(r_obj)
        .crs(base_tile) <- .raster_crs(r_obj)
    }
    # Update file_info
    .tile_file_info(base_tile) <- .fi_eo_from_files(
        files = files, fid = fid, bands = bands, date = date
    )
    # Return eo tile
    base_tile
}

.tile_eo_merge_blocks <- function(files, bands, base_tile, block_files,
                                  multicores, update_bbox) {
    # Get conf band
    band_conf <- .tile_band_conf(tile = base_tile, band = bands)
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        out_files = files, base_file = .fi_path(.fi(base_tile)),
        block_files = block_files, data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf), multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_eo_from_files(
        files = files, fid = .fi_fid(.fi(base_tile)), bands = bands,
        date = .fi_date(.fi(base_tile)), base_tile = base_tile,
        update_bbox = update_bbox
    )
    # If all goes well, delete block files
    unlink(unlist(block_files))
    # Return eo tile
    tile
}

#---- | <derived_cube> ----
.tile_derived_from_file <- function(file, band, base_tile, derived_class,
                                    labels = NULL, update_bbox) {
    if (update_bbox) {
        # Open raster
        r_obj <- .raster_open_rast(file)
        # Update spatial bbox
        .xmin(base_tile) <- .raster_xmin(r_obj)
        .xmax(base_tile) <- .raster_xmax(r_obj)
        .ymin(base_tile) <- .raster_ymin(r_obj)
        .ymax(base_tile) <- .raster_ymax(r_obj)
        .crs(base_tile) <- .raster_crs(r_obj)
    }
    # Update labels before file_info
    .tile_labels(base_tile) <- labels
    # Update file_info
    .tile_file_info(base_tile) <- .fi_derived_from_file(
        file = file, band = band, start_date = .tile_start_date(base_tile),
        end_date = .tile_end_date(base_tile)
    )
    # Set tile class and return tile
    .cube_set_class(base_tile, .conf_derived_s3class(derived_class))
}

.tile_derived_merge_blocks <- function(file, band, labels, base_tile,
                                       derived_class, block_files, multicores,
                                       update_bbox) {
    # Get conf band
    band_conf <- .conf_derived_band(derived_class = derived_class, band = band)
    # Set base tile
    base_file <- if (update_bbox) NULL else .fi_path(.fi(base_tile))
    # Create a template raster based on the first image of the tile
    .raster_merge_blocks(
        out_files = file, base_file = base_file,
        block_files = block_files, data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf), multicores = multicores
    )
    # Create tile based on template
    tile <- .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = derived_class, labels = labels,
        update_bbox = update_bbox
    )
    # If all goes well, delete block files
    unlink(block_files)
    # Return derived tile
    tile
}

# ---- | <probs_cube> ----
.tile_probs_from_file <- function(file, band, base_tile, labels, update_bbox) {
    # Open block file to be merged
    r_obj <- .raster_open_rast(file)
    # Check number of labels is correct
    .check_that(
        x = .raster_nlayers(r_obj) == length(labels),
        local_msg = "number of image layers does not match labels",
        msg = "invalid 'file' parameter"
    )
    .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = "probs_cube", labels = labels,
        update_bbox = update_bbox
    )
}

.tile_probs_merge_blocks <- function(file, band, labels, base_tile,
                                     block_files, multicores, update_bbox) {
    # Open first block file to be merged
    r_obj <- .raster_open_rast(unlist(block_files)[[1]])
    # Check number of labels is correct
    .check_that(
        x = .raster_nlayers(r_obj) == length(labels),
        local_msg = "number of image layers does not match labels",
        msg = "invalid 'file' parameter"
    )
    # Create probs cube and return it
    .tile_derived_merge_blocks(
        file = file, band = band, labels = labels,
        base_tile = base_tile, derived_class = "probs_cube",
        block_files = block_files, multicores = multicores,
        update_bbox = update_bbox
    )
}

#---- | <class_cube> ----
.tile_class_from_file <- function(file, band, base_tile) {
    .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = "class_cube", labels = .tile_labels(base_tile),
        update_bbox = FALSE
    )
}

.tile_class_merge_blocks <- function(file, band, labels, base_tile,
                                     block_files, multicores) {
    # Create class cube and return it
    .tile_derived_merge_blocks(
        file = file, band = band, labels = labels,
        base_tile = base_tile, derived_class = "class_cube",
        block_files = block_files, multicores = multicores,
        update_bbox = FALSE
    )
}

#---- | <uncertainty_cube> ----
.tile_uncertainty_from_file <- function(file, band, base_tile) {
    .tile_derived_from_file(
        file = file, band = band, base_tile = base_tile,
        derived_class = "uncertainty_cube", labels = .tile_labels(base_tile),
        update_bbox = FALSE
    )
}

.tile_uncertainty_merge_blocks <- function(file, band, labels, base_tile,
                                           block_files, multicores) {
    # Create uncertainty cube and return it
    .tile_derived_merge_blocks(
        file = file, band = band, labels = labels,
        base_tile = base_tile, derived_class = "uncertainty_cube",
        block_files = block_files, multicores = multicores,
        update_bbox = FALSE
    )
}

#---- Cube API ----

#---- | .cube_set_class() ----
.cube_set_class <- function(x, ...) {
    .set_class(x, ..., c("sits_cube", "tbl_df", "tbl", "data.frame"))
}

#---- | .cube_file_info() ----
.cube_file_info <- function(cube) {
    UseMethod(".cube_file_info", cube)
}

#' @export
.cube_file_info.raster_cube <- function(cube) {
    tidyr::unnest(cube["file_info"], "file_info")
}

#---- | .cube_start_date() ----
#' @title Cube API
#' @param cube A cube.
#' @description Get start dates from each tile.
#' @return date
.cube_start_date <- function(cube) {
    UseMethod(".cube_start_date", cube)
}

#' @export
.cube_start_date.raster_cube <- function(cube) {
    .compact(.as_date(slider::slide(cube, .tile_start_date)))
}

#---- | .cube_end_date() ----
#' @title Cube API
#' @param cube A cube.
#' @description Get end date from each tile.
#' @return date
.cube_end_date <- function(cube) {
    UseMethod(".cube_end_date", cube)
}

#' @export
.cube_end_date.raster_cube <- function(cube) {
    .compact(.as_date(slider::slide(cube, .tile_end_date)))
}

#---- | .cube_timeline() ----
#' @title Cube API
#' @param cube A cube.
#' @description Get timeline from each cube. If there are at least two
#' different timelines, all timelines will be returned in a list).
#' @return date or list(date)
.cube_timeline <- function(cube) {
    UseMethod(".cube_timeline", cube)
}

#' @export
.cube_timeline.raster_cube <- function(cube) {
    values <- .compact(slider::slide(cube, .tile_timeline))
    if (length(values) != 1) {
        return(values)
    }
    .as_date(values)
}


#---- | .cube_timeline_acquisiton() ----
#' @title Cube API
#' @param cube A cube.
#' @param period Period character vector in ISO format.
#' @param origin A date.
#' @description Compute how many images were acquired in different periods
#' and different tiles.
#' @return tibble
.cube_timeline_acquisiton <- function(cube, period, origin) {
    UseMethod(".cube_timeline_acquisiton", cube)
}

#' @export
.cube_timeline_acquisiton.raster_cube <-
    function(cube,
             period = "P1D",
             origin = NULL) {
        if (!.has(origin)) {
            origin <- .cube_start_date(cube)
        }
        values <- slider::slide_dfr(cube, function(tile) {
            tibble::tibble(
                tile = tile[["tile"]],
                dates = .tile_timeline(!!tile)
            )
        })
        values <- dplyr::filter(values, !!origin <= .data[["dates"]])
        values <- dplyr::arrange(values, .data[["dates"]])
        values <- slider::slide_period_dfr(
            values, values[["dates"]], .period_unit(period),
            function(x) {
                x[["from_date"]] <- min(x[["dates"]])
                x[["to_date"]] <- max(x[["dates"]])
                dplyr::count(
                    x, .data[["from_date"]], .data[["to_date"]],
                    .data[["tile"]]
                )
            },
            .every = .period_val(period), .origin = origin, .complete = TRUE
        )
        id_cols <- c("from_date", "to_date")
        if (all(values[["from_date"]] == values[["to_date"]])) {
            values[["date"]] <- values[["from_date"]]
            id_cols <- "date"
        }
        tidyr::pivot_wider(
            values,
            id_cols = id_cols,
            names_from = "tile",
            values_from = "n"
        )
    }

#---- | .cube_foreach_tile() ----
#' @title Cube API
#' @param cube A cube.
#' @param fn A function.
#' @param ... Additional arguments to be passed to \code{fn}.
#' @description Iterates over each cube tile, passing tile to function's
#' first argument.
#' @return cube
.cube_foreach_tile <- function(cube, fn, ...) {
    UseMethod(".cube_foreach_tile", cube)
}

#' @export
.cube_foreach_tile.raster_cube <- function(cube, fn, ...) {
    slider::slide_dfr(cube, fn, ...)
}

#---- | .cube_intersects() ----
#' @title Cube API
#' @param cube A cube.
#' @param roi A region of interest (ROI).
#' @description What tiles intersect \code{roi} parameter?
#' @return logical
.cube_intersects <- function(cube, roi) {
    UseMethod(".cube_intersects", cube)
}

#' @export
.cube_intersects.raster_cube <- function(cube, roi) {
    slider::slide_lgl(cube, .tile_intersects, roi = .roi_as_sf(roi))
}

#---- | .cube_filter_spatial() ----
#' @title Cube API
#' @param cube A cube.
#' @param roi A region of interest (ROI).
#' @description Filter tiles that intersect \code{roi} parameter.
#' @return cube
.cube_filter_spatial <- function(cube, roi) {
    UseMethod(".cube_filter_spatial", cube)
}

#' @export
.cube_filter_spatial.raster_cube <- function(cube, roi) {
    intersecting <- .cube_intersects(cube, roi)
    if (!any(intersecting)) {
        stop("informed roi does not intersect cube")
    }
    cube[intersecting, ]
}

#---- | .cube_during() ----
#' @title Cube API
#' @param cube A cube.
#' @param start_date,end_date Date of start and end.
#' @description What tiles have file_info entries between 'start_date'
#' and 'end_date'?
#' @return logical
.cube_during <- function(cube, start_date, end_date) {
    UseMethod(".cube_during", cube)
}

#' @export
.cube_during.raster_cube <- function(cube, start_date, end_date) {
    slider::slide_lgl(
        cube, .tile_during, start_date = start_date, end_date = end_date
    )
}

#---- | .cube_filter_temporal() ----
#' Cube API
#'
#' Filter tiles with 'file_info' entries between 'start_date'
#' and 'end_date'.
#'
#' @param cube A cube.
#' @param start_date,end_date Date of start and end.
#'
#' @return cube
.cube_filter_temporal <- function(cube, start_date, end_date) {
    UseMethod(".cube_filter_temporal", cube)
}

#' @export
.cube_filter_temporal.raster_cube <- function(cube, start_date, end_date) {
    during <- .cube_during(cube, start_date, end_date)
    if (!any(during)) {
        stop("informed interval does not interesect cube")
    }
    cube[during, ]
}

#---- | .cube_filter_bands() ----

.cube_filter_bands <- function(cube, bands) {
    UseMethod(".cube_filter_bands", cube)
}

.cube_filter_bands.raster_cube <- function(cube, bands) {
    slider::slide_dfr(cube, function(tile) {
        .tile_filter_bands(tile = tile, bands = bands)
    })
}

#---- | .cube_tiles() ----

.cube_tiles <- function(cube) {
    UseMethod(".cube_tiles", cube)
}

.cube_tiles.raster_cube <- function(cube) {
    .as_chr(cube[["tile"]])
}

#---- | .cube_tile_filter() ----

.cube_tile_filter <- function(cube, tile) {
    UseMethod(".cube_tile_filter", cube)
}

.cube_tile_filter.raster_cube <- function(cube, tile) {
    cube[.cube_tiles(cube) %in% tile, ]
}


#---- | .cube_create_features() ----


.cube_create_features <- function(cube) {
    # Process for each tile and return a cube
    slider::slide_dfr(cube, function(tile) {
        features <- tile[, c("tile", "file_info")]
        features <- tidyr::unnest(features, "file_info")
        features[["feature"]] <- features[["fid"]]
        features <- tidyr::nest(features, file_info = -c("tile", "feature"))
        # Replicate each tile so that we can copy file_info to cube
        tile <- tile[rep(1, nrow(features)), ]
        tile[["file_info"]] <- features[["file_info"]]
        tile
    })
}

#---- | .cube_merge_features() ----

.cube_merge_features <- function(features) {
    cube <- tidyr::unnest(features, "file_info", names_sep = ".")
    cube <- dplyr::arrange(
        cube, .data[["file_info.date"]], .data[["file_info.band"]]
    )
    cube <- tidyr::nest(
        cube, file_info = tidyr::starts_with("file_info"),
        .names_sep = "."
    )
    # Set class features and return
    .set_class(cube, class(features))
}


#---- | .cube_create_assets() ----

.cube_create_assets <- function(cube) {
    # Process for each tile and return a cube
    slider::slide_dfr(cube, function(tile) {
        assets <- tile[, c("tile", "file_info")]
        assets <- tidyr::unnest(assets, "file_info")
        assets[["feature"]] <- assets[["fid"]]
        assets[["asset"]] <- assets[["band"]]
        assets <- tidyr::nest(
            assets, file_info = -c("tile", "feature", "asset")
        )
        # Replicate each tile so that we can copy file_info to cube
        tile <- tile[rep(1, nrow(assets)), ]
        tile[["file_info"]] <- assets[["file_info"]]
        tile
    })
}

#---- | .cube_merge_assets() ----

.cube_merge_assets <- function(assets) {
    .cube_merge_features(assets)
}

# s2_cube <- sits_cube(
#     source = "AWS",
#     collection = "SENTINEL-S2-L2A-COGS",
#     tiles = c("20LKP", "20LLP", "20LNQ", "21LTH"),
#     bands = c("B08", "B11"),
#     start_date = "2018-07-12",
#     end_date = "2019-07-28"
# )
# .cube_intersects(s2_cube, s2_cube[2:3,])
# .cube_intersects(s2_cube, s2_cube[4,])
# .cube_filter_spatial(s2_cube, .bbox_as_sf(s2_cube[3:4,]))
# .cube_filter_spatial(s2_cube, .bbox_as_sf(s2_cube[3:4,], as_crs = 4326))
# .cube_start_date(s2_cube)
# .cube_end_date(s2_cube)
# .cube_timeline(s2_cube)
# .cube_filter_temporal(s2_cube, start_date = "2017-01-01", end_date = "2018-07-12")
# .cube_filter_temporal(s2_cube, start_date = "2017-01-01", end_date = "2018-07-14")
# .cube_filter_temporal(s2_cube, start_date = "2019-07-25", end_date = "2020-07-28")
# .cube_filter_temporal(s2_cube, start_date = "2019-07-28", end_date = "2020-07-28")
# .cube_filter_temporal(s2_cube, start_date = "2014-07-28", end_date = "2015-07-28")
#
# sinop <- sits_cube(
#     source = "BDC",
#     collection = "MOD13Q1-6",
#     data_dir = system.file("extdata/raster/mod13q1", package = "sits")
# )
# .cube_timeline(sinop)
# .cube_start_date(sinop)
# .cube_end_date(sinop)
# identical(.cube_merge_features(.cube_create_features(s2_cube)), s2_cube)
# identical(.cube_merge_assets(.cube_create_assets(s2_cube)), s2_cube)

#---- ml_model ----

.ml_model <- function(ml_model) {
    if ("model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["model"]]
    } else if ("torch_model" %in% ls(environment(ml_model))) {
        environment(ml_model)[["torch_model"]]
    } else {
        stop("cannot extract model object")
    }
}

.ml_stats_0 <- function(ml_model) {
    # Old stats variable
    environment(ml_model)[["stats"]]
}

.ml_stats <- function(ml_model) {
    # New stats variable
    environment(ml_model)[["ml_stats"]]
}

.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}

.ml_class <- function(ml_model) {
    class(ml_model)[[1]]
}

.ml_features_name <- function(ml_model) {
    # Get feature names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2:0]
}

.ml_bands <- function(ml_model) {
    .sits_bands(.ml_samples(ml_model))
}

.ml_labels <- function(ml_model) {
    .sits_labels(.ml_samples(ml_model))
}

.torch_serialize_model <- function(model) {
    # Open raw connection
    con <- rawConnection(raw(), open = "wr")
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Serialize and save torch model on connection
    torch::torch_save(model, con)
    # Read serialized model and return
    rawConnectionValue(con)
}

.torch_unserialize_model <- function(raw) {
    # Open raw connection to read model
    con <- rawConnection(raw)
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Unserialize and load torch model from connection and return
    torch::torch_load(con)
}

#---- stats ----

# Supports former version of stats
.stats_0_q02 <- function(stats, band) {
    quantile_02 <- 2
    stats[[band]][[quantile_02]]
}

# Supports former version of stats
.stats_0_q98 <- function(stats, band) {
    quantile_98 <- 3
    stats[[band]][[quantile_98]]
}

.stats_q02 <- function(stats) {
    stats[["q02"]]
}

.stats_q98 <- function(stats) {
    stats[["q98"]]
}

#---- sits (samples) ----

.sits_ts <- function(samples) {
    # Add sample_id column
    samples[["sample_id"]] <- seq_len(nrow(samples))
    # Extract time_series from column
    ts <- tidyr::unnest(
        data = samples[c("sample_id", "label", "time_series")],
        cols = "time_series"
    )
    # Select the same bands as in the first sample
    ts <- ts[c("sample_id", "label", "Index", .sits_bands(samples))]
    # Get the time series length for the first sample
    ntimes <- .sits_ntimes(samples)
    # Prune time series according to the first sample
    ts <- .by_dfr(data = ts, col = "sample_id", fn = function(x) {
        if (nrow(x) == ntimes) {
            x
        } else if (nrow(x) > ntimes) {
            x[seq_len(ntimes), ]
        } else {
            stop("time series length differs from first sample")
        }
    })
    # Return time series
    ts
}

.sits_ntimes <- function(samples) {
    # Number of observations of the first sample governs whole samples data
    nrow(samples[["time_series"]][[1]])
}

.sits_bands <- function(samples) {
    # Bands of the first sample governs whole samples data
    setdiff(names(samples[["time_series"]][[1]]), "Index")
}

.sits_filter_bands <- function(samples, bands) {
    # Missing bands
    miss_bands <- bands[!bands %in% .sits_bands(samples)]
    if (.has(miss_bands)) {
        stop("band(s) ", paste0("'", miss_bands, "'", collapse = ", "),
             " not found")
    }
    .sits_fast_apply(samples, col = "time_series", function(x) {
        dplyr::select(x, dplyr::all_of(c("#..", "Index", bands)))
    })
}

.sits_labels <- function(samples) {
    sort(unique(samples[["label"]]), na.last = TRUE)
}

.sits_split <- function(samples, split_intervals) {
    slider::slide_dfr(samples, function(sample) {
        ts <- sample[["time_series"]][[1]]
        purrr::map_dfr(split_intervals, function(index) {
            new_sample <- sample
            start <- index[[1]]
            end <- index[[2]]
            new_sample[["time_series"]][[1]] <- ts[seq(start, end), ]
            new_sample[["start_date"]] <- ts[["Index"]][[start]]
            new_sample[["end_date"]] <- ts[["Index"]][[end]]
            new_sample
        })
    })
}

.sits_predictors <- function(samples, ml_model = NULL) {
    # Get samples time series
    pred <- .sits_ts(samples)
    # By default get bands as the same of first sample
    bands <- .sits_bands(samples)
    # Preprocess time series
    if (.has(ml_model)) {
        # If a model is informed, get predictors from model bands
        bands <- .ml_bands(ml_model)
        # Normalize values for old version model classifiers that
        #   do not normalize values itself
        # Models trained after version 1.2 do this automatically before
        #   classification
        stats <- .ml_stats_0(ml_model) # works for old models only!!
        if (.has(stats)) {
            # Read and preprocess values of each band
            pred[bands] <- purrr::imap_dfc(pred[bands], function(values, band) {
                # Get old stats parameters
                q02 <- .stats_0_q02(stats, band)
                q98 <- .stats_0_q98(stats, band)
                if (.has(q02) && .has(q98)) {
                    # Use C_normalize_data_0 to process old version of
                    #   normalization
                    values <- C_normalize_data_0(
                        data = as.matrix(values), min = q02, max = q98
                    )
                    # Convert from matrix to vector and return
                    unlist(values)
                }
                # Return updated values
                values
            })
        }
    }
    # Create predictors...
    pred <- pred[c(.pred_cols, bands)]
    # Add sequence 'index' column grouped by 'sample_id'
    pred <- .by_dfr(data = pred, col = "sample_id", fn = function(x) {
        x[["index"]] <- seq_len(nrow(x))
        x
    })
    # Rearrange data to create predictors
    pred <- tidyr::pivot_wider(
        data = pred, names_from = "index", values_from = bands,
        names_prefix = if (length(bands) == 1) bands else "",
        names_sep = ""
    )
    # Return predictors
    pred
}

.sits_stats <- function(samples) {
    # Get all time series
    preds <- .sits_ts(samples)
    # Select attributes
    preds <- preds[.sits_bands(samples)]
    # Compute stats
    q02 <- apply(preds, 2, stats::quantile, probs = 0.02, na.rm = TRUE)
    q98 <- apply(preds, 2, stats::quantile, probs = 0.98, na.rm = TRUE)
    # Number of observations
    ntimes <- .sits_ntimes(samples)
    # Replicate stats
    q02 <- rep(unname(q02), each = ntimes)
    q98 <- rep(unname(q98), each = ntimes)
    # Return stats object
    list(q02 = q02, q98 = q98)
}

# ---- Predictors ----

.pred_cols <- c("sample_id", "label")

.pred_features <- function(pred) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, -2:0]
    } else {
        pred
    }
}

`.pred_features<-` <- function(pred, value) {
    if (all(.pred_cols %in% names(pred))) {
        pred[, -2:0] <- value
    } else {
        pred[,] <- value
    }
    pred
}

.pred_references <- function(pred) {
    if (all(.pred_cols %in% names(pred))) .as_chr(pred[["label"]]) else NULL
}

.pred_normalize <- function(pred, stats) {
    values <- as.matrix(.pred_features(pred))
    values <- C_normalize_data(
        data = values, min = .stats_q02(stats), max = .stats_q98(stats)
    )
    .pred_features(pred) <- values
    # Return predictors
    pred
}

.pred_create_partition <- function(pred, partitions) {
    pred[["part_id"]] <- .partitions(x = seq_len(nrow(pred)), n = partitions)
    tidyr::nest(pred, predictors = -"part_id")
}

# ---- Partitions ----

.part_predictors <- function(part) {
    if (.has(part[["predictors"]])) part[["predictors"]][[1]] else NULL
}

# ---- expressions ----

.expr_names <- function(expr) {
    if (is.call(expr)) {
        unique(unlist(lapply(as.list(expr)[-1], .expr_names)))
    } else if (is.name(expr)) {
        .as_chr(expr)
    } else {
        character()
    }
}

.expr_calls <- function(expr) {
    if (is.call(expr)) {
        unique(c(
            paste0(expr[[1]]), unlist(lapply(as.list(expr)[-1], .expr_calls))
        ))
    } else {
        character()
    }
}

# ---- gdal API ----

.gdal_data_type <- c(
    "INT1U" = "Byte", "INT2U" = "UInt16", "INT2S" = "Int16",
    "INT4U" = "UInt32", "INT4S" = "Int32", "FLT4S" = "Float32",
    "FLT8S" = "Float64"
)

.gdal_params <- function(params) {
    # Check if parameters are named
    if (!all(.has_name(params))) {
        stop("parameters should be named")
    }
    unlist(mapply(function(par, val) {
        if (is.logical(val)) {
            if (val) par else NULL
        } else if (is.list(val)) {
            c(par, unlist(val))
        } else {
            .as_chr(rbind(par, val))
        }
    }, names(params), unname(params), USE.NAMES = FALSE))
}

.gdal_translate <- function(file, base_file, params, quiet) {
    sf::gdal_utils(
        util = "translate", source = base_file[[1]], destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}

.gdal_warp <- function(file, base_files, params, quiet) {
    sf::gdal_utils(
        util = "warp", source = base_files, destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}

.gdal_template_from_file <- function(base_file, file, nlayers, miss_value,
                                     data_type) {
    # Convert to gdal data type
    data_type <- .gdal_data_type[[data_type]]
    # Output file
    file <- .try({
        .gdal_translate(
            file = file,
            base_file = base_file,
            params = list(
                "-ot" = data_type,
                "-of" = .conf("gdal_presets", "image", "of"),
                "-b" = rep(1, nlayers),
                "-scale" = list(0, 1, miss_value, miss_value),
                "-a_nodata" = miss_value,
                "-co" = .conf("gdal_presets", "image", "co")
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}

.gdal_template_block <- function(block, bbox, file, nlayers, miss_value,
                                 data_type) {
    # Get first file
    file <- file[[1]]
    # Convert to gdal data type
    data_type <- .gdal_data_type[[data_type]]
    # Output file
    file <- .try({
        .gdal_translate(
            file = file,
            # GDAL does not allow raster creation, to bypass this limitation
            # Let's base our raster creation by using a tiny template
            # (647 Bytes)
            base_file = system.file(
                "extdata/raster/gdal/template.tif", package = "sits"
            ),
            params = list(
                "-ot" = data_type,
                "-of" = .conf("gdal_presets", "block", "of"),
                "-b" = rep(1, nlayers),
                "-outsize" = list(.ncols(block), .nrows(block)),
                "-scale" = list(0, 1, miss_value, miss_value),
                "-a_srs" = .crs(bbox),
                "-a_ullr" = list(
                    .xmin(bbox), .ymax(bbox), .xmax(bbox), .ymin(bbox)
                ),
                "-a_nodata" = miss_value,
                "-co" = .conf("gdal_presets", "block", "co")
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}

.gdal_merge_into <- function(file, base_files, multicores) {
    # Merge src_files
    file <- .try({
        .gdal_warp(
            file = file,
            base_files = base_files,
            params = list(
                "-wo" = paste0("NUM_THREADS=", multicores),
                "-multi" = TRUE,
                "-q" = TRUE,
                "-overwrite" = FALSE
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}
