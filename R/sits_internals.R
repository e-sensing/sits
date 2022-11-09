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
    .default(lubridate::as_date(unlist(x, recursive = FALSE)))
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


#' @title Points accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These functions are accessors of `point` fields in a object tibble.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' it is removed from the object.
#'
#' `.lon()` and `.lat()` get/set, respectively, `"longitude"`
#' and `"latitude"` fields.
#'
#' @param x Object to get/set field value.
#' @param value Value to set on object field.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- c(longitude = "123")
#' .lon(x) # 123 as number
#' x <- list(longitude = 1:10)
#' .lat(x) <- 11:20
#' x # with 'longitude' and 'latitude' fields
#' }
#'
NULL

#' @title Get \code{'longitude'} field.
#' @noRd
.lon <- function(x) {
    .as_dbl(.compact(x[["longitude"]]))
}

#' @title Set \code{'longitude'} field as numeric.
#' @noRd
`.lon<-` <- function(x, value) {
    x[["longitude"]] <- .as_dbl(value)
    x
}

#' @title Get \code{'latitude'} field.
#' @noRd
.lat <- function(x) {
    .as_dbl(.compact(x[["latitude"]]))
}

#' @title Set \code{'latitude'} field as numeric.
#' @noRd
`.lat<-` <- function(x, value) {
    x[["latitude"]] <- .as_dbl(value)
    x
}

#' @title Point API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' A point represents a dimensionless geographical location in a given
#' projection. A \code{point} is any \code{list} or \code{tibble}
#' containing \code{longitude} and \code{latitude} fields. A \code{point} may
#' contains multiple entries.
#'
#' @param x Any object to extract a \code{point}.
#' @param ... Additional parameters.
#' @param point A \code{point}.
#' @param crs The point CRS. If not informed, default CRS is \code{'EPSG:4326'}.
#' @param as_crs A CRS to project \code{point}.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- list(a = 0, z = 0)
#' .point(x) # NULL
#' x <- list(a = 0, longitude = 1:3, b = 2:4, latitude = 2, z = 0)
#' .point(x)
#' .point_as_sf(x) # 3 features
#' .point_as_sf(x, as_crs = "EPSG:3857") # reprojected features
#' }
NULL

# point fields
.point_cols <- c("longitude", "latitude")

#' @title Does vector \code{x} has \code{point} fields?
#' @returns \code{.has_point()}: \code{logical}.
#' @noRd
.has_point <- function(x) {
    all(.point_cols %in% names(x))
}

#' @title Is vector \code{x} a \code{point} object?
#' @returns \code{.is_point()}: \code{logical}.
#' @noRd
.is_point <- function(x) {
    setequal(names(x), c(.point_cols, "crs"))
}

.check_point <- function(x) {
    if (!.is_point(x)) {
        stop("object is not a valid point")
    }
}

#' @title Extract a \code{point} from any given \code{vector}.
#' @returns \code{.point()}: \code{point}.
#' @noRd
.point <- function(x, crs = NULL, as_crs = NULL) {
    if (!.has_point(x)) {
        return(NULL)
    }
    if (!.has(crs)) crs <- "EPSG:4326"
    # Create point
    point <- .common_size(longitude = .lon(x), latitude = .lat(x), crs = crs)
    # Project to CRS
    if (.has(as_crs)) {
        point <- .point_as_sf(point = point, as_crs = as_crs)
    }
    # Return point
    point
}

#' @title Convert a \code{point} into a \code{sf} point object.
#' @returns \code{.point_as_sf()}: \code{sf}.
#' @noRd
.point_as_sf <- function(point, as_crs = NULL) {
    # Check for valid point
    .check_point(point)
    # Convert to sf object and return it
    geom <- sf::st_as_sf(
        x = point,
        coords = c("longitude", "latitude"),
        crs = .crs(point)
    )
    # Project CRS
    if (.has(as_crs)) {
        geom <- sf::st_transform(geom, crs = as_crs)
    }
    # Return geom
    geom
}

.point_coords <- function(point) {
    # Check for a valid points
    .check_is_sf(point)
    # Get coordinates from points
    pts <- sf::st_coordinates(point)
    colnames(pts) <- c("X", "Y")

    pts
}

.check_is_sf <- function(x) {
    .check_that(.is_sf(x))
}
