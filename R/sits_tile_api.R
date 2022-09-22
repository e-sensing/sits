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
#' @family data types
#' @keywords internal
#' @name data_type
NULL

#' @describeIn data_type Convert an input to \code{integer}.
#'
#' @return \code{integer} or \code{NULL} if value is empty.
.as_int <- function(x) {
    if (.has(x)) as.integer(x) else NULL
}
#' @describeIn data_type Convert an input to \code{character}.
#'
#' @return \code{character} or \code{NULL} if value is empty.
.as_chr <- function(x) {
    if (.has(x)) as.character(x) else NULL
}

#' @describeIn data_type Convert an input to \code{numeric}.
#'
#' @return \code{numeric} or \code{NULL} if value is empty.
.as_dbl <- function(x) {
    if (.has(x)) as.numeric(x) else NULL
}

#' @describeIn data_type Convert an input to a date type. This is
#'   the same function as \code{lubridate::as_date()}.
#'
#' @return \code{date} or \code{NULL} if value is empty.
.as_date <- function(x) {
    if (.has(x)) lubridate::as_date(unlist(x, recursive = FALSE)) else NULL
}

#' @describeIn data_type Check if an input has a value or not. Any zero length
#'   value of any type is evaluated as \code{FALSE}. This function is broader
#'   than \code{is.null()} that only accounts for \code{NULL} value.
#' @return \code{logical}
.has <- function(x) {
    length(x) > 0
}

#' @describeIn data_type Check if an input has names or not. If there is
#'   any element without a name the function evaluates as \code{FALSE}.
#' @return \code{logical}
.has_name <- function(x) {
    if (.has(names(x))) return(names(x) != "")
    rep(FALSE, length(x))
}

#' @describeIn data_type Set \code{class} of object \code{x}.
#' @return Updated object \code{x}.
.set_class <- function(x, ...) {
    class(x) <- unique(c(...))
    x
}

#' @describeIn data_type Evaluates unique values of \code{x}. If there is
#'   only one unique value, return it. Otherwise return all \code{x}.
#' @return Same value as \code{x} or the unique value in \code{x} (if
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
#' @seealso \link{tryCatch()}
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

.by <- function(data, col, fn, ...) {
    unname(c(by(data, data[[col]], fn, ...)))
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

#' @describeIn band_accessors Return name of the cloud band.
.band_cloud <- function() {
    "CLOUD"
}

#---- block API: ----

#' Block API
#'
#' A block represents a region of a matrix. A \code{block} is any
#' \code{list} or \code{tibble} containing \code{col}, \code{row},
#' \code{ncols}, and \code{nrows} fields.
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
#' x <- list(a = 0, col = 1, row = 2, ncols = 3, nrows = 4, z = 0)
#' .block(x)
#' .block_size(x, 0)
#' .block_size(x, 2)
#' }
#'
#' @seealso \link{block_accessors}
#' @family region objects API
#' @keywords internal
#' @name block_api
NULL

# block fields
.block_cols <- c("col", "row", "ncols", "nrows")

#' @describeIn block_api Extract a \code{block} from any given
#' \code{vector}.
#'
#' @returns \code{.block()}: \code{block}.
.block <- function(x) {
    if (!all(.block_cols %in% names(x))) {
        return(NULL)
    }
    as.list(x[.block_cols])
}

#' @describeIn block_api Compute the number of pixels for a
#' \code{block} considering an additional overlapping parameter.
#'
#' @returns \code{.block_size()}: \code{integer}.
.block_size <- function(block, overlap = 0) {
    (block[["nrows"]] + 2 * overlap) * (block[["ncols"]] + 2 * overlap)
}

#---- bbox API: ----

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

#' @describeIn bbox_api extract a \code{bbox} from any given
#' \code{vector}.
#'
#' @returns \code{.bbox()}: \code{bbox}.
.bbox <- function(x, ..., default_crs = NULL) {
    if (!all(.bbox_cols %in% names(x))) {
        return(NULL)
    }
    xmin <- pmin(x[["xmin"]], x[["xmax"]])
    xmax <- pmax(x[["xmin"]], x[["xmax"]])
    ymin <- pmin(x[["ymin"]], x[["ymax"]])
    ymax <- pmax(x[["ymin"]], x[["ymax"]])
    if ("crs" %in% names(x)) {
        crs <- x[["crs"]]
    } else {
        crs <- default_crs
        if (is.null(default_crs)) {
            warning("object has no crs, assuming 'EPSG:4326'")
            crs <- "EPSG:4326"
        }
    }
    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, crs = crs)
}

#' @describeIn bbox_api Convert a \code{bbox} into a
#' \code{sf} polygon object.
#'
#' @returns \code{.bbox_as_sf()}: \code{sf}.
.bbox_as_sf <- function(bbox, ..., default_crs = NULL, as_crs = NULL) {
    bbox <- .bbox(bbox, default_crs = default_crs)
    if (!all(c(.bbox_cols, "crs") %in% names(bbox))) {
        stop("object does not have a valid bbox")
    }
    # Check if there are multiple CRS in bbox
    if (length(.crs(bbox)) > 1 && is.null(as_crs)) {
        warning(
            "object has multiples crs values, reprojecting to ",
            "EPSG:4326\n", "(use 'as_crs' to reproject to a ",
            "different crs value)"
        )
        as_crs <- "EPSG:4326"
    }
    # Convert to sf object and return it
    purrr::pmap_dfr(as.list(bbox), function(xmin, xmax, ymin, ymax, crs, ...) {
        geom <- sf::st_sf(
            geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(
                    c(xmin, ymax), c(xmax, ymax), c(xmax, ymin),
                    c(xmin, ymin), c(xmin, ymax)
                )
            ))), crs = crs
        )
        # Project CRS
        if (!is.null(as_crs)) {
            geom <- sf::st_transform(geom, crs = as_crs)
        }
        geom
    })
}

#---- roi API: ----

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

#---- chunks API: ----

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

#---- period API: ----

#' period API
#'
#' According to ISO-8601 a duration is the amount of intervening time
#' in a time interval. Here, we use a simplified representation of a duration
#' that we call \code{period}.
#'
#' It is represented by the the regular expression
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
#' .conf_exists("not_existing_entry") # FALSE
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
    exists <- !is.null(.try(sits_env[["config"]][[key]], .default = NULL))
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
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   band = "NDVI"
#' )
#' # get configuration for band NDVI of 'BDC -> MOD13Q1-6' collection
#' x <- .conf_eo_band(
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   band = "NDVI"
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
    band <- toupper(band)
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
#' .conf_derived_s3class("probs")
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
    # derived_class and band are lowercase
    derived_class <- tolower(derived_class)
    band <- tolower(band)
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


#---- fi API: ----

.fi_type <- function(fi) {
    if ("date" %in% names(fi)) {
        "eo_cube"
    } else if (all(c("start_date", "end_date") %in% names(fi))) {
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
    # Create a new earth observation file_info
    tibble::tibble(
        fid = fid, band = band,
        date = date,
        ncols = ncols,
        nrows = nrows,
        xres = xres,
        yres = yres,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        path = path
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

.fi_derived <- function(band,
                        start_date,
                        end_date,
                        ncols,
                        nrows,
                        xres,
                        yres,
                        xmin,
                        xmax,
                        ymin,
                        ymax,
                        crs,
                        path) {
    # Create a new derived file_info
    tibble::tibble(
        band = band,
        start_date = start_date,
        end_date = end_date,
        ncols = ncols,
        nrows = nrows,
        xres = xres,
        yres = yres,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        crs = crs,
        path = path
    )
}

.fi_derived_from_file <-
    function(file, band, start_date, end_date) {
        file <- path.expand(file)
        r_obj <- .raster_open_rast(file)
        .fi_derived(
            band = band,
            start_date = start_date,
            end_date = end_date,
            ncols = .raster_ncols(r_obj),
            nrows = .raster_nrows(r_obj),
            xres = .raster_xres(r_obj),
            yres = .raster_yres(r_obj),
            xmin = .raster_xmin(r_obj),
            xmax = .raster_xmax(r_obj),
            ymin = .raster_ymin(r_obj),
            ymax = .raster_ymax(r_obj),
            crs = .raster_crs(r_obj),
            path = file
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

.fi_band_filter <- function(fi, band) {
    fi[.fi_bands(fi) %in% band, ]
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

.fi_temporal_filter <- function(fi, start_date, end_date) {
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

.fi_spatial_filter <- function(fi, roi) {
    fi[.fi_intersects(fi, roi), ]
}

.fi_read_block <- function(fi, band, block) {
    fi <- .fi_band_filter(fi = fi, band = band)
    files <- .fi_paths(fi)
    if (!.has(files)) {
        return(NULL)
    }

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
        source = .tile_source(tile),
        collection = .tile_collection(tile),
        band = band[[1]]
    )
}

#' @export
.tile_band_conf.derived_cube <- function(tile, band) {
    .conf_derived_band(derived_class = .tile_derived_class(tile), band = band)
}

#---- | .tile_band_filter() ----
#' Tile API
#'
#' Filter file_info entries of a given \code{band}.
#'
#' @param tile A tile.
#' @param band A region of interest (ROI).
#'
#' @return tile
.tile_band_filter <- function(tile, band) {
    UseMethod(".tile_band_filter", tile)
}

#' @export
.tile_band_filter.raster_cube <- function(tile, band) {
    tile <- .tile(tile)
    .tile_file_info(tile) <-
        .fi_band_filter(fi = .fi(tile), band = band)
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

#---- | .tile_spatial_filter() ----
#' Tile API
#'
#' Filter file_info entries that intersect \code{roi} parameter.
#'
#' @param tile A tile.
#' @param roi A region of interest (ROI).
#'
#' @return tile
.tile_spatial_filter <- function(tile, roi) {
    UseMethod(".tile_spatial_filter", tile)
}

#' @export
.tile_spatial_filter.raster_cube <- function(tile, roi) {
    tile <- .tile(tile)
    .tile_file_info(tile) <-
        .fi_spatial_filter(fi = .fi(tile), roi = roi)
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
        fi = .fi(tile),
        start_date = start_date,
        end_date = end_date
    ))
}

#---- | .tile_temporal_filter() ----
#' Tile API
#'
#' Filter file_info entries by 'start_date' and 'end_date.'
#'
#' @param tile A tile.
#' @param start_date,end_date Date of start and end.
#'
#' @return tile
.tile_temporal_filter <- function(tile, start_date, end_date) {
    UseMethod(".tile_temporal_filter", tile)
}

#' @export
.tile_temporal_filter.raster_cube <-
    function(tile, start_date, end_date) {
        tile <- .tile(tile)
        .tile_file_info(tile) <- .fi_temporal_filter(
            fi = .fi(tile),
            start_date = start_date,
            end_date = end_date
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
#' @param tile A tile.
#' @param band Band character vector.
#' @param block A block list with (col, row, ncols, nrows).
#' @param replace_by_minmax Should values out of minimum and maximum range
#'   be replaced by minimum and maximum value? If \code{FALSE}, values out
#'   of range will be replaced by \code{NA}.
#'
#' @return numeric
.tile_read_block <- function(tile, band, block, replace_by_minmax) {
    UseMethod(".tile_read_block", tile)
}

#' @export
.tile_read_block.raster_cube <- function(tile, band, block, replace_by_minmax) {
    fi <- .fi(tile)
    values <- .fi_read_block(
        fi = fi,
        band = band,
        block = block
    )
    if (!.has(values)) {
        return(NULL)
    }


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
        values[values < min_value] <- if (replace_by_minmax) min_value else NA
    }
    max_value <- .max_value(band_conf)
    if (.has(max_value)) {
        values[values > max_value] <- if (replace_by_minmax) max_value else NA
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
    values <- .tile_read_block(
        tile = tile,
        band = .band_cloud(),
        block = block,
        replace_by_minmax = FALSE
    )
    if (!.has(values)) {
        return(NULL)
    }


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
.tile_probs_from_file <- function(file, band, base_tile, labels,
                                  update_bbox) {
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

#---- | .cube_band_conf() ----
.cube_band_conf <- function(cube, band) {
    UseMethod(".cube_band_conf", cube)
}

#' @export
.cube_band_conf.raster_cube <- function(cube, band) {
    .tile_band_conf(tile = cube, band = band)
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

#---- | .cube_spatial_filter() ----
#' @title Cube API
#' @param cube A cube.
#' @param roi A region of interest (ROI).
#' @description Filter tiles that intersect \code{roi} parameter.
#' @return cube
.cube_spatial_filter <- function(cube, roi) {
    UseMethod(".cube_spatial_filter", cube)
}

#' @export
.cube_spatial_filter.raster_cube <- function(cube, roi) {
    cube[.cube_intersects(cube, roi), ]
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
    slider::slide_lgl(cube,
                      .tile_during,
                      start_date = start_date,
                      end_date = end_date
    )
}

#---- | .cube_temporal_filter() ----
#' Cube API
#'
#' Filter tiles with 'file_info' entries between 'start_date'
#' and 'end_date'.
#'
#' @param cube A cube.
#' @param start_date,end_date Date of start and end.
#'
#' @return cube
.cube_temporal_filter <- function(cube, start_date, end_date) {
    UseMethod(".cube_temporal_filter", cube)
}

#' @export
.cube_temporal_filter.raster_cube <-
    function(cube, start_date, end_date) {
        cube[.cube_during(cube, start_date, end_date), ]
    }

#---- | .cube_band_filter() ----

.cube_band_filter <- function(cube, band) {
    UseMethod(".cube_band_filter", cube)
}

.cube_band_filter.raster_cube <- function(cube, band) {
    .cube_foreach_tile(cube, function(tile) {
        .tile_band_filter(tile = tile, band = band)
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

#---- | .cube_merge_features() ----

.cube_feature_create <- function(tile) {
    features <- tile[, c("tile", "file_info")]
    features <- tidyr::unnest(features, "file_info")
    features[["feature"]] <- features[["fid"]]
    features <- tidyr::nest(features, file_info = -c("tile", "feature"))
    tile <- tile[rep(1, nrow(features)), ]
    tile[["file_info"]] <- features[["file_info"]]
    tile
}

.cube_merge_features <- function(features) {
    cube <- tidyr::nest(
        tidyr::unnest(features, "file_info", names_sep = "."),
        file_info = tidyr::starts_with("file_info"), .names_sep = "."
    )
    class(cube) <- class(features)
    cube
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
# .cube_spatial_filter(s2_cube, .bbox_as_sf(s2_cube[3:4,]))
# .cube_spatial_filter(s2_cube, .bbox_as_sf(s2_cube[3:4,], as_crs = 4326))
# .cube_start_date(s2_cube)
# .cube_end_date(s2_cube)
# .cube_timeline(s2_cube)
# .cube_temporal_filter(s2_cube, start_date = "2017-01-01", end_date = "2018-07-12")
# .cube_temporal_filter(s2_cube, start_date = "2017-01-01", end_date = "2018-07-14")
# .cube_temporal_filter(s2_cube, start_date = "2019-07-25", end_date = "2020-07-28")
# .cube_temporal_filter(s2_cube, start_date = "2019-07-28", end_date = "2020-07-28")
# .cube_temporal_filter(s2_cube, start_date = "2014-07-28", end_date = "2015-07-28")
#
# sinop <- sits_cube(
#     source = "BDC",
#     collection = "MOD13Q1-6",
#     data_dir = system.file("extdata/raster/mod13q1", package = "sits")
# )
# .cube_timeline(sinop)
# .cube_start_date(sinop)
# .cube_end_date(sinop)

#---- ml_model ----


.ml_stats_create <- function(samples) {
    ts <- dplyr::bind_rows(samples[["time_series"]])
    # Remove Index column
    ts <- dplyr::select(ts, -.data[["Index"]])
    # Compute median
    med <- dplyr::summarise(
        ts,
        dplyr::across(dplyr::everything(), stats::median, na.rm = TRUE)
    )
    # Compute quantile 0.02
    q02 <- dplyr::summarise(
        ts,
        dplyr::across(
            dplyr::everything(),
            stats::quantile,
            probs = 0.02,
            na.rm = TRUE
        )
    )
    # Compute quantile 0.98
    q98 <- dplyr::summarise(
        ts,
        dplyr::across(
            dplyr::everything(),
            stats::quantile,
            probs = 0.98,
            na.rm = TRUE
        )
    )
    # Return stats object
    dplyr::bind_rows(med, q02, q98)
}

.ml_stats <- function(ml_model) {
    environment(ml_model)[["stats"]]
}

.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}

.ml_class <- function(ml_model) {
    class(ml_model)[[1]]
}

.ml_attr_names <- function(ml_model) {
    # Get attr names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2:0]
}

.ml_bands <- function(ml_model) {
    .sits_bands(.ml_samples(ml_model))
}

.ml_labels <- function(ml_model) {
    .sits_labels(.ml_samples(ml_model))
}

.ml_foreach_band <- function(ml_model, fn, ...) {
    purrr::map(.ml_bands(ml_model), fn, ...)
}

#---- stats ----
.stats_q02_band <- function(stats, band) {
    quantile_02 <- 2
    stats[[band]][[quantile_02]]
}

.stats_q98_band <- function(stats, band) {
    quantile_98 <- 3
    stats[[band]][[quantile_98]]
}

#---- sits (samples) ----

.sits_bands <- function(samples) {
    setdiff(names(samples[["time_series"]][[1]]), "Index")
}

.sits_select <- function(samples, bands) {
    .sits_fast_apply(samples, col = "time_series", function(x) {
        dplyr::select(x, dplyr::all_of(c("#..", "Index", bands)))
    })
}

.sits_labels <- function(samples) {
    sort(unique(samples[["label"]]))
}

.distances <- function(samples, bands, label_col = TRUE) {
    label <- NULL
    if (label_col) {
        samples <- dplyr::mutate(samples, reference = .data[["label"]])
        label <- "reference"
    }
    samples <- samples[c(label, "time_series")]
    samples[["original_row"]] <- seq_len(nrow(samples))
    samples <- tidyr::unnest(samples, "time_series")
    samples <- samples[c("original_row", label, bands)]
    samples <- dplyr::group_by(samples, .data[["original_row"]])
    samples <- dplyr::mutate(samples, index = seq_len(dplyr::n()))
    samples <- dplyr::ungroup(samples)
    # Arrange data: samples x bands/index
    samples <- tidyr::pivot_wider(
        samples,
        names_from = "index",
        values_from = bands,
        names_prefix = ifelse(length(bands) == 1, bands, ""),
        names_sep = ""
    )
    # Remove 'id' column
    return(samples)
}

.sits_normalize <- function(samples, stats) {
    .apply_across(samples, function(b) {
        band <- dplyr::cur_column()
        quant_02 <- .stats_q02_band(stats, band)
        quant_98 <- .stats_q98_band(stats, band)
        c(normalize_data(as.matrix(b), quant_02, quant_98))
    })
}

.sits_get_chunk_ts <- function(samples, nchunks) {
    ngroup <- ceiling(nrow(samples) / nchunks)
    group_id <-
        rep(seq_len(nchunks), each = ngroup)[seq_len(nrow(samples))]

    samples[["group_id"]] <- group_id
    dplyr::group_split(dplyr::group_by(samples, .data[["group_id"]]),
                       .keep = FALSE
    )
}

