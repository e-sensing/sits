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
#' @keywords internal
#' @name data_type
NULL

#' @describeIn data_type Convert an input to \code{integer}. This is
#'   the same function as \code{as.integer()}.
#' @return \code{integer}
.as_int <- as.integer

#' @describeIn data_type Convert an input to \code{character}. This is
#'   the same function as \code{as.character()}.
#' @return \code{character}
.as_chr <- as.character

#' @describeIn data_type Convert an input to \code{numeric}. This is
#'   the same function as \code{as.numeric()}.
#' @return \code{numeric}
.as_dbl <- as.numeric

#' @describeIn data_type Convert an input to a date type. This is
#'   the same function as \code{lubridate::as_date()}.
#' @return \code{date}
.as_date <- function(x) {
    lubridate::as_date(unlist(x, recursive = FALSE))
}

#' @describeIn data_type Check if an input has a value or nor. Any zero length
#'   value of any type is evaluated as \code{FALSE}. This function is broader
#'   than \code{is.null()} that only accounts for \code{NULL} value.
#' @return \code{logical}
.has <- function(x) {
    length(x) > 0
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
#' This functions is a fancy implementation of \code{tryCatch()}. It
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
#' @param ... Additional parameter to be passed to \code{tryCatach()}.
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
#' @return Last expression evaluated in \code{expr}, if no error occurs.
#'   If an error occurs, the function returns the last expression
#'   evaluated in \code{.default} parameter. If \code{.default} parameter
#'   is not informed, the function will raise the error.
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

.intersects <- function(x, y) {
    as_crs <- sf::st_crs(x)
    y <- sf::st_transform(y, crs = as_crs)
    apply(sf::st_intersects(x, y, sparse = FALSE), 1, any)
}

.between <- function(x, min, max) {
    min <= x & x <= max
}

#---- generic accessors ----

#' bbox accessors
#'
#' These functions are accessors of bbox fields inside \code{vectors}.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length,
#' and the field is not \code{atomic} it is removed from the object.
#'
#' @param x Object to get/set field value.
#' @param value Value to set on object field.
#'
#' @examples
#' \dontrun{
#'
#' }
#' @keywords internal
#' @name bbox_accessors
NULL

#' @describeIn bbox_accessors Get \code{'xmin'} field.
#' @return \code{numeric}.
.xmin <- function(x) {
    .as_dbl(.compact(x[["xmin"]]))
}

#' @describeIn bbox_accessors Set \code{'xmin'} field.
#' @return Updated object \code{x}.
`.xmin<-` <- function(x, value) {
    x[["xmin"]] <- if (.has(x)) .as_dbl(value) else NULL
    x
}

#' @describeIn bbox_accessors Get \code{'xmax'} field.
#' @return \code{numeric}.
.xmax <- function(x) {
    .as_dbl(.compact(x[["xmax"]]))
}

#' @describeIn bbox_accessors Set \code{'xmax'} field.
#' @return Updated object \code{x}.
`.xmax<-` <- function(x, value) {
    x[["xmax"]] <- if (.has(x)) .as_dbl(value) else NULL
    x
}

#' @describeIn bbox_accessors Get \code{'ymin'} field.
#' @return \code{numeric}.
.ymin <- function(x) {
    .as_dbl(.compact(x[["ymin"]]))
}

#' @describeIn bbox_accessors Set \code{'ymin'} field.
#' @return Updated object \code{x}.
`.ymin<-` <- function(x, value) {
    x[["ymin"]] <- if (.has(x)) .as_dbl(value) else NULL
    x
}

#' @describeIn bbox_accessors Get \code{'ymax'} field.
#' @return \code{numeric}.
.ymax <- function(x) {
    .as_dbl(.compact(x[["ymax"]]))
}

#' @describeIn bbox_accessors Set \code{'ymax'} field.
#' @return Updated object \code{x}.
`.ymax<-` <- function(x, value) {
    x[["ymax"]] <- if (.has(x)) .as_dbl(value) else NULL
    x
}

#' @describeIn bbox_accessors Convert a CRS value to \code{character}.
#' @return \code{character}.
.as_crs <- function(x) {
    if (.has(x)) {
        if (is.character(x))
            x
        else if (is.numeric(x))
            paste0("EPSG:", x)
        stop("invalid crs value")
    } else
        NULL
}

#' @describeIn bbox_accessors Get \code{'crs'} field.
#' @return \code{character}.
.crs <- function(x) {
    crs <- .compact(x[["crs"]])
    .as_chr(crs)
}

#' @describeIn bbox_accessors Set \code{'crs'} field.
#' @return Updated object \code{x}.
`.crs<-` <- function(x, value) {
    x[["crs"]] <- .as_crs(value)
    x
}

# block accessors

.col <- function(x) {
    .as_int(.compact(x[["col"]]))
}

.row <- function(x) {
    .as_int(.compact(x[["row"]]))
}

.ncols <- function(x) {
    .as_int(.compact(x[["ncols"]]))
}

.nrows <- function(x) {
    .as_int(.compact(x[["nrows"]]))
}

.xres <- function(x) {
    (.xmax(x) - .xmin(x)) / .ncols(x)
}

.yres <- function(x) {
    (.ymax(x) - .ymin(x)) / .nrows(x)
}

#---- block API: ----

#' Block API
#'
#' A \code{block} is any \code{list} or \code{tibble} containing
#' \code{col}, \code{row}, \code{ncols}, and \code{nrows} fields.
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
#' @name block_api
NULL

# block fields
.block_cols <- c("col", "row", "ncols", "nrows")

#' @describeIn block_api Extract a \code{block} from any given
#' \code{vector}.
#'
#' @return \code{.block()}: \code{block}.
.block <- function(x) {
    if (!all(.block_cols %in% names(x))) {
        return(NULL)
    }
    as.list(x[.block_cols])
}

#' @describeIn block_api Compute the number of pixels for a
#' \code{block} considering an additional overlapping parameter.
#'
#' @return \code{.block_size()}: \code{numeric}.
.block_size <- function(block, overlap = 0) {
    (block[["nrows"]] + 2 * overlap) * (block[["ncols"]] + 2 * overlap)
}

#---- bbox API: ----

#' bbox API
#'
#' A \code{bbox} is any \code{list} or \code{tibble} containing \code{xmin},
#' \code{xmax}, \code{ymin}, \code{ymax}, and \code{crs} fields.
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
#' @name bbox_api
NULL

# bbox fields
.bbox_cols <- c("xmin", "xmax", "ymin", "ymax")

#' @describeIn bbox_api extract a \code{bbox} from any given
#' \code{vector}.
#'
#' @return \code{.bbox()}: \code{bbox}.
.bbox <- function(x, ..., default_crs = NULL) {
    if (!all(.bbox_cols %in% names(x))) {
        return(NULL)
    }
    if ("crs" %in% names(x)) {
        return(as.list(x[c(.bbox_cols, "crs")]))
    }
    if (is.null(default_crs)) {
        warning("object has no crs, assuming 'EPSG:4326'")
        default_crs <- "EPSG:4326"
    }
    as.list(c(x[.bbox_cols], list(crs = default_crs)))
}

#' @describeIn bbox_api Convert a \code{bbox} into a
#' \code{sf} polygon object.
#'
#' @return \code{.bbox_as_sf()}: \code{sf}.
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

#---- ROI API: ----

#' ROI API
#'
#' A ROI (or 'Region of Interest') represents an geographic area. There are
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
#' @seealso \code{\link{.bbox}()}
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
#' @name roi_api
NULL

# roi 'lonlat' fields
.roi_lonlat_cols <- c("lon_min", "lon_max", "lat_min", "lat_max")

#' @describeIn roi_api Tells which type of ROI is in \code{roi}
#' parameter (One of \code{'sf'}, \code{'bbox'}, or \code{'lonlat'}).
#'
#' @return \code{.roi_type()}: \code{character}.
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
#' @return \code{.roi_switch()}: one of the arguments in \code{...}.
.roi_switch <- function(roi, ...) {
    switch(.roi_type(roi),
           ...
    )
}

#' @describeIn roi_api Converts \code{roi} to an \code{sf} object.
#'
#' @return \code{.roi_as_sf()}: \code{sf}.
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

#---- Period API: ----

#' Period API
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
#' @name period_api
NULL

#' @describeIn period_api Check if a character string is a valid
#' \code{period}.
#'
#' @return \code{.period_check()}: nothing.
.period_check <- function(period) {
    if (!grepl("^P[0-9]+[DMY]$", period)) {
        stop("invalid period format")
    }
}

#' @describeIn period_api Return the value part of a
#' \code{period}.
#'
#' @return \code{.period_val()}: integer.
.period_val <- function(period) {
    .period_check(period)
    .as_dbl(gsub("^P([0-9]+)[DMY]$", "\\1", period))
}

#' @describeIn period_api Return the unit of a \code{period}.
#' Can be one of \code{'day'}, \code{'month'}, or \code{'year'}.
#'
#' @return \code{.period_unit()}: character.
.period_unit <- function(period) {
    .period_check(period)
    unit <- c(D = "day", M = "month", Y = "year")
    unit[[gsub("^P[0-9]+([DMY])$", "\\1", period)]]
}

#---- Config: ----

.conf_exists <- function(...) {
    key <- c(...)
    !is.null(.try(sits_env[["config"]][[key]], .default = NULL))
}

.conf <- function(...) {
    key <- c(...)
    if (!.conf_exists(key)) {
        stop("key '", paste(key, collapse = "->"), "' not found in config")
    }
    sits_env[["config"]][[c(key)]]
}

# eo_cube

.conf_eo_band_exists <- function(source, collection, band) {
    .conf_exists("sources", source, "collections", collection, "bands", band)
}

.conf_eo_band <- function(source, collection, band) {
    if (!.conf_eo_band_exists(source, collection, band)) {
        return(.conf("default_values", "eo_cube"))
    }
    .conf("sources", source, "collections", collection, "bands", band)
}

# derived_cube

.conf_derived_s3class <- function(derived_class) {
    .conf("derived_cube", derived_class, "s3_class")
}

.conf_derived_band <- function(derived_class, band) {
    .conf("derived_cube", derived_class, "bands", band)
}

.band_data_type <- function(conf) {
    .as_chr(conf[["data_type"]][[1]])
}

.band_miss_value <- function(conf) {
    .as_dbl(conf[["missing_value"]][[1]])
}

.band_min_value <- function(conf) {
    .as_dbl(conf[["minimum_value"]][[1]])
}

.band_max_value <- function(conf) {
    .as_dbl(conf[["maximum_value"]][[1]])
}

.band_scale <- function(conf) {
    .as_dbl(conf[["scale_factor"]][[1]])
}

.band_offset <- function(conf) {
    .as_dbl(conf[["offset_value"]][[1]])
}

.band_cloud_interp_values <- function(conf) {
    .as_int(conf[["interp_values"]])
}

.band_cloud_bit_mask <- function(conf) {
    .as_int(conf[["bit_mask"]][[1]])
}

.band_cloud <- function() {
    "CLOUD"
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
        path = file
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

#---- | .tile() ----

#' Tile API
#'
#' Get first tile of a cube.
#'
#' @param cube A cube.
#'
#' @return tile
.tile <- function(cube) {
    UseMethod(".tile", cube)
}

#' @export
.tile.raster_cube <- function(cube) {
    cube[1, ]
}

#---- | .tile_source() ----
#' Tile API
#'
#' Get tile source field.
#'
#' @param tile A tile.
#'
#' @return character
.tile_source <- function(tile) {
    UseMethod(".tile_source", tile)
}

#' @export
.tile_source.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["source"]])
}

#---- | .tile_collection() ----
#' Tile API

#' Get tile collection field.

#' @param tile A tile.

#' @return character
.tile_collection <- function(tile) {
    UseMethod(".tile_collection", tile)
}

#' @export
.tile_collection.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["collection"]])
}

#---- | .tile_name() ----
#' Tile API
#'
#' Get tile name field.
#'
#' @param tile A tile.
#'
#' @return character
.tile_name <- function(tile) {
    UseMethod(".tile_name", tile)
}

#' @export
.tile_name.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["tile"]])
}

#---- | .tile_labels() ----
#' Tile API
#'
#' Get/set tile labels field.
#'
#' @param tile A tile.
#'
#' @return character
.tile_labels <- function(tile) {
    UseMethod(".tile_labels", tile)
}

#' @export
.tile_labels.raster_cube <- function(tile) {
    tile <- .tile(tile)
    .as_chr(tile[["labels"]][[1]])
}

#' @name .tile_labels
#'
#' @param value Label character vector.
#'
#' @return tile
`.tile_labels<-` <- function(tile, value) {
    UseMethod(".tile_labels<-", tile)
}

#' @export
`.tile_labels<-.raster_cube` <- function(tile, value) {
    tile <- .tile(tile)
    tile[["labels"]] <- list(.as_chr(value))
    tile
}

#---- | .tile_file_info() ----
#' Tile API
#'
#' Get/set tile file_info field.
#'
#' @param tile A tile.
#'
#' @return file_info
.tile_file_info <- function(tile) {
    UseMethod(".tile_file_info", tile)
}

#' @export
.tile_file_info.raster_cube <- function(tile) {
    .fi(tile) # Get first file_info
}

#' @name .tile_file_info
#'
#' @param value A file_info tibble.
#'
#' @return tile
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

#---- | .tile_ncols() ----
#' Tile API
#'
#' Get number of image columns from \code{ncols} field (if it exists)
#' or from unique \code{ncols} in file_info (which can be more than one if tile
#' images has multiples \code{ncols}).
#'
#' @param tile A tile.
#'
#' @return integer
.tile_ncols <- function(tile) {
    UseMethod(".tile_ncols", tile)
}

#' @export
.tile_ncols.raster_cube <- function(tile) {
    if ("ncols" %in% tile) {
        return(.ncols(tile)[[1]])
    }
    unique(.ncols(.fi(tile)))
}

#---- | .tile_nrows() ----
#' Tile API
#'
#' Get number of image columns from \code{nrows} field (if it exists)
#' or from unique \code{nrows} in file_info (which can be more than one if tile
#' images has multiples \code{nrows}).
#'
#' @param tile A tile.
#'
#' @return integer
.tile_nrows <- function(tile) {
    UseMethod(".tile_nrows", tile)
}

#' @export
.tile_nrows.raster_cube <- function(tile) {
    if ("nrows" %in% tile) {
        return(.nrows(tile)[[1]])
    }
    unique(.nrows(.fi(tile)))
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
    miss_value <- .band_miss_value(band_conf)
    if (.has(miss_value)) {
        values[values == miss_value] <- NA
    }
    min_value <- .band_min_value(band_conf)
    if (.has(min_value)) {
        values[values < min_value] <- if (replace_by_minmax) min_value else NA
    }
    max_value <- .band_max_value(band_conf)
    if (.has(max_value)) {
        values[values > max_value] <- if (replace_by_minmax) max_value else NA
    }
    scale <- .band_scale(band_conf)
    if (.has(scale) && scale != 1) {
        values <- values * scale
    }
    offset <- .band_offset(band_conf)
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
    interp_values <- .band_cloud_interp_values(cloud_conf)
    is_bit_mask <- .band_cloud_bit_mask(cloud_conf)
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
        ncols = .tile_ncols(tile),
        nrows = .tile_nrows(tile),
        xmin = .xmin(tile),
        xmax = .xmax(tile),
        ymin = .ymin(tile),
        ymax = .ymax(tile),
        crs = .crs(tile)
    )
}

#---- | .tile_feature_create() ----

.cube_feature_create <- function(cube) {
    .cube_foreach_tile(cube, .tile_feature_create)
}

.tile_feature_create <- function(tile) {
    tile <- .tile(tile)
    features <- tile[, c("tile", "file_info")]
    features <- tidyr::unnest(features, "file_info")
    features[["feature"]] <- features[["fid"]]
    features <- tidyr::nest(features, file_info = -c("tile", "feature"))
    tile <- tile[rep(1, nrow(features)), ]
    tile[["file_info"]] <- features[["file_info"]]
    tile
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
        block_files = block_files, data_type = .band_data_type(band_conf),
        missing_value = .band_miss_value(band_conf), multicores = multicores
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
        block_files = block_files, data_type = .band_data_type(band_conf),
        missing_value = .band_miss_value(band_conf), multicores = multicores
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

#---- chunk ----

.chunks_create <- function(block,
                           overlap,
                           ncols,
                           nrows,
                           xmin,
                           xmax,
                           ymin,
                           ymax,
                           crs) {
    # Prepare raster tile template
    r_obj <- .raster_new_rast(
        nrows = nrows,
        ncols = ncols,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        nlayers = 1,
        crs = crs
    )
    chunks <- purrr::cross_df(list(
        col = seq(1, ncols, .ncols(block)),
        row = seq(1, nrows, .nrows(block))
    ))
    chunks[["col"]] <- .as_int(pmax(1, .col(chunks) - overlap))
    chunks[["row"]] <- .as_int(pmax(1, .row(chunks) - overlap))
    chunks[["ncols"]] <- .as_int(pmin(ncols, .col(chunks) +
                                          .ncols(block) + overlap - 1) -
                                     .col(chunks) + 1)
    chunks[["nrows"]] <- .as_int(pmin(nrows, .row(chunks) +
                                          .nrows(block) + overlap - 1) -
                                     .row(chunks) + 1)
    chunks <- slider::slide_dfr(chunks, function(chunk, template) {
        # Crop block from template
        r_obj <-
            .raster_crop_metadata(r_obj = template, block = .block(chunk))
        # Add bbox information
        .xmin(chunk) <- .raster_xmin(r_obj = r_obj)
        .xmax(chunk) <- .raster_xmax(r_obj = r_obj)
        .ymin(chunk) <- .raster_ymin(r_obj = r_obj)
        .ymax(chunk) <- .raster_ymax(r_obj = r_obj)
        .crs(chunk) <- .raster_crs(r_obj = r_obj)
        chunk
    }, template = r_obj)
    # Chunk overlap
    chunks[["overlap"]] <- .as_int(overlap)
    # Chunk size without overlap
    chunks[["crop_ncols"]] <- .as_int(pmin(
        ncols - .col(chunks) + 1,
        .ncols(chunks) - overlap
    ))
    chunks[["crop_nrows"]] <- .as_int(pmin(
        nrows - .row(chunks) + 1,
        .nrows(chunks) - overlap
    ))
    # Return chunks
    chunks
}

.chunks_block_no_overlap <- function(chunk) {
    list(
        col = .as_int(pmin(chunk[["overlap"]] + 1, .col(chunk))),
        row = .as_int(pmin(chunk[["overlap"]] + 1, .row(chunk))),
        ncols = .as_int(chunk[["crop_ncols"]]),
        nrows = .as_int(chunk[["crop_nrows"]])
    )
}

.chunks_filter_spatial <- function(chunks, roi) {
    chunks[.intersects(.bbox_as_sf(chunks), .roi_as_sf(roi)), ]
}
