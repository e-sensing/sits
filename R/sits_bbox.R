#' @title Get the bounding box of the data
#'
#' @name sits_bbox
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Obtain a vector of limits (either on lat/long for time series
#'               or in projection coordinates in the case of cubes)
#'
#' @param data   \code{samples} data or \code{cube}.
#' @param crs    CRS of the samples points.
#' @param as_crs CRS to project the resulting \code{bbox}.
#' @param ...    Additional parameters.
#'
#' @return A \code{bbox}.
#'
#' @examples
#' if (sits_run_examples()) {
#' sits_bbox(samples_modis_ndvi)
#' }
#' @export
#'
sits_bbox <- function(data, ..., as_crs = NULL) {

    # Get the meta-type (sits or cube)
    data <- .conf_data_meta_type(data)

    UseMethod("sits_bbox", data)
}

#' @rdname sits_bbox
#' @export
#'
sits_bbox.sits <- function(data, ..., crs = "EPSG:4326", as_crs = NULL) {

    # Pre-conditions
    .check_samples(data)

    # Convert to bbox
    bbox <- .bbox(.point(x = data, crs = crs, as_crs = as_crs))

    return(bbox)
}
#' @rdname sits_bbox
#' @export
#'
sits_bbox.sits_cube <- function(data, ..., as_crs = NULL) {

    # Pre-condition
    .check_is_sits_cube(data)

    # Convert to bbox
    bbox <- .bbox(x = data, as_crs = as_crs)

    return(bbox)
}

#' @title Intersection between a bounding box and a cube
#' @name .sits_bbox_intersect
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param bbox           Bounding box for a region of interest.
#' @param cube           Data cube.
#' @return               Vector the bounding box intersection.
#'
.sits_bbox_intersect <- function(bbox, cube) {
    bbox_out <- vector("double", length = 4)
    names(bbox_out) <- c("xmin", "ymin", "xmax", "ymax")

    if (bbox["xmin"] > cube$xmax |
        bbox["ymin"] > cube$ymax |
        bbox["xmax"] < cube$xmin |
        bbox["ymax"] < cube$ymin) {
        return(NULL)
    }

    if (bbox["xmin"] < cube$xmin) {
        bbox_out["xmin"] <- cube$xmin
    } else {
        bbox_out["xmin"] <- bbox["xmin"]
    }

    if (bbox["ymin"] < cube$ymin) {
        bbox_out["ymin"] <- cube$ymin
    } else {
        bbox_out["ymin"] <- bbox["ymin"]
    }

    if (bbox["xmax"] > cube$xmax) {
        bbox_out["xmax"] <- cube$xmax
    } else {
        bbox_out["xmax"] <- bbox["xmax"]
    }

    if (bbox["ymax"] > cube$ymax) {
        bbox_out["ymax"] <- cube$ymax
    } else {
        bbox_out["ymax"] <- bbox["ymax"]
    }

    return(bbox_out)
}

#---- bbox API: ----

#' Bbox API
#'
#' A bounding box represents a rectangular geographical region in a certain
#' projection. A \code{bbox} is any \code{list} or \code{tibble} containing
#' \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}, and \code{crs} fields.
#' A \code{bbox} may contains multiple entries.
#'
#' @param x      Any object to extract a \code{bbox}.
#' @param ...    Parameters to be evaluated accordingly to input object.
#' @param default_crs If no CRS is present in \code{x}, which CRS should be
#'   used? If \code{NULL} default CRS will be \code{'EPSG:4326'}.
#' @param bbox   A \code{bbox}.
#' @param as_crs A CRS to project \code{bbox}. Useful if bbox has
#'   multiples CRS.
#'
#' @examples
#' if (sits_run_examples()) {
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
#' @noRd
NULL

# bbox fields
.bbox_cols <- c("xmin", "xmax", "ymin", "ymax")

#' @describeIn bbox_api Does vector \code{x} has \code{bbox} fields?
#' @returns \code{.has_bbox()}: \code{logical}.
#' @noRd
.has_bbox <- function(x) {
    all(.bbox_cols %in% names(x))
}

.is_bbox <- function(x) {
    setequal(names(x), c(.bbox_cols, "crs"))
}

.check_bbox <- function(x) {
    if (!.is_bbox(x)) {
        stop("object is not a valid bbox")
    }
}

#' @describeIn bbox_api Tells which bbox type is in \code{x}
#'   parameter (One of \code{'sf'}, \code{'df'}, or \code{'point'}).
#' @returns \code{.bbox_type()}: \code{character}.
#' @noRd
.bbox_type <- function(x) {
    if (inherits(x, c("sf", "sfc"))) {
        "sf"
    } else if (.has_bbox(x)) {
        "df"
    } else if (.is_point(x)) {
        "point"
    } else {
        stop("cannot extract bbox from object of class ", class(x))
    }
}

#' @describeIn bbox_api Chooses one of the arguments passed in
#'   \code{...} according to which type of \code{bbox} parameter.
#' @returns \code{.bbox_switch()}: one of the arguments in \code{...}.
#' @noRd
.bbox_switch <- function(x, ...) {
    switch(.bbox_type(x), ...)
}

#' @describeIn bbox_api Extract a \code{bbox} from any given \code{object}.
#' @returns \code{.bbox()}: \code{bbox}.
#' @noRd
.bbox <- function(x, default_crs = NULL, as_crs = NULL) {
    x <- .bbox_switch(
        x = x,
        sf = .bbox_from_sf(x),
        df = .bbox_from_df(x = x, default_crs = default_crs),
        point = .bbox_from_point(x)
    )
    # Convert to sf and get bbox
    geom <- .bbox_as_sf(bbox = x, as_crs = as_crs)
    bbox <- .bbox_from_sf(geom)
    # Update crs
    if (.has(as_crs)) {
        .crs(bbox) <- as_crs
    }
    # Return bbox
    bbox
}

#' @describeIn bbox_api Extract a \code{bbox} from any given \code{sf}.
#' @returns \code{.bbox_from_sf()}: \code{bbox}.
#' @noRd
.bbox_from_sf <- function(x) {
    bbox <- tibble::as_tibble_row(c(sf::st_bbox(x)))
    bbox <- bbox[.bbox_cols]
    .crs(bbox) <- sf::st_crs(x)[["wkt"]]
    # Return bbox
    bbox
}

#' @describeIn bbox_api Extract a \code{bbox} from any given \code{tibble}.
#' @returns \code{.bbox_from_df()}: \code{bbox}.
#' @noRd
.bbox_from_df <- function(x, default_crs = NULL) {
    xmin <- .xmin(x)
    xmax <- .xmax(x)
    ymin <- .ymin(x)
    ymax <- .ymax(x)
    if ("crs" %in% names(x)) {
        crs <- .crs(x)
    } else {
        crs <- .default(default_crs, default = {
            warning("object has no crs, assuming 'EPSG:4326'", call. = FALSE)
            "EPSG:4326"
        })
    }
    # Create a bbox
    bbox <- .common_size(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, crs = crs
    )
    # Fix inconsistencies
    xmin <- pmin(.xmin(bbox), .xmax(bbox))
    xmax <- pmax(.xmin(bbox), .xmax(bbox))
    ymin <- pmin(.ymin(bbox), .ymax(bbox))
    ymax <- pmax(.ymin(bbox), .ymax(bbox))
    # Compute final bbox
    .xmin(bbox) <- xmin
    .xmax(bbox) <- xmax
    .ymin(bbox) <- ymin
    .ymax(bbox) <- ymax
    # Return bbox
    bbox
}

#' @describeIn bbox_api Extract a \code{bbox} from any given \code{point}.
#' @returns \code{.bbox_from_point()}: \code{bbox}.
#' @noRd
.bbox_from_point <- function(point) {
    # Create bbox
    bbox <- .common_size(
        xmin = min(.lon(point)), xmax = max(.lon(point)),
        ymin = min(.lat(point)), ymax = max(.lat(point)),
        crs = .crs(point)
    )
    # Return bbox
    bbox
}

#' @describeIn bbox_api Convert a \code{bbox} into a \code{sf} polygon object.
#' @returns \code{.bbox_as_sf()}: \code{sf}.
#' @noRd
.bbox_as_sf <- function(bbox, as_crs = NULL) {
    # Check for a valid bbox
    .check_bbox(bbox)
    # Check if there are multiple CRS in bbox
    if (length(.crs(bbox)) > 1 && is.null(as_crs)) {
        warning("object has multiples CRS values, reprojecting to ",
                "'EPSG:4326'\n", "(use 'as_crs' to reproject to a ",
                "different CRS)", call. = FALSE)
        as_crs <- "EPSG:4326"
    }
    # Convert to sf object and return it
    geom <- purrr::pmap_dfr(bbox, function(xmin, xmax, ymin, ymax, crs, ...) {
        sf::st_sf(
            geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(xmin, ymax), c(xmax, ymax), c(xmax, ymin),
                      c(xmin, ymin), c(xmin, ymax))
            ))), crs = crs
        )
    })
    # Project CRS
    if (!is.null(as_crs)) {
        geom <- sf::st_transform(geom, crs = as_crs)
    }
    # Return geom
    geom
}

.bbox_intersection <- function(x, y) {
    # Check for a valid bbox
    .check_bbox(x)
    .check_bbox(y)
    # Transform y projection according with x
    as_crs <- .crs(x)
    y <- .bbox_as_sf(bbox = y, as_crs = as_crs)
    x <- .bbox_as_sf(bbox = x)
    # Do intersection
    if (!.intersects(x, y)) {
        return(NULL)
    }
    geom <- sf::st_intersection(x, y)
    bbox <- .bbox(geom)
    # Return bbox
    bbox
}
