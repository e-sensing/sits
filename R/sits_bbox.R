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
#' @noRd
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

#' @describeIn bbox_api extract a \code{bbox} from any given
#' \code{vector}.
#' @noRd
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
#' @noRd
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
