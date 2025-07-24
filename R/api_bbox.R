#' @title Check if bboxs are equal
#' @name .bbox_equal
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param bbox1          Bounding box for a region of interest.
#' @param bbox2          Bounding box for a region of interest.
#' @param tolerance      Tolerance (numerical value)
#' @return               A logical value
#'
.bbox_equal <- function(bbox1, bbox2, tolerance = 0.0) {
    .is_eq(unlist(bbox1[.bbox_cols]), unlist(bbox2[.bbox_cols]),
        tolerance = tolerance
    )
}
#' @title Bounding box API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' A bounding box represents a rectangular geographical region in a certain
#' projection. A \code{bbox} is any \code{list} or \code{tibble} containing
#' \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}, and \code{crs} fields.
#' A \code{bbox} may contains multiple entries.
#'
#' @param x      Any object to extract a \code{bbox}.
#' @param ...    Parameters to be evaluated accordingly to input object.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- list(a = 0, z = 0)
#'     .bbox(x) # NULL
#'     x <- list(
#'         a = 0, xmin = 1:3, xmax = 2:4, ymin = 3:5, ymax = 4:6,
#'         crs = 4326, z = 0
#'     )
#'     .bbox(x)
#'     .bbox_as_sf(x) # 3 features
#'     .bbox_as_sf(x, as_crs = "EPSG:3857")
#' }
NULL
# bbox fields
.bbox_cols <- c("xmin", "xmax", "ymin", "ymax")
#' @title Check if an object contains a bbox
#' @noRd
#' @returns A logical indicating if an object contains a bbox.
.has_bbox <- function(x) {
    all(.bbox_cols %in% names(x))
}
#' @title Check if an object is a bbox
#' @noRd
#' @returns A logical indicating if an object is a bbox.
.is_bbox <- function(x) {
    setequal(names(x), c(.bbox_cols, "crs"))
}
#' @title Get the type of object containing a bbox
#' @noRd
#' @returns A bbox type (One of 'sf', 'tbl', or 'point').
.bbox_type <- function(x) {
    if (inherits(x, c("sf", "sfc"))) {
        "sf"
    } else if (.has_bbox(x)) {
        "tbl"
    } else if (.is_point(x)) {
        "point"
    } else {
        stop(.conf("messages", ".bbox_type"), class(x))
    }
}
#' @title Switch bbox type
#' @noRd
#' @returns One of the arguments passed in `...` according to a bbox type.
.bbox_switch <- function(x, ...) {
    switch(.bbox_type(x),
        ...
    )
}
#' @title Extract a bbox
#' @noRd
#' @param default_crs  If no CRS is present in `x`, which CRS should be
#'   used? If `NULL`, default CRS will be 'EPSG:4326'.
#' @param as_crs  A CRS to project bbox. Useful if bbox has multiples CRS.
#' @returns A bbox from any given object.
.bbox <- function(x, default_crs = NULL, as_crs = NULL, by_feature = FALSE) {
    x <- .bbox_switch(
        x = x,
        sf = .bbox_from_sf(x),
        tbl = .bbox_from_tbl(x = x, default_crs = default_crs),
        point = .bbox_from_point(x)
    )
    # Convert to sf and get bbox
    geom <- .bbox_as_sf(bbox = x, as_crs = as_crs)
    bbox <- .bbox_from_sf(geom, by_feature = by_feature)
    # Update crs
    if (.has(as_crs)) {
        .crs(bbox) <- as_crs
    }
    # Return bbox
    bbox
}
#' @title Extract a bbox from a sf object
#' @noRd
#' @returns A \code{bbox} from any given \code{sf}.
.bbox_from_sf <- function(x, by_feature = FALSE) {
    bbox <- if (by_feature) {
        slider::slide_dfr(x, function(y) {
            tibble::as_tibble_row(c(sf::st_bbox(y)))
        })
    } else {
        tibble::as_tibble_row(c(sf::st_bbox(x)))
    }
    bbox <- bbox[.bbox_cols]
    .crs(bbox) <- sf::st_crs(x)[["wkt"]]
    # Return bbox
    bbox
}
#' @title Extract a bbox from a tibble object
#' @noRd
#' @param default_crs  If no CRS is present in `x`, which CRS should be
#'   used? If `NULL`, default CRS will be 'EPSG:4326'.
#' @returns a \code{bbox} from any given \code{tibble}.
.bbox_from_tbl <- function(x, default_crs = NULL) {
    xmin <- .xmin(x)
    xmax <- .xmax(x)
    ymin <- .ymin(x)
    ymax <- .ymax(x)
    if ("crs" %in% names(x)) {
        crs <- .crs(x)
    } else {
        crs <- .default(default_crs, default = {
            if (.message_warnings()) {
                msg <- .conf("messages", ".bbox_from_tbl")
                warning(msg, call. = FALSE)
            }
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
#' @title Extract a bbox from a set of points
#' @noRd
#' @returns A bbox from any given set of points.
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
#' @title Convert a bbox into a sf object
#' @noRd
#' @param bbox    A bbox.
#' @param as_crs  A CRS to project bbox. Useful if bbox has multiples CRS.
#' @returns A sf polygon object from a bbox.
.bbox_as_sf <- function(bbox, as_crs = NULL) {
    # Check for a valid bbox
    .check_bbox(bbox)
    # Check if there are multiple CRS in bbox
    if (length(.crs(bbox)) > 1L && .has_not(as_crs)) {
        .message_warnings_bbox_as_sf()
        as_crs <- "EPSG:4326"
    }
    # Convert to sf object and return it
    geom <- purrr::pmap_dfr(bbox, function(xmin, xmax, ymin, ymax, crs, ...) {
        geom_elem <- sf::st_sf(
            geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(
                    c(xmin, ymax), c(xmax, ymax), c(xmax, ymin),
                    c(xmin, ymin), c(xmin, ymax)
                )
            ))), crs = crs
        )
        # Project CRS
        if (!is.null(as_crs)) {
            geom_elem <- sf::st_transform(geom_elem, crs = as_crs)
        }
        # Return geometry
        geom_elem
    })
    # Return geom
    sf::st_make_valid(geom)
}
#' @title Compute the intersection of two bbox
#' @noRd
#' @param x,y  A bbox.
#' @returns  An intersected bbox.
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
#' @title Convert WKT projection name no PROJ4 name
#' @name .crs_wkt_to_proj4
#' @noRd
#' @param wkt_crs  CRS in WKT name
#' @returns  CRS in PROJ4 name
.crs_wkt_to_proj4 <- function(wkt_crs) {
    # Convert WKT to sf CRS object
    crs_sf <- sf::st_crs(wkt_crs)
    # Convert sf CRS object to PROJ4 string
    proj4string <- crs_sf[["proj4string"]]
    proj4string
}

#' @title Check if CRS is WGS84
#' @name .is_crs_wgs84
#' @noRd
#' @param crs character or numeric crs
#' @returns  a logical
.is_crs_wgs84 <- function(crs) {
    crs <- sf::st_crs(crs)
    crs == sf::st_crs("EPSG:4326")
}
