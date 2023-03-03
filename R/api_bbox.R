#' @title Check if bboxs are equal
#' @name .bbox_equal
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param bbox1          Bounding box for a region of interest.
#' @param bbox2          Bounding box for a region of interest.
#' @param tolerance      Tolerance (numerical value)
#' @return               A logical value
#'
.bbox_equal <- function(bbox1, bbox2, tolerance = 0){
    .is_eq(unlist(bbox1[.bbox_cols]), unlist(bbox2[.bbox_cols]),
           tolerance = tolerance)
}
#' @title Bounding box API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
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
#' @title Check if an object is a bbox
#' @noRd
#' @returns Throws an error if an object is not a bbox.
.check_bbox <- function(x) {
    if (!.is_bbox(x)) {
        stop("object is not a valid bbox")
    }
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
        stop("cannot extract bbox from object of class ", class(x))
    }
}
#' @title Switch bbox type
#' @noRd
#' @returns One of the arguments passed in `...` according to a bbox type.
.bbox_switch <- function(x, ...) {
    switch(.bbox_type(x), ...)
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

#' @title Bounding box accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These functions are accessors of bbox fields of a vector.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the vector is not atomic, it is removed from the object.
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
NULL

.xmin <- function(x) {
    .as_dbl(.compact(x[["xmin"]]))
}
`.xmin<-` <- function(x, value) {
    x[["xmin"]] <- .as_dbl(value)
    x
}
.xmax <- function(x) {
    .as_dbl(.compact(x[["xmax"]]))
}
`.xmax<-` <- function(x, value) {
    x[["xmax"]] <- .as_dbl(value)
    x
}
.ymin <- function(x) {
    .as_dbl(.compact(x[["ymin"]]))
}
`.ymin<-` <- function(x, value) {
    x[["ymin"]] <- .as_dbl(value)
    x
}
.ymax <- function(x) {
    .as_dbl(.compact(x[["ymax"]]))
}
`.ymax<-` <- function(x, value) {
    x[["ymax"]] <- .as_dbl(value)
    x
}
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
.crs <- function(x) {
    .as_crs(x[["crs"]])
}
`.crs<-` <- function(x, value) {
    x[["crs"]] <- .as_crs(value)
    x
}
