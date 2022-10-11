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
#' @param data      Valid sits tibble (time series or a cube).
#' @param wgs84     Reproject bbox to WGS84 (EPSG:4326)?
#' @param ...       Additional parameters (not implemented).
#'
#' @return          Bounding box in WGS84 for time series or
#'                  on the cube projection for a data cube
#'                  unless wgs84 parameter is TRUE.
#'
#' @examples
#' bbox <- sits_bbox(samples_modis_ndvi)
#'
#' @export
#'
sits_bbox <- function(data, wgs84 = FALSE, ...) {
    .check_set_caller("sits_bbox")

    # Get the meta-type (sits or cube)
    data <- .conf_data_meta_type(data)

    UseMethod("sits_bbox", data)
}

#' @rdname sits_bbox
#' @export
#'
sits_bbox.sits <- function(data, ...) {

    # Get the max and min longitudes and latitudes
    lon_max <- max(data$longitude)
    lon_min <- min(data$longitude)
    lat_max <- max(data$latitude)
    lat_min <- min(data$latitude)
    # create and return the bounding box
    bbox <- c(lon_min, lat_min, lon_max, lat_max)
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
    return(bbox)
}
#' @rdname sits_bbox
#' @export
#'
sits_bbox.sits_cube <- function(data, wgs84 = FALSE, ...) {

    # Pre-condition
    .check_is_sits_cube(data)

    if (!wgs84 && length(unique(data[["crs"]])) > 1) {
        warning("cube has more than one projection - using wgs84 coords")
        wgs84 <- TRUE
    }
    if (wgs84) {
        bbox_dfr <- slider::slide_dfr(data, function(tile) {
            # create and return the bounding box

            bbox <- .sits_coords_to_bbox_wgs84(
                xmin = tile[["xmin"]],
                ymin = tile[["ymin"]],
                xmax = tile[["xmax"]],
                ymax = tile[["ymax"]],
                crs  = tile[["crs"]][[1]]
            )
            tibble::as_tibble_row(c(bbox))
        })
    } else {
        bbox_dfr <- data[c("xmin", "ymin", "xmax", "ymax")]
    }
    bbox <- c(
        "xmin" = min(bbox_dfr[["xmin"]]),
        "ymin" = min(bbox_dfr[["ymin"]]),
        "xmax" = max(bbox_dfr[["xmax"]]),
        "ymax" = max(bbox_dfr[["ymax"]])
    )

    return(bbox)
}
#' @title Convert coordinates to bounding box
#' @name .bbox_wgs84
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param xmin           Minimum X coordinate
#' @param ymin           Minimum Y coordinate
#' @param xmax           Maximum X coordinate
#' @param ymax           Maximum Y coordinate
#' @param crs            Projection for X,Y coordinates
#' @return               Coordinates in WGS84.
.bbox_wgs84 <- function(xmin, xmax, ymin, ymax, crs) {
    pt1 <- c(xmin, ymax)
    pt2 <- c(xmax, ymax)
    pt3 <- c(xmax, ymin)
    pt4 <- c(xmin, ymin)

    # detect if crs is an EPSG code
    epsg <- suppressWarnings(as.numeric(crs))
    if (!is.na(epsg)) {
        crs <- epsg
    }

    bbox <- sf::st_sfc(
        sf::st_polygon(list(rbind(pt1, pt2, pt3, pt4, pt1))),
        crs = crs
    )

    # Create a polygon and transform the proj
    bbox_latlng <- c(sf::st_bbox(sf::st_transform(bbox, crs = 4326)))

    return(bbox_latlng)
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

#' @title Convert a bounding box to a sf object (polygon)
#' @name .sits_bbox_to_sf
#' @keywords internal
#' @noRd
#' @param xmin,xmax,ymin,ymax  Bounding box values.
#' @param crs                  Valid crs value.
#' @return                     An sf object.
#'
.sits_bbox_to_sf <- function(xmin, xmax, ymin, ymax, crs) {
    pt1 <- c(xmin, ymax)
    pt2 <- c(xmax, ymax)
    pt3 <- c(xmax, ymin)
    pt4 <- c(xmin, ymin)

    sf_obj <- sf::st_sf(geometry = sf::st_sfc(
        sf::st_polygon(list(rbind(pt1, pt2, pt3, pt4, pt1)))
    ), crs = crs)

    return(sf_obj)
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

#' @describeIn bbox_api extract a \code{bbox} from any given
#' \code{vector}.
#' @noRd
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
