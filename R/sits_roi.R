#' @title Find the bounding box for a spatial ROI in a data cube
#' @name .roi_bbox
#' @keywords internal
#' @noRd
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
.roi_bbox <- function(roi, cube) {

    # set caller to show in errors
    .check_set_caller(".roi_bbox")

    if (!(inherits(roi, "sf"))) {
        if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(roi))) {
            class(roi) <- c("xy", class(roi))
        } else if (all(
            c("lon_min", "lon_max", "lat_min", "lat_max") %in% names(roi)
        )) {
            class(roi) <- c("ll", class(roi))
        }
    }

    .check_that(
        x = inherits(roi, c("sf", "xy", "ll")),
        msg = "invalid definition of ROI"
    )

    UseMethod(".roi_bbox", roi)
}

#' @title Find the bounding box for a spatial ROI defined as an sf object
#' @name .roi_bbox.sf
#' @keywords internal
#' @noRd
#' @param  roi             spatial region of interest
#' @param  cube            input data cube.
#' @return                 vector with information on the subimage
#' @export
.roi_bbox.sf <- function(roi, cube) {
    bbox <- roi %>%
        sf::st_transform(crs = .cube_crs(cube)) %>%
        suppressWarnings() %>%
        sf::st_bbox()

    return(bbox)
}
#' @title Find the bounding box for a spatial ROI defined as a bounding box
#' @name .roi_bbox.xy
#' @keywords internal
#' @noRd
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
#' @export
.roi_bbox.xy <- function(roi, cube) {
    return(roi)
}
#' @title Find the bounding box for a spatial ROI defined as a lat/lon box
#' @name .roi_bbox.ll
#' @keywords internal
#' @noRd
#' @param  cube            input data cube.
#' @param  roi             spatial region of interest
#' @return                 vector with information on the subimage
#' @export
.roi_bbox.ll <- function(roi, cube) {
    # region of interest defined by two points
    df <- data.frame(
        lon = c(roi["lon_min"], roi["lon_max"], roi["lon_max"], roi["lon_min"]),
        lat = c(roi["lat_min"], roi["lat_min"], roi["lat_max"], roi["lat_max"])
    )

    sf_region <- df %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        dplyr::summarise(geometry = sf::st_combine(.data[["geometry"]])) %>%
        sf::st_cast("POLYGON")

    bbox <- sf::st_bbox(
        suppressWarnings(
            sf::st_transform(sf_region, crs = .cube_crs(cube))
        )
    )
}
#' @title Convert a ROI defined as sf object to a geojson polygon geometry
#' @name .roi_sf_to_geojson
#' @keywords internal
#' @noRd
#' @param  roi_sf   region of interest as sf object
#' @return a geojson polygon geometry
.roi_sf_to_geojson <- function(roi_sf) {

    # pre-conditions
    .check_that(nrow(roi_sf) == 1,
        local_msg = "roi_sf should have only one row",
        msg = "invalid roi_sf value"
    )
    # verifies if geojsonsf and jsonlite packages are installed
    .check_require_packages(c("geojsonsf", "jsonlite"))

    # convert roi_sf to geojson
    geojson <- roi_sf %>%
        sf::st_convex_hull() %>%
        sf::st_geometry() %>%
        geojsonsf::sfc_geojson()
    geojson <- jsonlite::fromJSON(geojson)

    return(geojson)
}
#  ROI API
#
#  A Region of Interest (ROI) represents an geographic area. There are
#  three types of ROI objects, namely \code{sf} (from package \code{sf}),
#  \code{bbox} (from \code{.bbox()}), and \code{lonlat}.
#  A \code{lonlat} object is any \code{vector} containing \code{lon_min},
#  \code{lon_max}, \code{lat_min}, and \code{lat_max} fields. Its CRS is
#  defined to be \code{'EPSG:4326'}.
#
#' @param roi A \code{roi}.
#' @param ... Parameters to be evaluated accordingly to \code{roi} type.
#' @param default_crs If no CRS is present in a \code{bbox} object passed
#'   to \code{crs}, which CRS should be used? If \code{NULL} default CRS will
#'   be \code{'EPSG:4326'}.
#' @param as_crs CRS to project \code{sf} object.
#'
#' @examples
#' if (sits_run_examples()) {
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
#' @noRd
NULL

# roi 'lonlat' fields
.roi_lonlat_cols <- c("lon_min", "lon_max", "lat_min", "lat_max")

#' @describeIn roi_api Tells which type of ROI is in \code{roi}
#'   parameter (One of \code{'sf'}, \code{'bbox'}, or \code{'lonlat'}).
#' @returns \code{.roi_type()}: \code{character}.
#' @noRd
.roi_type <- function(roi) {
    if (inherits(roi, c("sf", "sfc"))) {
        "sf"
    } else if (.has_bbox(roi)) {
        "bbox"
    } else if (all(.roi_lonlat_cols %in% names(roi))) {
        "lonlat"
    } else {
        stop("invalid 'roi' parameter")
    }
}

#' @describeIn roi_api Chooses one of the arguments passed in
#'   \code{...} according to which type of \code{roi} parameter.
#' @returns \code{.roi_switch()}: one of the arguments in \code{...}.
#' @noRd
.roi_switch <- function(roi, ...) {
    switch(.roi_type(roi), ...)
}

#' @describeIn roi_api Converts \code{roi} to an \code{sf} object.
#' @returns \code{.roi_as_sf()}: \code{sf}.
#' @noRd
.roi_as_sf <- function(roi, default_crs = NULL, as_crs = NULL) {
    roi <- .roi_switch(
        roi = roi,
        sf = roi,
        bbox = .bbox_as_sf(.bbox(roi, default_crs = default_crs)),
        lonlat = .bbox_as_sf(list(
            xmin = roi[["lon_min"]], xmax = roi[["lon_max"]],
            ymin = roi[["lat_min"]], ymax = roi[["lat_max"]],
            crs = "EPSG:4326"
        ))
    )
    # Project roi
    if (.has(as_crs)) {
        roi <- sf::st_transform(roi, crs = as_crs)
    }
    # Transform feature to multipolygons
    roi <- if (.has(nrow(roi)) && nrow(roi) > 1) sf::st_union(roi) else roi
    # Return roi
    roi
}
