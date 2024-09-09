#' @title Convert a ROI defined as sf object to a geojson polygon geometry
#' @name .roi_sf_to_geojson
#' @keywords internal
#' @noRd
#' @param  roi   region of interest as sf object
#' @return a geojson polygon geometry
.roi_sf_to_geojson <- function(roi) {
    # set caller to show in errors
    .check_set_caller(".roi_sf_to_geojson")
    # verifies if geojsonsf and jsonlite packages are installed
    .check_require_packages(c("geojsonsf", "jsonlite"))
    # pre-conditions
    .check_that(nrow(roi) == 1)
    # reproject roi to WGS84
    roi <- .roi_as_sf(roi, as_crs = "WGS84")
    # convert roi_sf to geojson
    geojson <- sf::st_geometry(sf::st_convex_hull(roi))
    geojson <- geojsonsf::sfc_geojson(geojson)
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
#'     x <- list(lon_min = 1, lon_max = 2, lat_min = 3, lat_max = 4)
#'     .roi_type(x) # lonlat
#'     .roi_as_sf(x)
#'     x <- list(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 3857)
#'     .roi_type(x) # bbox
#'     .roi_as_sf(x)
#'     .roi_switch(
#'         x,
#'         lonlat = "It's in WGS 84",
#'         bbox = paste("It's in", .crs(x))
#'     )
#' }
#'
#' @family region objects API
#' @keywords internal
#' @name roi_api
#' @noRd
NULL

# roi 'lonlat' fields
.roi_lonlat_cols <- c("lon_min", "lon_max", "lat_min", "lat_max")

# roi 'xs' fields
.roi_xs_cols <- c("xmin", "xmax", "ymin", "ymax")

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
    } else if (all(.roi_xs_cols %in% names(roi))) {
        "xs"
    } else if (inherits(roi, "SpatExtent")) {
        "xs"
    } else {
        stop(.conf("messages", ".roi_type"))
    }
}

#' @describeIn roi_api Chooses one of the arguments passed in
#'   \code{...} according to which type of \code{roi} parameter.
#' @returns \code{.roi_switch()}: one of the arguments in \code{...}.
#' @noRd
.roi_switch <- function(roi, ...) {
    switch(.roi_type(roi),
        ...
    )
}

#' @describeIn roi_api Converts \code{roi} to an \code{sf} object.
#' @returns \code{.roi_as_sf()}: \code{sf}.
#' @noRd
.roi_as_sf <- function(roi, default_crs = NULL, as_crs = NULL) {
    # is the roi defined by a shapefile
    if (is.character(roi) &&
        file.exists(roi) &&
        (tools::file_ext(roi) == "shp"))
        roi <- sf::st_read(roi)
    # `xs` requires the definition of a CRS
    if (.roi_type(roi) == "xs" || .roi_type(roi) == "bbox") {
        # transform roi to list
        roi <- as.list(roi)
        # check the default CRS
        .check_that(.has(default_crs))
    }
    # convert R objects to sf object
    roi <- .roi_switch(
        roi = roi,
        sf = roi,
        bbox = .bbox_as_sf(.bbox(roi, default_crs = default_crs)),
        lonlat = .bbox_as_sf(list(
            xmin = roi[["lon_min"]], xmax = roi[["lon_max"]],
            ymin = roi[["lat_min"]], ymax = roi[["lat_max"]],
            crs = "EPSG:4326"
        )),
        xs = .bbox_as_sf(list(
            xmin = roi[["xmin"]], xmax = roi[["xmax"]],
            ymin = roi[["ymin"]], ymax = roi[["ymax"]],
            crs = default_crs
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

.roi_write <- function(roi, output_file, quiet, ...) {
    sf::st_write(obj = roi, dsn = output_file, quiet = quiet, ...)
    output_file
}

.roi_delete <- function(output_file) {
    dir <- .file_dir(output_file)
    file_name <- .file_sans_ext(output_file)
    shp_exts <- c(".shp", ".shx", ".dbf", ".prj")
    unlink(paste0(dir, file_name, shp_exts))
}
